{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

-- | Types and functions for reading inputs files (Cabal, source files, etc)
module FrisbyGlobalTagsCabalGhcIn (
  CabalPlan(..),
  HashMD5(..),
  Id(..),
  PackageJsonLocalEntry(..),
  encodeBase64HashMD5,
  md5File,
  ) where

import           Control.Applicative ( (<|>) )
import           Control.Monad ( guard )
import qualified Crypto.Hash.MD5                                       as MD5
import qualified Data.Aeson                                          as Aeson
import qualified Data.Aeson.Types                                    as Aeson
import qualified Data.ByteString                                        as BS
import qualified Data.ByteString.Base64                                 as BS
import           Data.Foldable ( toList )
import           Data.Functor ( (<&>) )
import qualified Data.Map                                              as Map
import           Data.Maybe ( mapMaybe )
import           Data.Text ( Text )
import qualified Data.Text                                            as Text
import qualified Data.Text.Lazy                                   as LazyText
import qualified Data.Text.Lazy.Encoding                          as LazyText
import qualified Data.Text.Lazy.IO                                as LazyText

import qualified Distribution.Simple.LocalBuildInfo                  as Cabal
import qualified Distribution.System                                 as Cabal
import qualified Distribution.Types.Flag                             as Cabal
import qualified Distribution.Types.UnqualComponentName              as Cabal

-----

data MaybePackageJsonLocalEntry f = JustPJE (PackageJsonLocalEntry f) | NothingPJE

-- | Summary of one step of the 'CabalPlan'
data PackageJsonLocalEntry f = PJE
  {
    -- | The package name for this step of the build plan
    pjePackageName :: f Text
  ,
    -- | The flags as configured for this .cabal file in the build plan
    pjeFlags :: f Cabal.FlagAssignment
  ,
    -- | The name of this step of the build plan (eg @lib:foo@, @exe:bar@, @test:baz@, etc)
    pjeComponentName :: f Cabal.ComponentName
  ,
    -- | Path to the .cabal file for this component of the build plan
    pjeCabalPath :: f Text
  ,
    -- | The package dependencies of this Cabal target as configured in this build plan
    pjeDepends :: f [Text]
  }

traversePJE ::
     Applicative i
  => (forall a. f a -> i (g a))
  -> PackageJsonLocalEntry f
  -> i (PackageJsonLocalEntry g)
traversePJE f as =
    PJE <$> f a1 <*> f a2 <*> f a3 <*> f a4 <*> f a5
  where
    PJE a1 a2 a3 a4 a5 = as

newtype Id a = Id{unId :: a}

instance Aeson.FromJSON (MaybePackageJsonLocalEntry Id) where
  parseJSON v =
      (<|> pure NothingPJE) $
      ($ v) $ Aeson.withObject "entry" $ \o ->
      fmap JustPJE $ traversePJE (fmap Id) $ PJE
        {
          pjePackageName = o Aeson..: "pkg-name"
        ,
          pjeFlags =
            fmap (\x -> Cabal.mkFlagAssignment [ (Cabal.mkFlagName (Text.unpack fl), b) | (fl, b) <- Map.toList x ]) $
            o Aeson..: "flags"
        ,
          pjeComponentName =
            (o Aeson..: "component-name") <&> \x -> case Text.unpack x of
              "lib" -> Cabal.CLibName Cabal.LMainLibName
              'l':'i':'b'         :':':n -> Cabal.CLibName $ Cabal.LSubLibName $ Cabal.mkUnqualComponentName n
              'e':'x':'e'         :':':n -> Cabal.CExeName                     $ Cabal.mkUnqualComponentName n
              't':'e':'s':'t'     :':':n -> Cabal.CTestName                    $ Cabal.mkUnqualComponentName n
              'b':'e':'n':'c':'h' :':':n -> Cabal.CBenchName                   $ Cabal.mkUnqualComponentName n
              _ -> error $ "bad/TODO component-name: " <> Text.unpack x
        ,
          pjeCabalPath =
            (\f -> Aeson.explicitParseField f o "pkg-src") $ Aeson.withObject "pkg-src" $ \o' -> do
              typ <- o' Aeson..: "type"
              guard $ typ == ("local" :: Text)
              o' Aeson..: "path"
        ,
          pjeDepends =
            (\f -> Aeson.explicitParseField f o "depends") $ Aeson.withArray "depends" $ \arr -> do
              traverse (\e -> Aeson.withText "depend" pure e) (toList arr)
        }

-- | The build plan resulting from an invocation of @cabal configure@
data CabalPlan f = CabalPlan
  {
    cpCompiler :: f String
  ,
    cpOS :: f Cabal.OS
  ,
    cpArch :: f Cabal.Arch
  ,
    cpInstallPlan :: f [PackageJsonLocalEntry Id]
  }

traverseCP :: Applicative i => (forall a. f a -> i (g a)) -> CabalPlan f -> i (CabalPlan g)
traverseCP f as =
    CabalPlan <$> f a1 <*> f a2 <*> f a3 <*> f a4
  where
    CabalPlan a1 a2 a3 a4 = as

instance Aeson.FromJSON (CabalPlan Id) where
  parseJSON =
      Aeson.withObject "plan" $ \o -> do
        traverseCP (fmap Id) $ CabalPlan
          {
            cpCompiler = o Aeson..: "compiler-id" >>= \x -> case Text.unpack x of
              y@('g':'h':'c':_) -> pure y
              _                 -> error $ "bad/TODO compiler: " <> Text.unpack x
          ,
            cpOS = o Aeson..: "os" <&> \x -> case Text.unpack x of
              "linux" -> Cabal.Linux
              _ -> error $ "bad/TODO os: " <> Text.unpack x
          ,
            cpArch = o Aeson..: "arch" <&> \x -> case Text.unpack x of
              "x86_64" -> Cabal.X86_64
              _ -> error $ "bad/TODO arch: " <> Text.unpack x
          ,
            cpInstallPlan =
              fmap
                (mapMaybe (\case NothingPJE -> Nothing; JustPJE x -> Just x)) $
              o Aeson..: "install-plan"
          }

-----

data HashMD5 = MkHashMD5 {unHashMD5 :: BS.ByteString}

encodeBase64HashMD5 :: HashMD5 -> Text
encodeBase64HashMD5 = BS.encodeBase64 . unHashMD5

md5File :: FilePath -> IO HashMD5
md5File path = do
  contents <- LazyText.readFile path
  -- some of these # lines include temporary file names
  --
  --   o The tmp file is system-dependent
  --
  --   o The tmp file includes ghc's process id, which is nondeterministic
  --
  --   o TODO probably shouldn't drop them all!
  let isActualLine ln = not $ LazyText.pack "# " `LazyText.isPrefixOf` ln
      fltr :: LazyText.Text -> LazyText.Text
      fltr = LazyText.concat . filter isActualLine . LazyText.lines
  pure $ MkHashMD5 $ MD5.hashlazy $ LazyText.encodeUtf8 $ fltr contents
