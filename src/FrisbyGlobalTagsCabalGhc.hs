{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FrisbyGlobalTagsCabalGhc ( main ) where

import           Control.Applicative ( (<|>) )
import           Control.Monad ( when )
import           Control.Monad.IO.Class ( MonadIO( liftIO ) )
import           Control.Monad.Trans.Except ( ExceptT(..) , runExceptT )
import qualified Data.Aeson                                          as Aeson
import           Data.Char ( isDigit , isUpper )
import           Data.Foldable ( forM_ , toList )
import           Data.IORef (
  IORef,
  newIORef,
  readIORef,
  writeIORef,
  )
import           Data.List (
  groupBy,
  intercalate,
  isPrefixOf,
  isSuffixOf,
  )
import           Data.List.NonEmpty ( NonEmpty(..) )
import           Data.Map ( Map )
import qualified Data.Map                                              as Map
import           Data.Monoid ( Endo (..) )
import           Data.Set ( Set )
import qualified Data.Set                                              as Set
import           Data.Text ( Text )
import qualified Data.Text                                            as Text
import           Data.Traversable ( forM )
import qualified Database.SQLite.Simple                               as SQL3
import           System.Directory (
  copyFile,
  doesFileExist,
  removeFile,
  renameFile,
  )
import           System.Environment ( getArgs )
import           System.Exit ( die )
import           System.FilePath (
  (</>),
  (<.>),
  makeRelative,
  normalise,
  takeDirectory,
  )
import           System.IO ( hPutStrLn , stderr )
import           System.IO.Temp ( withSystemTempDirectory )
import           Text.Read ( readMaybe )

import qualified Distribution.Compiler                               as Cabal
import qualified Distribution.ModuleName                             as Cabal
import qualified Distribution.PackageDescription.Configuration       as Cabal
import qualified Distribution.PackageDescription.Parsec              as Cabal
import qualified Distribution.Parsec                                 as Cabal
--import qualified Distribution.Pretty                                 as Cabal
--import qualified Distribution.Simple.Build                           as Cabal
--import qualified Distribution.Simple.BuildPaths                      as Cabal
--import qualified Distribution.Simple.Configure                       as Cabal
--import qualified Distribution.Simple.GHC                             as Cabal
import qualified Distribution.Simple.Build.Macros                    as Cabal
import qualified Distribution.Simple.LocalBuildInfo                  as Cabal
import qualified Distribution.Simple.Utils                           as Cabal
import qualified Distribution.Types.Benchmark                        as Cabal
import qualified Distribution.Types.BenchmarkInterface               as Cabal
import qualified Distribution.Types.BuildInfo                        as Cabal
import qualified Distribution.Types.ComponentRequestedSpec           as Cabal
import qualified Distribution.Types.Executable                       as Cabal
import qualified Distribution.Types.Library                          as Cabal
import qualified Distribution.Types.PackageDescription               as Cabal
import qualified Distribution.Types.PackageId                        as Cabal
import qualified Distribution.Types.PackageName                      as Cabal
import qualified Distribution.Types.TestSuite                        as Cabal
import qualified Distribution.Types.TestSuiteInterface               as Cabal
import qualified Distribution.Types.UnqualComponentName              as Cabal
import qualified Distribution.System                                 as Cabal
import qualified Distribution.Verbosity                              as Cabal
import qualified DriverPipeline                                        as GHC
import qualified DynFlags                                              as GHC
import qualified ErrUtils                                              as GHC
import qualified FastString                                            as GHC
import qualified Finder                                                as GHC
import qualified GHC                                                   as GHC
import qualified HscTypes                                              as GHC
import qualified OccName                                               as GHC
--import qualified Outputable                                            as GHC
import qualified RdrName                                               as GHC
import qualified StringBuffer                                          as GHC
import qualified Util                                                  as GHC

import qualified FrisbyGlobalTagsCabalGhcGhc                      as GrephGhc
import qualified FrisbyGlobalTagsCabalGhcIn                        as GrephIn
import qualified FrisbyGlobalTagsCabalGhcOut                      as GrephOut

-----

tunpack :: GrephIn.Id Text -> String
tunpack = Text.unpack . GrephIn.unId

-----

{-
test :: IO ()
test = do
  -- CF https://git.savannah.gnu.org/cgit/emacs.git/tree/etc/ETAGS.EBNF

  let ff    = "\x0c"
      lf    = "\x0a"
      del   = "\x7f"
      soh   = "\x01"
      comma = ","

      tagfile = tagsection1 <> tagsection2

      tagsection1 = tagsection regularsec1
      tagsection2 = tagsection regularsec2

      tagsection sec = ff <> lf <> sec <> lf

      regularsec1 = regularsec filename1 [tag11, tag12, tag13]
      regularsec2 = regularsec filename2 [tag21, tag22]

      regularsec filename tags = filename <> "," <> sized (BS.empty <> BS.concat (map (lf <>) tags))

      filename1 = "ouroboros-consensus/src/Ouroboros/Consensus/Protocol/BFText.hs"
      filename2 = "ouroboros-consensus/src/Ouroboros/Consensus/Node.hs"

      tag11 = directTag   "74" "forgeBftFields"
      tag12 = directTag   "62" "instance,NoThunks,BftFields"
      tag13 = directTag   "84" "forgeBftFields"

      tag21 = directTag  "564" "stdChainSyncTimeout"
      tag22 = directTag  "612" "StdRunNodeArgs"

      sized bs = BS8.pack (show (BS.length bs)) <> bs

      directTag lineno tagname =
          BS.empty <> del <> (tagname <> soh) <> position
        where
          position     = realposition
          realposition = lineno <> comma

  BS.writeFile "../TAGS" tagfile
-}

-----

main :: IO ()
main = do
    [buildDir] <- getArgs    -- eg greph "$(pwd)/dist-newstyle"

    let planJsonPath = buildDir </> "cache" </> "plan.json"
        projectDir   = takeDirectory buildDir

    --------------------
    -- Use Cabal-the-library to compute the 'Cabal.PackageDescription' for each
    -- planned build component.
    --
    -- Note that this includes the "finalize" step, which is necessary for the
    -- concrete result of interpreting a @.cabal@ file, eg the conditionals are
    -- all resolved.
    cp <- Aeson.decodeFileStrict planJsonPath >>= \case
      Nothing -> die "could not parse Cabal's `plan.json' output file; have you ran `cabal [v2-]configure' for the --build-dir you gave?"
      Just x  -> pure x

    let plannedComponents :: Map FilePath (NonEmpty (GrephIn.PackageJsonLocalEntry GrephIn.Id))
        plannedComponents =
            Map.fromListWith (<>) $
            flip foldMap (GrephIn.unId $ GrephIn.cpInstallPlan cp) $ \pje ->
            [ (,)
                (tunpack (GrephIn.pjeCabalPath pje) </> tunpack (GrephIn.pjePackageName pje) <.> ".cabal")
                (pje :| [])
            ]

    pds <-
      fmap (concatMap toList) $
      -- Note: the order here does not matter, since the syntactic tag analysis
      -- doesn't require well-formedness of dependencies.
      forM (Map.toList plannedComponents) $ \((,) cabalPath pjes) -> do
        gpd <- Cabal.readGenericPackageDescription Cabal.verbose cabalPath
        forM pjes $ \pje -> do
          let compName = GrephIn.unId (GrephIn.pjeComponentName pje)
              platform = Cabal.Platform (GrephIn.unId (GrephIn.cpArch cp)) (GrephIn.unId (GrephIn.cpOS cp))
          packageDescription <-
              (\case
                Left{}             -> die $ "could not finalize: " <> cabalPath
                Right (pd, _flags) -> pure pd
              )
            $ Cabal.finalizePD
                (GrephIn.unId $ GrephIn.pjeFlags pje)
                (Cabal.OneComponentRequestedSpec compName)
                (let assumePackageIsSatisfiable = \_buildDep -> True in assumePackageIsSatisfiable)
                platform
                (Cabal.unknownCompilerInfo Cabal.buildCompilerId Cabal.NoAbiTag)
                (let additionalConstraints = [] in additionalConstraints)
                gpd
          packageDependencyIds <- do
            let ePkgIds = traverse parseDepenencyPkgIdentifier $ GrephIn.unId $ GrephIn.pjeDepends pje
            case ePkgIds of
              Left txt     -> die $ "could not parse pkgid: " <> txt
              Right pkgids -> pure pkgids

{-          lbi <- Cabal.getConfigStateFile $ traceShowId $
              buildDir
            </>
              "build"
            </>
              show (Cabal.pretty platform)
            </>
              unId (cpCompiler cp)
            </>
              (let pkgname = show (Cabal.pretty (Cabal.package pd))
               in case compName of
                 Cabal.CLibName{}   -> pkgname
                 Cabal.CFLibName{}  -> pkgname
                 Cabal.CExeName{}   -> pkgname </> "x"
                 Cabal.CTestName{}  -> pkgname </> "t"
                 Cabal.CBenchName{} -> pkgname </> "b"
              )
            </>
              "setup-config"
          pure (cabalPath, compName, lbi{Cabal.localPkgDescr = pd})
-}
          pure (PackageEnv{..}, compName)

--    let _ = lbis :: [(,,) PkgDescrFilePath Cabal.ComponentName Cabal.LocalBuildInfo]
    let _ = pds :: [(,) PackageEnv Cabal.ComponentName]

    --------------------
    -- Prepare the output files

    let fresh path = do
            doesFileExist path >>= flip when (removeFile path)
            SQL3.open path

    -- This tool is intentionally not calculating the use-def mapping, so it
    -- simply always writes an empty db to @GRTAGS@.
    do
      conn <- fresh (projectDir </> "GRTAGS")
      SQL3.execute_ conn "CREATE TABLE IF NOT EXISTS db (key text, dat text, extra text)"
      SQL3.execute  conn "INSERT INTO db (key, dat, extra) VALUES (?,?,?)" (GrephOut.Raw " __.VERSION" " __.VERSION 6" "")
      SQL3.execute  conn "INSERT INTO db (key, dat, extra) VALUES (?,?,?)" (GrephOut.Raw " __.COMPNAME" " __.COMPNAME" "")
      SQL3.close conn

    -- This tool writes to @.working@ files and then writes them over the old
    -- @GPATH@ and @GTAGS@ files only when it successfully terminates.
    --
    -- If those files already exist when the tool is invoked, it's first step
    -- is to copy them into the @.working@ files.
    doingUpdate <- doesFileExist (projectDir </> "GPATH")
    (,) connTAGS connPATH <- if doingUpdate
      then do
        copyFile (projectDir </> "GTAGS") (projectDir </> "GTAGS.working")
        copyFile (projectDir </> "GPATH") (projectDir </> "GPATH.working")
        (,) <$> SQL3.open (projectDir </> "GTAGS.working") <*> SQL3.open (projectDir </> "GPATH.working")
      else do
        connTAGS <- fresh (projectDir </> "GTAGS.working")
        SQL3.execute_ connTAGS "CREATE TABLE IF NOT EXISTS db (key text, dat text, extra text)"
        SQL3.execute  connTAGS "INSERT INTO db (key, dat, extra) VALUES (?,?,?)" (GrephOut.Raw " __.VERSION" " __.VERSION 6" "")
        SQL3.execute  connTAGS "INSERT INTO db (key, dat, extra) VALUES (?,?,?)" (GrephOut.Raw " __.COMPNAME" " __.COMPNAME" "")

        connPATH <- fresh (projectDir </> "GPATH.working")
        SQL3.execute_ connPATH "CREATE TABLE IF NOT EXISTS db (key text, dat text, extra text)"
        SQL3.execute  connPATH "INSERT INTO db (key, dat, extra) VALUES (?,?,?)" (GrephOut.Raw " __.VERSION" " __.VERSION 2" "")
        SQL3.execute  connPATH "INSERT INTO db (key, dat, extra) VALUES (?,?,?)" (GrephOut.Raw " __.NEXTKEY" "1" "")

        pure $ (,) connTAGS connPATH

    --------------------
    -- Compute the static environment for this analysis invocation, which
    -- includes the initial state

    oldPaths <- do
      rows <- SQL3.query_ connPATH "SELECT key FROM db"
      pure
        $ Set.fromList
        $ map ModulePath
        $ filter (not . (" __." `isPrefixOf`))
        $ filter (not . all isDigit)
        $ map SQL3.fromOnly rows

    [SQL3.Only sFirstFileNo] <- SQL3.query connPATH "SELECT dat FROM db WHERE key = ?" (SQL3.Only (" __.NEXTKEY" :: String))
    baseFileNo <- case readMaybe (sFirstFileNo :: String) of
      Nothing -> die " __.NEXTKEY must be an integer"
      Just x  -> pure (x :: Int)

    visitedRef <- newIORef Set.empty

    let senv = StaticEnv{..}

    --------------------
    -- Do the analysis

    mapM_ (uncurry $ analyzeComponentName senv) pds

    visitedPaths <- readIORef visitedRef

    let nextFileNo = Set.size visitedPaths + baseFileNo
    SQL3.execute connPATH "DELETE FROM db WHERE key = ?" (SQL3.Only (" __.NEXTKEY" :: String))
    SQL3.execute connPATH "INSERT INTO db (key, dat, extra) VALUES (?,?,?)" (GrephOut.Raw " __.NEXTKEY" (show nextFileNo) "")

    -- Delete the paths from the pre-existing @GPATH@ file that were not been
    -- visited during this analysis.
    --
    -- TODO add flag to disable this, so the tool can be ran on one piece of
    -- the codebase at a time
    mapM_ (deleteOldPath senv) (oldPaths `Set.difference` visitedPaths)

    SQL3.close connTAGS
    SQL3.close connPATH

    renameFile (projectDir </> "GTAGS.working") (projectDir </> "GTAGS")
    renameFile (projectDir </> "GPATH.working") (projectDir </> "GPATH")

-- | Delete this path from @GPATH.working@ and any tags from @GTAGS.working@
-- that are now dangling
deleteOldPath :: StaticEnv -> ModulePath -> IO ()
deleteOldPath senv modulePath = do
    let StaticEnv{..} = senv
        srcFilePath   = unModulePath modulePath
    rows <- SQL3.query connPATH "SELECT dat FROM db WHERE key = ?" (SQL3.Only srcFilePath)
    oldFileNo <- case rows of
      [] -> error "impossible!"
      [SQL3.Only sOldFileNo] ->
          case readMaybe (sOldFileNo :: String) of
            Nothing -> die $ "GPATH.dat must an integer if .key is a filepath " <> srcFilePath
            Just x  -> pure (x :: Int)
      o  -> die $ "ambiguous GPATH " <> srcFilePath <> " " <> show (length o)

    hPutStrLn stderr $ "Removed   file " <> srcFilePath

    SQL3.execute connPATH "DELETE FROM db WHERE dat = ? OR key = ?" ((,) oldFileNo oldFileNo)
    SQL3.execute connTAGS "DELETE FROM db WHERE extra = ?" (SQL3.Only oldFileNo)

-----

-- | The file number used to refer to @GPATH@ from @GTAGS@
newtype FileNo = FileNo Int

-- | The name of a module
newtype ModuleNameString = ModuleNameString {unModuleNameString :: String}

-- | The path to a module's source file
newtype ModulePath = ModulePath {unModulePath :: FilePath}
  deriving (Eq, Ord)

-- | Parse a dependency string as a Cabal package identifier
--
-- TODO this parser makes a lot of simplifying assumptions; why exactly are
-- they justified?
parseDepenencyPkgIdentifier :: Text -> Either String Cabal.PackageIdentifier
parseDepenencyPkgIdentifier txt =
      maybe (Left (Text.unpack txt)) Right
    $     (Cabal.simpleParsec $ Text.unpack txt)
      <|> (Cabal.simpleParsec $ drophash $ Text.unpack txt)
      <|> (Cabal.simpleParsec $ dropinplace $ Text.unpack txt)
  where
    drophash    = concat . init . split
    dropinplace = concat . takeWhile (/= "-inplace") . split
          
    split s = glue $ groupBy (\a b -> ('-' == a) == ('-' == b)) s

    glue :: [String] -> [String]
    glue = \case
      []         -> []
      ("-":s:ss) -> ('-':s) : glue ss
      s:ss       ->      s  : glue ss

-----

-- | This environment doesn't change during the program execution
data StaticEnv = StaticEnv
  {
    -- | The next 'FileNo'
    --
    -- This is @1@ if the tool is starting from scratch, otherwise it's the
    -- value of the @ __.NEXTKEY@ in the incoming @GPATH@ file.
    --
    -- TODO use Word64 to be ertain this can't overflow except in
    -- extraordinarily pathological cases
    baseFileNo :: Int
  ,
    -- | The files this analysis has visited
    visitedRef :: IORef (Set ModulePath)
  ,
    -- | Handle to @GTAGS.working@
    connTAGS :: SQL3.Connection
  ,
    -- | Handle to @GPATH.working@
    connPATH :: SQL3.Connection
  ,
    -- | The root directory of the project being analyzed
    --
    -- The @gtags@ files are created here, and the paths in @GPATH@ are
    -- relative to this one.
    projectDir :: FilePath
  }

-- | This environment varies as the tool processes different packages in the
-- 'GrephIn.CabalPlan'
data PackageEnv = PackageEnv
  {
    -- | Path to the @.cabal@ file
    cabalPath :: FilePath
  ,
    -- | A "finalized" package description
    --
    -- The finalization step maps the contents a @.cabal@ file to a concrete
    -- package description. The finalization is a lossy function, and in
    -- particular may have been focused on a specific component within the
    -- @.cabal@ file.
    packageDescription :: Cabal.PackageDescription
  ,
    -- | The dependencies listed for this package in the 'GrephIn.CabalPlan'
    packageDependencyIds :: [Cabal.PackageIdentifier]
  }

-- | This environment varies as the tool processes different components in the
-- 'PackageEnv'
data ComponentEnv = ComponentEnv
  {
    -- | The 'BuildInfo' for this component, from 'packageDescription'
    buildInfo :: Cabal.BuildInfo
  ,
    -- | The path (relative to one of the 'sourceDirPaths') to the module
    -- source file that contains the executable component's @main@ function
    mainIs :: Maybe FilePath
  ,
    -- | Module names of the planned component
    moduleNames :: [Cabal.ModuleName]
  ,
    -- | Options from the @.cabal@ file necessary for parsing (notably language
    -- extensions)
    ghcParserOptions :: GHC.DynFlags -> GHC.DynFlags
  ,
    -- | The paths to the source directories of the planned component
    sourceDirPaths :: [FilePath]
  }

-- | This environment varies as the tool processes different modules in the 'ComponentEnv'
data ModuleEnv = ModuleEnv
  {
    -- | Path to the module's source file, relative to 'projectDir'
    moduleFilePath :: FilePath
  ,
    -- | The module summary after GHC has preprocessed the file (eg including
    -- CPP)
    moduleSummary :: GHC.ModSummary
  ,
    -- | The hash of the preprocessed file
    moduleMD5 :: GrephIn.HashMD5
  ,
    -- | The unique file number a la 'baseFileNo' of this module
    moduleFileNo :: Int
  }

-----

-- | Looks up the component in the @.cabal@ file, finds its GHC options
-- (notably language extensions) listed in the @.cabal@ file, prepares the
-- component's CPP header file of @*VERSION*@ CPP macros, and delegates to
-- 'analyzeComponent' for the components' modules
--
-- It's a noop for foreign libraries.
analyzeComponentName :: StaticEnv -> PackageEnv -> Cabal.ComponentName -> IO ()
analyzeComponentName senv packageEnv componentName = do
    let PackageEnv{..} = packageEnv

-- TODO I gave up on generating eg the Paths_ module; the tool just skips
-- "Path_*" modules for now. But I hope this snippet will be helpful for the
-- next attempt.
{-
        pd        = Cabal.localPkgDescr lbi
        clbi      = case Map.lookup componentName (Cabal.componentNameMap lbi) of
          Just [x] -> x
          _  -> error "impossible! component name not a singleton in LBI"

    Cabal.writeAutogenFiles Cabal.verbose pd lbi clbi
    let autogenDir = Cabal.autogenComponentModulesDir lbi clbi
-}

    let filename = "greph-min-versions.h"
        dirnameTemplate =
             Cabal.unPackageName (Cabal.pkgName $ Cabal.package packageDescription)
          <> maybe "" (\u -> "-" <> Cabal.unUnqualComponentName u) (Cabal.componentNameString componentName)
          <> "-greph.include"

    let k buildInfo moduleNames mainIs = do
          withSystemTempDirectory dirnameTemplate $ \dirPath -> do
            writeFile (dirPath </> filename) $ Cabal.generatePackageVersionMacros packageDependencyIds

            let ghcParserOptions =
                    {- (\dflags -> dflags {GHC.verbosity = 7}) . -}
                    (\dflags -> dflags { GHC.includePaths =
                                           GHC.addGlobalInclude
                                             (GHC.includePaths dflags)
                                             [dirPath]
                                       }
                    )
                  . GrephGhc.addOptP ("-include" <> filename)
                  . ( \nil ->
                          foldl (\acc ext -> GrephGhc.cnvExtension ext acc) nil
                        $ Cabal.defaultExtensions buildInfo
                    )
                  . GrephGhc.cnvLanguage (Cabal.defaultLanguage buildInfo)

            let sourceDirPaths :: [FilePath]
                sourceDirPaths =
                    map (takeDirectory cabalPath </>)
                  $ Cabal.hsSourceDirs buildInfo

            analyzeComponent senv ComponentEnv{..}

    case Cabal.getComponent packageDescription componentName of
        Cabal.CLib lib ->
            k
              (Cabal.libBuildInfo lib)
              (Cabal.explicitLibModules lib)
              Nothing
        Cabal.CFLib{} -> pure ()
        Cabal.CExe exe ->
            k
              (Cabal.buildInfo exe)
              (Cabal.exeModules exe)
              (Just $ Cabal.modulePath exe)
        Cabal.CTest testsuite ->
            k
              (Cabal.testBuildInfo testsuite)
              ( Cabal.testModules testsuite <>
                case Cabal.testInterface testsuite of
                  Cabal.TestSuiteExeV10{}          -> []
                  Cabal.TestSuiteLibV09 _version m -> [m]
                  Cabal.TestSuiteUnsupported{}     -> []
              )
              ( case Cabal.testInterface testsuite of
                  Cabal.TestSuiteExeV10 _version path -> Just path
                  Cabal.TestSuiteLibV09{}             -> Nothing
                  Cabal.TestSuiteUnsupported{}        -> Nothing
              )
        Cabal.CBench benchmark ->
            k
              (Cabal.benchmarkBuildInfo benchmark)
              (Cabal.benchmarkModules benchmark)
              ( case Cabal.benchmarkInterface benchmark of
                  Cabal.BenchmarkExeV10 _version path -> Just path
                  Cabal.BenchmarkUnsupported{}        -> Nothing
              )

-- | Calculates the component's modules' source file paths and delegates to
-- 'analyzeModulePaths'
analyzeComponent :: StaticEnv -> ComponentEnv -> IO ()
analyzeComponent senv componentEnv = do
    let moduleNames1 = flip filter moduleNames $ \mn -> case Cabal.components mn of
          [s] -> not $ "Paths_" `isPrefixOf` s   -- TODO how to generate these files instead of skip them?
          _   -> True
    modulePaths1 <-
        fmap (map (\(m, (p1, p2)) -> (ModuleNameString m, ModulePath (p1 </> p2))))
      $ fmap (zip (map (intercalate "." . Cabal.components) moduleNames1))
      $ Cabal.findModuleFilesEx Cabal.silent sourceDirPaths (words "hs lhs hsc") moduleNames1
    let _ = modulePaths1 :: [(,) ModuleNameString ModulePath]

    mbMainIs2 <- case mainIs of
      Nothing   -> pure Nothing
      Just path -> do
        Cabal.findFirstFile id [ dir </> path | dir <- sourceDirPaths ] >>= \case
          Nothing -> die $ "Could not find: " <> path
          Just x  -> pure (Just x)
    let _ = mbMainIs2 :: Maybe FilePath

    let modulePaths4 :: [(,) ModuleNameString ModulePath]
        modulePaths4 =
            [ (,)
                m
                ( ModulePath
                $ ("." </>)
                $ normalise
                $ makeRelative projectDir p
                )
            | (m, ModulePath p) <- modulePaths3
            ]
          where
            modulePaths2 = 
                ( case mbMainIs2 of
                    Nothing -> id
                    Just p  -> (:) (ModuleNameString "Main", ModulePath p)
                )
              $ modulePaths1

            modulePaths3 =
                filter (("hs" `isSuffixOf`) . unModulePath . snd)
              $ modulePaths2   -- TODO how to support hsc?

    analyzeModulePaths senv componentEnv modulePaths4

  where
    StaticEnv{..}    = senv
    ComponentEnv{..} = componentEnv

-- | Runs GHC to parse the component's Haskell module source files and
-- delegates to 'visitModule'
--
-- TODO more efficient to re-use one GHC session and alter the options between
-- components?
analyzeModulePaths ::
     StaticEnv
  -> ComponentEnv
  -> [(ModuleNameString, ModulePath)]
  -> IO ()
analyzeModulePaths senv componentEnv modulePaths = GrephGhc.runGhc $ do
    let StaticEnv{..}    = senv
        ComponentEnv{..} = componentEnv
    dflags <- GHC.getSessionDynFlags

--     liftIO $ putStrLn $ GHC.showPpr dflags $ [ext | ext <- [minBound..maxBound], GHC.xopt ext (applyGhcParserOptions ghcParserOptions dflags) ]

    forM_ modulePaths $ \(moduleNameString, modulePath) -> dontVisitTwice (FileNo baseFileNo) visitedRef modulePath $ \(FileNo moduleFileNo) -> do
      let m              = unModuleNameString moduleNameString
          moduleFilePath = unModulePath modulePath

      _ <- GHC.setSessionDynFlags
        $ (\x->x{GHC.hscTarget = GHC.HscNothing})   -- parsing has no result
        $ ghcParserOptions dflags

      let m' = GHC.mkModuleName m
      hsc_env <- GHC.getSession

      mbEiModSum <- liftIO $ runExceptT $
        mkDegenerateModSummary hsc_env m' modulePath

      case mbEiModSum of
        Left{} -> error $ "could not summarize: " <> m
        Right ((,) moduleMD5 moduleSummary) -> do

          visitModule senv componentEnv ModuleEnv{..}

-- | The tool only visits a file once, even if multiple @.cabal@ components
-- compile it
--
-- TODO isn't that an incompleteness when the .cabal components set eg
-- different CPP flags?
dontVisitTwice ::
     MonadIO m
  => FileNo
  -> IORef (Set ModulePath)
  -> ModulePath
  -> (FileNo -> m ())
  -> m ()
dontVisitTwice (FileNo baseFileNo) visitedRef modulePath k = do
    visited <- liftIO $ readIORef visitedRef
    when (modulePath `Set.notMember` visited) $ do
      liftIO $ writeIORef visitedRef $! Set.insert modulePath visited
      k $ FileNo $ baseFileNo + Set.size visited

-- | Simplifies targeting, loading, etc, since we are merely parsing
--
-- The hash is computed /after/ GHC preprocesses the file.
mkDegenerateModSummary ::
     GHC.HscEnv
  -> GHC.ModuleName
  -> ModulePath
  -> ExceptT GHC.ErrorMessages IO ((,) GrephIn.HashMD5 GHC.ModSummary)
mkDegenerateModSummary hsc_env mod_name modulePath = do
    let src_fn = unModulePath modulePath
    src_timestamp <- liftIO $ GHC.getModificationUTCTime src_fn
    location <- liftIO $
      GHC.mkHomeModLocation (GHC.hsc_dflags hsc_env) mod_name src_fn

    -- 'GHC.preprocess' handles pragmas, CPP, etc
    (local_dflags, hspp_fn) <- ExceptT $
      GHC.preprocess
        hsc_env
        src_fn
        Nothing   -- no buffer yet
        Nothing   -- let GHC determine the phase

    hashMD5  <- liftIO $ GrephIn.md5File hspp_fn   -- TODO should include the pjeDepends too
    hspp_buf <- liftIO $ GHC.hGetStringBuffer hspp_fn

    let m = GHC.mkModule (GHC.thisPackage (GHC.hsc_dflags hsc_env)) mod_name

    return $ (,) hashMD5 $ GHC.ModSummary
      { GHC.ms_hie_date     = Nothing   -- need not avoid recompliation
      , GHC.ms_hs_date      = src_timestamp
      , GHC.ms_hsc_src      = GHC.HsSrcFile
      , GHC.ms_hspp_buf     = Just hspp_buf
      , GHC.ms_hspp_file    = hspp_fn
      , GHC.ms_hspp_opts    = local_dflags
      , GHC.ms_iface_date   = Nothing   -- need not avoid recompliation
      , GHC.ms_location     = location
      , GHC.ms_mod          = m
      , GHC.ms_obj_date     = Nothing   -- need not avoid recompliation
      , GHC.ms_parsed_mod   = Nothing
      , GHC.ms_srcimps      = []   -- parsing doesn't need import info
      , GHC.ms_textual_imps = []   -- parsing doesn't need import info
      }

-----

-- | Determines if the file needs to be processed and delegates to
-- 'analyzeParsedModule'
visitModule :: StaticEnv -> ComponentEnv -> ModuleEnv -> GHC.Ghc ()
visitModule senv _componentEnv moduleEnv = do
    let StaticEnv{..} = senv
        ModuleEnv{..} = moduleEnv

    -- skip the file, if it hasn't changed
    fileStatus <- liftIO $ do
      let thash = GrephIn.encodeBase64HashMD5 moduleMD5
      rows <- SQL3.query connPATH "SELECT dat, extra FROM db WHERE key = ?" (SQL3.Only moduleFilePath)
      case rows of
        [(,) sOldFileNo oldHash] ->
            if thash == oldHash then pure FileUnchanged else do
              oldFileNo <- case readMaybe (sOldFileNo :: String) of
                Nothing -> die $ "GPATH.dat must an integer if .key is a filepath " <> moduleFilePath
                Just x  -> pure (x :: Int)
              pure $ FileChanged oldFileNo
        [] -> pure FileNew
        o  -> die $ "ambiguous GPATH " <> moduleFilePath <> " " <> show (length o)

    let k = do
            let pe = GrephOut.PathsEntry
                  {
                    GrephOut.fileNo' = moduleFileNo
                  ,
                    GrephOut.filepath = moduleFilePath
                  ,
                    GrephOut.fileMD5 = moduleMD5
                  }

            liftIO $ SQL3.execute connPATH "INSERT INTO db (key, dat, extra) VALUES (?,?,?)" pe
            liftIO $ SQL3.execute connPATH "INSERT INTO db (key, dat, extra) VALUES (?,?,?)" $ GrephOut.Invert pe

            GHC.parseModule moduleSummary >>= liftIO . analyzeParsedModule senv moduleEnv

    case fileStatus of
      FileUnchanged -> do
          liftIO $ hPutStrLn stderr $ "Unchanged file " <> moduleFilePath

      FileNew -> do
          liftIO $ hPutStrLn stderr $ "New file       " <> moduleFilePath
          k

      FileChanged oldFileNo -> do
          liftIO $ hPutStrLn stderr $ "Changed file   " <> moduleFilePath

          liftIO $ SQL3.execute connPATH "DELETE FROM db WHERE dat = ? OR key = ?" ((,) oldFileNo oldFileNo)
          liftIO $ SQL3.execute connTAGS "DELETE FROM db WHERE extra = ?" (SQL3.Only oldFileNo)

          k

data FileStatus = FileNew | FileChanged Int | FileUnchanged

data Singleton a = Zero | One a | Many
  deriving (Functor)

instance Monoid (Singleton a) where mempty = Zero

instance Semigroup (Singleton a) where
  Zero <> x = x
  x <> Zero = x
  _ <> _ = Many

data Step = Ignore | Set (Maybe String)

-- | If effectively-adjacent declarations have the same single tag, suppress
-- all but the first
--
-- TODO also support @a, b :: X; a = foo; b = bar@?
--
-- TODO also support @data T = ... | T ...@? That's one declaration with multiple tags, some of which match.
suppressAdjacentValDs :: [GHC.HsDecl GHC.GhcPs] -> [GHC.HsDecl GHC.GhcPs]
suppressAdjacentValDs =
    go Nothing
  where
    go acc = \case
        [] -> []
        d : ds ->
            (case get d of Set (Just tag) | acc == Just tag -> id; _ -> (:) d)
          $ go (case get d of Set acc' -> acc'; Ignore -> acc) ds

    get d = case d of
      GHC.ForD{} -> setJust d
      GHC.KindSigD{} -> setJust d
      GHC.SigD{} -> setJust d
      GHC.TyClD{} -> setJust d
      GHC.ValD{} -> setJust d

      -- these cannot be between the matching decls
      GHC.DefD{} -> Set Nothing
      GHC.DerivD{} -> Set Nothing
      GHC.InstD{} -> Set Nothing

      -- these can be between the matching decls
      GHC.RoleAnnotD{} -> Ignore   -- TODO could include these, but we don't generate any tags for them
      GHC.AnnD{} -> Ignore
      GHC.DocD{} -> Ignore
      GHC.SpliceD{} -> Ignore
      GHC.RuleD{} -> Ignore
      GHC.WarningD{} -> Ignore

      GHC.XHsDecl x -> GHC.noExtCon x

    setJust d = Set $ case foldDeclTags (\Tag{..} -> One tagName) d of
        Zero -> Nothing
        One a -> Just a
        Many -> Nothing

-- | Analyzes the parsed AST
analyzeParsedModule :: StaticEnv -> ModuleEnv -> GHC.ParsedModule -> IO ()
analyzeParsedModule senv moduleEnv parsedModule = do
    let hsMod = GHC.unLoc $ GHC.parsedSource parsedModule

    forM_ (GHC.hsmodName hsMod) $ \nm -> case GHC.getLoc nm of
        GHC.UnhelpfulSpan{} -> mempty
        GHC.RealSrcSpan rss -> runIOU $ f Tag
          {
            lineNo = GHC.srcSpanStartLine rss
          ,
            tagImage = "module"
          ,
            tagName = GHC.moduleNameString (GHC.unLoc nm)
          }

    -- TODO if there's an export list, suppress things that aren't exported?
    mapM_
      (runIOU . foldDeclTags f)
      (suppressAdjacentValDs $ GHC.unLoc <$> GHC.hsmodDecls hsMod)
  where
    StaticEnv{..} = senv
    ModuleEnv{..} = moduleEnv

    f Tag{..} = IOU $ do
        when False $ hPutStrLn stderr $ tagName <> " " <> moduleFilePath <> " " <> show lineNo
        GrephOut.insertTAGS connTAGS GrephOut.TagsEntry
          {
            GrephOut.fileNo = moduleFileNo
          ,
            GrephOut.lineNo = lineNo
          ,
            GrephOut.tagImage = tagImage
          ,
            GrephOut.tagName = tagName
          }

newtype IOU = IOU {runIOU :: IO ()}

instance Monoid IOU where mempty = IOU (pure ())

instance Semigroup IOU where IOU l <> IOU r = IOU (do l; r)  

-----

data Tag = Tag {
    -- | Which line in the file?
    lineNo :: Int
  ,
    -- | A slight elaboration of the tag, shown to help the user disambiguate
    -- it from tags with the same name
    tagImage :: String
  ,
    -- | The key for a declaration, matched against the user's query
    tagName :: String
  }

-- | Analyzes the parsed AST
foldDeclTags :: forall m. Monoid m => (Tag -> m) -> GHC.HsDecl GHC.GhcPs -> m
foldDeclTags f = \case
      GHC.DerivD   _x dec -> goDeriv dec
      GHC.ForD     _x dec -> goForeign dec
      GHC.InstD    _x dec -> goInstDecl dec
      GHC.KindSigD _x dec -> goKindSig dec
      GHC.SigD     _x dec -> goSig dec
      GHC.TyClD    _x dec -> goTyClDecl dec
      GHC.ValD     _x dec -> goVal dec

      -- these do not bind names
      GHC.AnnD{} -> mempty
      GHC.DefD{} -> mempty
      GHC.DocD{} -> mempty
      GHC.SpliceD{} -> mempty
      GHC.RoleAnnotD{} -> mempty
      GHC.RuleD{} -> mempty
      GHC.WarningD{} -> mempty

      GHC.XHsDecl x -> GHC.noExtCon x
  where
{-
    sho :: GHC.Outputable a => a -> String
    sho = GHC.showSDoc dflags . GHC.ppr
-}

    goInstDecl = \case
      GHC.ClsInstD     _xx dec -> goClsInstDecl     dec
      GHC.DataFamInstD _xx dec -> goDataFamInstDecl dec
      GHC.TyFamInstD   _xx dec -> goTyFamInstDecl   dec
      GHC.XInstDecl x          -> GHC.noExtCon x

    goClsInstDecl dec =
        let dec' :: GHC.Located (GHC.HsType GHC.GhcPs)
            dec' = GHC.hsib_body $ GHC.cid_poly_ty dec
        in

{-
            (_, y) = GHC.splitLHsQualTy dec'
            f :| as = splitLhsAppTys y

        putStrLn $ sho $ GHC.getLoc dec'
        putStr $ indent $ sho f
        foldMap (putStr . indent . indent . sho) as
-}

           -- the instance head
           goHead dec'

           -- included associated type instances
        <> foldMap (goTyFamInstDecl   . GHC.unLoc) (GHC.cid_tyfam_insts   dec)
        <> foldMap (goDataFamInstDecl . GHC.unLoc) (GHC.cid_datafam_insts dec)

    goDataFamInstDecl = \case
        GHC.DataFamInstDecl
          {
            GHC.dfid_eqn = GHC.HsIB
              {
                GHC.hsib_body = GHC.FamEqn
                  {
                    GHC.feqn_tycon = fam
                  ,
                    GHC.feqn_pats = pats
                  ,
                    GHC.feqn_rhs = rhs
                  }
              }
          } -> 
            let self = Endo (ident fam :) <> foldMap tyargs pats
                lhs' = goHead_ fam self
                rhs' = goDataDefn self rhs
            in
            lhs' <> rhs'
        GHC.DataFamInstDecl (GHC.HsIB _x (GHC.XFamEqn x)) -> GHC.noExtCon x
        GHC.DataFamInstDecl (GHC.XHsImplicitBndrs x)      -> GHC.noExtCon x

    goTyFamInstDecl = \case
        GHC.TyFamInstDecl
          {
            GHC.tfid_eqn = GHC.HsIB
              {
                GHC.hsib_body = GHC.FamEqn
                  {
                    GHC.feqn_tycon = fam
                  ,
                    GHC.feqn_pats = pats
                  }
              }
          } ->
            -- the LHS
            goHead_ fam $ Endo (ident fam :) <> foldMap tyargs pats
        GHC.TyFamInstDecl (GHC.HsIB _x (GHC.XFamEqn x)) -> GHC.noExtCon x
        GHC.TyFamInstDecl (GHC.XHsImplicitBndrs x)      -> GHC.noExtCon x

    goDataDefn self = \case
       GHC.HsDataDefn{ GHC.dd_cons = ctors , GHC.dd_derivs = derivs } ->
            ( flip foldMap ctors $ \lcondecl -> case GHC.unLoc lcondecl of
                GHC.ConDeclGADT { GHC.con_names = lids , GHC.con_args = args } ->
                       foldMap (goBinder "datacon") lids
                    <> goDataConArgs args
                GHC.ConDeclH98 { GHC.con_name = lid , GHC.con_args = args } ->
                       goBinder "datacon" lid
                    <> goDataConArgs args
                GHC.XConDecl x -> GHC.noExtCon x
            )
            <> foldMap (goDerivClause self) (GHC.unLoc derivs)
       GHC.XHsDataDefn x -> GHC.noExtCon x

    getNod = \case
       GHC.HsDataDefn{ GHC.dd_ND = nod } -> nod
       GHC.XHsDataDefn x -> GHC.noExtCon x

    goDataConArgs = goConArgs GHC.unLoc $ (. GHC.unLoc) $ \case
        GHC.ConDeclField{ GHC.cd_fld_names = lsels } -> flip foldMap lsels $ \lsel -> case GHC.unLoc lsel of
            GHC.FieldOcc{ GHC.rdrNameFieldOcc = lid } -> goBinder "selector" lid
            GHC.XFieldOcc x -> GHC.noExtCon x
        GHC.XConDeclField x -> GHC.noExtCon x

    -- the record selectors
    goConArgs :: (fields -> [field]) -> (field -> m) -> GHC.HsConDetails x fields -> m
    goConArgs un g = \case
        GHC.PrefixCon{}   -> mempty
        GHC.InfixCon{}    -> mempty
        GHC.RecCon fields -> foldMap g (un fields)

    goTyClDecl = \case
        GHC.ClassDecl{ GHC.tcdLName = lid , GHC.tcdSigs = methods , GHC.tcdATs = ats } ->
               goBinder "class" lid
            <> foldMap (goSig . GHC.unLoc) methods
            <> foldMap goAT ats
        GHC.DataDecl{ GHC.tcdLName = lid , GHC.tcdDataDefn = dataDefn } ->
            let prefix = case getNod dataDefn of
                    GHC.NewType  -> "newtype"
                    GHC.DataType -> "data"
            in
               goDataDefn (Endo (ident lid:)) dataDefn
            <> goBinder prefix lid
        GHC.SynDecl{ GHC.tcdLName = lid } -> goBinder "type" lid
        GHC.FamDecl
          {
            GHC.tcdFam = GHC.FamilyDecl
              {
                GHC.fdInfo = info
              ,
                GHC.fdLName = lid
              }
          } -> goBinder prefix lid
          where
            prefix = case info of
                GHC.DataFamily -> "data fam"
                GHC.OpenTypeFamily -> "type fam"
                GHC.ClosedTypeFamily{} -> "type fam"

        GHC.FamDecl _x (GHC.XFamilyDecl x) -> GHC.noExtCon x
        GHC.XTyClDecl x                    -> GHC.noExtCon x
          
    goSig :: GHC.Sig GHC.GhcPs -> m
    goSig = \case
        GHC.ClassOpSig _x False lids _ty -> foldMap (goBinder "method signature") lids
        GHC.PatSynSig  _x       lids _ty -> foldMap (goBinder "pattern signature") lids
        GHC.TypeSig    _x       lids _ty -> foldMap (goBinder "signature") lids

        -- ignore @default@ signatures
        GHC.ClassOpSig _x True _lids _ty -> mempty

        -- only arise from generated code
        GHC.IdSig{} -> mempty

        -- these do not bind names
        GHC.FixSig{} -> mempty
        GHC.InlineSig{} -> mempty
        GHC.SpecSig{} -> mempty
        GHC.SpecInstSig{} -> mempty
        GHC.MinimalSig{} -> mempty
        GHC.SCCFunSig{} -> mempty
        GHC.CompleteMatchSig{} -> mempty

        GHC.XSig x -> GHC.noExtCon x

    goKindSig :: GHC.StandaloneKindSig GHC.GhcPs -> m
    goKindSig = \case
        GHC.StandaloneKindSig _x lid _kind -> goBinder "signature" lid
        GHC.XStandaloneKindSig x           -> GHC.noExtCon x

    goForeign :: GHC.ForeignDecl GHC.GhcPs -> m
    goForeign = \case
        GHC.ForeignImport{ GHC.fd_name = lid } -> goBinder "fimport" lid
        GHC.ForeignExport{ GHC.fd_name = lid } -> goBinder "fexport" lid
        GHC.XForeignDecl x                     -> GHC.noExtCon x

    goAT :: GHC.LFamilyDecl GHC.GhcPs -> m
    goAT = (. GHC.unLoc) $ \case
        GHC.FamilyDecl { GHC.fdInfo = info , GHC.fdLName = lid } ->
            goBinder prefix lid
          where
            prefix = case info of
                GHC.DataFamily -> "assoc data"
                GHC.OpenTypeFamily -> "assoc type"
                GHC.ClosedTypeFamily{} -> "assoc type"
        GHC.XFamilyDecl x -> GHC.noExtCon x

    goVal :: GHC.HsBind GHC.GhcPs -> m
    goVal = \case
        GHC.AbsBinds{} -> mempty   -- only arise from renaming
        GHC.FunBind{ GHC.fun_id = lid } -> goBinder "value" lid   -- TODO suppress these, in favor of signatures?
        GHC.PatBind{ GHC.pat_lhs = pat } -> goPat pat
        GHC.PatSynBind _x GHC.PSB{ GHC.psb_id = lid , GHC.psb_args = args } ->
               goBinder "pattern" lid
               -- record selectors defined by the pattern
            <> ( flip (goConArgs id) args $ \case
                   GHC.RecordPatSynField{ GHC.recordPatSynSelectorId = lid' } ->
                     goBinder "selector" lid'
               )
        GHC.PatSynBind _x (GHC.XPatSynBind x) -> GHC.noExtCon x
        GHC.VarBind{} -> mempty   -- only arise from typechecking
        GHC.XHsBindsLR x -> GHC.noExtCon x

    -- TODO suppress these, in favor of signatures?
    goPat :: GHC.LPat GHC.GhcPs -> m
    goPat = (. GHC.unLoc) $ \case
         GHC.AsPat _x lid p -> goBinder "value" lid <> goPat p
         GHC.BangPat _x p -> goPat p
         GHC.ConPatIn _dconlid args -> case args of
             GHC.InfixCon l r -> goPat l <> goPat r
             GHC.PrefixCon ps -> foldMap goPat ps
             GHC.RecCon GHC.HsRecFields{ GHC.rec_flds = flds } ->
                 foldMap (goPat . GHC.hsRecFieldArg . GHC.unLoc) flds
         GHC.LazyPat _x p -> goPat p
         GHC.ListPat _x p -> foldMap goPat p
         GHC.NPlusKPat _x lid _ _ _ _ -> goBinder "value" lid
         GHC.ParPat _x p -> goPat p
         GHC.SigPat _x p _ty -> goPat p
         GHC.SumPat _x p _tag _arity -> goPat p
         GHC.TuplePat _x ps _boxity -> foldMap goPat ps
         GHC.VarPat _x lid -> goBinder "value" lid
         GHC.ViewPat _x _expr p -> goPat p

         -- these don't bind source names
         GHC.CoPat{} -> mempty
         GHC.LitPat{} -> mempty
         GHC.NPat{} -> mempty
         GHC.SplicePat{} -> mempty
         GHC.WildPat{} -> mempty

         -- only arise from renaming
         GHC.ConPatOut{} -> mempty

         GHC.XPat x -> GHC.noExtCon x

    goDeriv :: GHC.DerivDecl GHC.GhcPs -> m
    goDeriv = \case
        GHC.DerivDecl{ GHC.deriv_type = ty } -> case ty of
            GHC.HsWC{ GHC.hswc_body = lsig } -> case lsig of
                GHC.HsIB { GHC.hsib_body = lty } -> goHead lty
                GHC.XHsImplicitBndrs x -> GHC.noExtCon x
            GHC.XHsWildCardBndrs x -> GHC.noExtCon x
        GHC.XDerivDecl x -> GHC.noExtCon x

    goDerivClause :: Endo [String] -> GHC.LHsDerivingClause GHC.GhcPs -> m
    goDerivClause lhs = (. GHC.unLoc) $ \case
        GHC.HsDerivingClause{ GHC.deriv_clause_tys = tys } ->
            flip foldMap (GHC.unLoc tys) $ \case
              GHC.HsIB{ GHC.hsib_body = ty } -> goHead_ ty (tycons ty <> lhs)
              GHC.XHsImplicitBndrs x -> GHC.noExtCon x
        GHC.XHsDerivingClause x -> GHC.noExtCon x

{-
    indent1 s = "    " <> s
    indent    = unlines . map indent1 . lines
-}

    goHead :: GHC.LHsType GHC.GhcPs -> m
    goHead dec = goHead_ dec (tycons dec)

    goHead_ :: GHC.Located x -> Endo [String] -> m
    goHead_ dec tycons' = case GHC.getLoc dec of
        GHC.UnhelpfulSpan{} -> mempty
        GHC.RealSrcSpan rss ->
            let tagname = intercalate "," $ (`appEndo` []) $ Endo ("instance" :) <> tycons'
                lineno = GHC.srcSpanStartLine rss
            in
            f Tag
              {
                lineNo = lineno
              ,
                tagImage = ""
              ,
                tagName = tagname
              }

    goBinder :: String -> GHC.Located (GHC.IdP GHC.GhcPs) -> m
    goBinder prefix lid = case GHC.getLoc lid of
        GHC.UnhelpfulSpan{} -> mempty
        GHC.RealSrcSpan rss -> f Tag
          {
            lineNo = GHC.srcSpanStartLine rss
          ,
            tagImage = prefix
          ,
            tagName = ident lid
          }

{-
splitLhsAppTys :: GHC.LHsType GHC.GhcPs -> NonEmpty (GHC.LHsType GHC.GhcPs)
splitLhsAppTys =
    go []
  where
    go acc lty = case GHC.unLoc lty of
        GHC.HsAppKindTy _x f a -> go (a : acc) f
        GHC.HsAppTy     _x f a -> go (a : acc) f
        _                      -> lty :| acc
-}

rdr :: GHC.RdrName -> String
rdr = GHC.occNameString . GHC.rdrNameOcc

ident :: GHC.Located (GHC.IdP GHC.GhcPs) -> String
ident = rdr . GHC.unLoc

tyargs :: GHC.LHsTypeArg GHC.GhcPs -> Endo [String]
tyargs = \case
    GHC.HsValArg ty -> tycons ty

    -- these are not visible in the source code
    GHC.HsArgPar{} -> mempty
    GHC.HsTypeArg{} -> mempty

tycons :: GHC.LHsType GHC.GhcPs -> Endo [String]
tycons = (. GHC.unLoc) $ \case
    GHC.HsTyVar _x _ticked lid ->
        if isConstructor s then Endo (ident lid :) else mempty
      where
        s = ident lid
    GHC.HsForAllTy{GHC.hst_body = t} -> tycons t
    GHC.HsQualTy{GHC.hst_body = t} -> tycons t
    GHC.HsAppTy _x f a -> tycons f <> tycons a
    GHC.HsAppKindTy _x f a -> tycons f <> tycons a
    GHC.HsFunTy _x d c -> Endo ("Fun" :) <> tycons d <> tycons c
    GHC.HsListTy _x a -> Endo ("List" :) <> tycons a
    GHC.HsTupleTy _x _sort as -> Endo (("Tuple" <> show (length as)) :) <> foldMap tycons as
    GHC.HsSumTy _x as -> Endo (("Sum" <> show (length as)) :) <> foldMap tycons as
    GHC.HsOpTy _x l o r -> Endo (ident o :) <> tycons l <> tycons r
    GHC.HsParTy _x ty -> tycons ty
    GHC.HsIParamTy{} -> mempty
    GHC.HsStarTy _x _unicode -> Endo ("Type" :)
    GHC.HsKindSig _x ty k -> tycons ty <> tycons k
    GHC.HsSpliceTy{} -> mempty   -- TODO
    GHC.HsDocTy{} -> mempty
    GHC.HsBangTy{} -> mempty
    GHC.HsRecTy{} -> mempty
    GHC.HsExplicitListTy _x _ticked as -> Endo (("LiteralList" <> show (length as)) :) <> foldMap tycons as
    GHC.HsExplicitTupleTy _x as -> Endo (("LiteralTuple" <> show (length as)) :) <> foldMap tycons as
    GHC.HsTyLit _x lit -> case lit of
      GHC.HsNumTy _srctxt int -> Endo (show int :)
      GHC.HsStrTy _srctxt fs  -> Endo (GHC.unpackFS fs :)
    GHC.HsWildCardTy{} -> mempty
    GHC.XHsType GHC.NHsCoreTy{} -> mempty   -- TODO
  where

isConstructor :: String -> Bool
isConstructor = \case
    "->"  -> True   -- TODO more of these special cases? eg tuple names etc?
    c : _ -> isUpper c || ':' == c
    _     -> False
