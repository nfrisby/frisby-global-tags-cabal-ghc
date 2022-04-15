{-# LANGUAGE OverloadedStrings #-}

-- | Types and functions for writing output files (the three @gtags@ files)
module FrisbyGlobalTagsCabalGhcOut (
  Invert(..),
  PathsEntry(..),
  Raw(..),
  TagsEntry(..),
  insertTAGS,
  ) where

import qualified Database.SQLite.Simple                               as SQL3
import qualified FrisbyGlobalTagsCabalGhcIn                        as GrephIn

-----

data TagsEntry = TagsEntry
  {
    -- | Which file, according to the @GPATH@ db?
    fileNo :: Int
  ,
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

instance SQL3.ToRow TagsEntry where
  toRow te = SQL3.toRow
    ( tagName te
    , show (fileNo te) <> " @n " <> show (lineNo te) <> " " <> tagImage te
    , fileNo te
    )

insertTAGS :: SQL3.Connection -> TagsEntry -> IO ()
insertTAGS conn =
    SQL3.execute conn "INSERT INTO db (key, dat, extra) VALUES (?,?,?)"

data PathsEntry = PathsEntry
  {
    fileNo' :: Int
  ,
    -- | Path to the file, relative to the invocation of this program
    filepath :: String
  ,
    -- | A fingerprint of the content of the file when this program analyzed it
    fileMD5 :: GrephIn.HashMD5
  }

instance SQL3.ToRow PathsEntry where
  toRow pe = SQL3.toRow
    ( filepath pe
    , fileNo' pe
    , GrephIn.encodeBase64HashMD5 $ fileMD5 pe
    )

-- | The @GPATHS@ db stores both the number-to-path mapping and the
-- path-to-number mapping
newtype Invert = Invert PathsEntry

instance SQL3.ToRow Invert where
  toRow (Invert pe) = SQL3.toRow
    ( fileNo' pe
    , filepath pe
    , "" :: String
    )

-- | Just a raw triple, useful since all of the @gtags@ dbs are ternary
-- relations
data Raw = Raw String String String

instance SQL3.ToRow Raw where
  toRow (Raw a b c) = SQL3.toRow ( a , b , c )
