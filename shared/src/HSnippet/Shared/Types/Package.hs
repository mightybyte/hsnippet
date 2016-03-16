{-# LANGUAGE TemplateHaskell #-}

module HSnippet.Shared.Types.Package where

------------------------------------------------------------------------------
import           Data.Aeson.TH
import           Data.Text (Text)
import qualified Data.Text as T
------------------------------------------------------------------------------

data Package = Package
    { packageName :: Text
    , packageVersion :: Text
    }
  deriving (Eq, Ord, Show, Read)

mkPackage :: Text -> Package
mkPackage t = Package name ver
  where
    rev = T.reverse t
    ver = T.reverse $ T.takeWhile (/= '-') rev
    name = T.reverse $ T.tail $ T.dropWhile (/= '-') rev

deriveJSON defaultOptions ''Package

