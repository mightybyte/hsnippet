{-# LANGUAGE TemplateHaskell #-}

module HSnippet.Shared.Types.Package where

------------------------------------------------------------------------------
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Data.Time.Clock
------------------------------------------------------------------------------

data Package = Package { packageName :: Text }
  deriving (Eq, Ord, Show, Read)

deriveJSON defaultOptions ''Package

