{-# LANGUAGE TemplateHaskell #-}

module HSnippet.Shared.WsApi where

------------------------------------------------------------------------------
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Data.Time.Clock
------------------------------------------------------------------------------
import HSnippet.Shared.Types.Package
------------------------------------------------------------------------------

data Up = Up_GetPackages
  deriving (Eq, Ord, Show, Read)

data Down = Down_Packages [Package]
  deriving (Eq, Ord, Show, Read)

deriveJSON defaultOptions ''Up
deriveJSON defaultOptions ''Down
