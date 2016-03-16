{-# LANGUAGE TemplateHaskell #-}

module HSnippet.Shared.WsApi where

------------------------------------------------------------------------------
import Control.Lens.TH
import Data.Aeson.TH
------------------------------------------------------------------------------
import HSnippet.Shared.Types.Package
------------------------------------------------------------------------------

data Up = Up_GetPackages
        | Up_Dummy
  deriving (Eq, Ord, Show, Read)

data Down = Down_Packages [Package]
          | Down_Dummy
  deriving (Eq, Ord, Show, Read)

makePrisms ''Up
makePrisms ''Down

deriveJSON defaultOptions ''Up
deriveJSON defaultOptions ''Down
