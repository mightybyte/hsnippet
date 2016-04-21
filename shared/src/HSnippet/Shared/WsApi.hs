{-# LANGUAGE TemplateHaskell #-}

module HSnippet.Shared.WsApi where

------------------------------------------------------------------------------
import Control.Lens.TH
import Data.Aeson.TH
import Data.Text (Text)
------------------------------------------------------------------------------
import HSnippet.Shared.Types.BuildMessage
import HSnippet.Shared.Types.BuildResults
import HSnippet.Shared.Types.ExampleSnippet
import HSnippet.Shared.Types.Package
import HSnippet.Shared.Types.SnippetContents
------------------------------------------------------------------------------

data Up = Up_GetPackages
        | Up_GetExamples
        | Up_GetExports Module
        | Up_RunSnippet SnippetContents
  deriving (Eq, Ord, Show, Read)

data Down = Down_Packages [Package]
          | Down_BuildFinished BuildResults
          | Down_BuildOutLine [Either Text BuildMessage]
          | Down_Examples [ExampleSnippet]
          | Down_Exports (Module, [Export])
  deriving (Eq, Ord, Show, Read)

makePrisms ''Up
makePrisms ''Down

deriveJSON defaultOptions ''Up
deriveJSON defaultOptions ''Down
