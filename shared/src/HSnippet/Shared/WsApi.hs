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

upSummary :: Up -> String
upSummary Up_GetPackages = "Up_GetPackages"
upSummary Up_GetExamples = "Up_GetExamples"
upSummary (Up_GetExports _) = "Up_GetExports"
upSummary (Up_RunSnippet _) = "Up_RunSnippet"

data Down = Down_Packages [Package]
          | Down_BuildFinished BuildResults
          | Down_BuildOutLine [Either Text BuildMessage]
          | Down_Examples [ExampleSnippet]
          | Down_Exports (Module, [Export])
  deriving (Eq, Ord, Show, Read)

downSummary :: Down -> String
downSummary (Down_Packages _) = "Down_Packages"
downSummary (Down_BuildFinished _) = "Down_BuildFinished"
downSummary (Down_BuildOutLine _) = "Down_BuildOutLine"
downSummary (Down_Examples _) = "Down_Examples"
downSummary (Down_Exports _) = "Down_Exports"

makePrisms ''Up
makePrisms ''Down

deriveJSON defaultOptions ''Up
deriveJSON defaultOptions ''Down
