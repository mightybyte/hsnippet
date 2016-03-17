{-# LANGUAGE TemplateHaskell       #-}

module HSnippet.Shared.Types.ExampleSnippet where

------------------------------------------------------------------------------
import           Control.Lens
import           Data.Aeson.TH
------------------------------------------------------------------------------

data ExampleSnippet = ExampleSnippet
    { exampleName :: String
    , exampleCode :: String
    } deriving (Eq,Ord,Show,Read)

makeLenses ''ExampleSnippet

deriveJSON defaultOptions ''ExampleSnippet

