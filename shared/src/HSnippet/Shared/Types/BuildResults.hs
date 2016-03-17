{-# LANGUAGE TemplateHaskell #-}

module HSnippet.Shared.Types.BuildResults where

------------------------------------------------------------------------------
import           Data.Aeson.TH
------------------------------------------------------------------------------

data BuildResults = BuildResults
    { brSnippetHash :: String
    , brConsoleOut  :: String
    , brSuccess     :: Bool
    } deriving (Eq,Ord,Show,Read)

$(deriveJSON defaultOptions ''BuildResults)
