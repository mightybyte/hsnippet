{-# LANGUAGE TemplateHaskell #-}

module HSnippet.Shared.Types.BuildResults where

------------------------------------------------------------------------------
import           Data.Aeson.TH
------------------------------------------------------------------------------

data BuildResults = BuildResults
    { brSnippetHash :: String
    , brConsoleOut  :: String
    , brConsoleErr  :: String
    , brSuccess     :: Bool
    } deriving (Eq,Show,Ord)

$(deriveJSON defaultOptions ''BuildResults)
