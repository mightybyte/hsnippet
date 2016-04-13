{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module HSnippet.Shared.Types.SnippetContents where

------------------------------------------------------------------------------
import           Data.Aeson.TH
import           Data.Text (Text)
------------------------------------------------------------------------------

data SnippetContents = SnippetContents
    { scCode    :: Text
    , scImports :: Text
    }
  deriving (Eq, Ord, Show, Read)

deriveJSON defaultOptions ''SnippetContents

