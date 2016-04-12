{-# LANGUAGE RecordWildCards #-}

module HSnippet.Shared.Types.SnippetImport where

import Data.List

data ImportExtra = NoExtra
                 | QualifiedName String
                 | HidingSymbols [String]
                 | ExplicitSymbols [String]
  deriving (Eq,Ord,Show,Read)

data SnippetImport = SnippetImport
    { importModuleName :: String
    , importExtra      :: ImportExtra
    } deriving (Eq,Ord,Show,Read)

renderImport :: SnippetImport -> String
renderImport SnippetImport{..} = unwords
    [ "import"
    , qual
    , importModuleName
    , rest
    ]
  where
    qual = case importExtra of
             QualifiedName _ -> "qualified"
             _ -> "         "
    rest = case importExtra of
             QualifiedName q -> "as "++q
             HidingSymbols ss -> "hiding (" ++ intercalate ", " ss ++ ")"
             ExplicitSymbols ss -> "(" ++ intercalate ", " ss ++ ")"
             _ -> ""
