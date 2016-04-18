{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module HSnippet.Shared.Types.Package where

------------------------------------------------------------------------------
import           Data.Aeson.TH
import           Data.Char
import qualified Data.Map as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
------------------------------------------------------------------------------

newtype Export = Export { exportName :: Text }
  deriving (Eq, Ord, Show, Read)

newtype Module = Module { moduleName :: Text }
  deriving (Eq, Ord, Show, Read)

deriveJSON defaultOptions ''Export
deriveJSON defaultOptions ''Module

data Package = Package
    { packageName      :: Text
    , packageVersion   :: Text
    , packageModules   :: [Module]
    , packageLibDir    :: Maybe Text
    --, packageFields :: Map String [String]
    }
  deriving (Eq, Ord, Show, Read)

printPackage :: Package -> IO ()
printPackage Package{..} = do
    print packageName
    print packageVersion
    print packageModules

deriveJSON defaultOptions ''Package

mkPackageFromDump :: [String] -> Package
mkPackageFromDump pkgLines = Package (T.pack n) (T.pack v) ms libDir
  where
    fs@((_,[n]):(_,[v]):_) = parsePkgInfo pkgLines
    fieldMap = M.fromList fs
    ms = map Module $ T.words $ T.unlines $ map T.pack $
           filter (not . null) $
           fromMaybe [] $ M.lookup "exposed-modules" fieldMap
    libDir = fmap T.pack . listToMaybe =<< M.lookup "library-dirs" fieldMap

parsePkgInfo :: [String] -> [(String, [String])]
parsePkgInfo [] = []
parsePkgInfo (l:ls) = ppi ls (parseLine l)

ppi :: [String] -> (String, [String]) -> [(String, [String])]
ppi [] accum = [accum]
ppi (l:ls) (k,accum) =
    if map isSpace (take 1 l) == [True]
       then ppi ls (k, accum ++ [dropWhile isSpace l])
       else (k,accum) : ppi ls (parseLine l)

parseLine :: String -> (String, [String])
parseLine l = (k2, [dropWhile isSpace $ tail rest])
  where
    (k2,rest) = break (==':') l
