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

data Package = Package
    { packageName :: Text
    , packageVersion :: Text
    , packageModules :: [Text]
    --, packageFields :: Map String [String]
    }
  deriving (Eq, Ord, Show, Read)

printPackage :: Package -> IO ()
printPackage Package{..} = do
    print packageName
    print packageVersion
    print packageModules

mkPackage :: Text -> Package
mkPackage t = Package name ver [] --mempty
  where
    rev = T.reverse t
    ver = T.reverse $ T.takeWhile (/= '-') rev
    name = T.strip $ T.reverse $ T.tail $ T.dropWhile (/= '-') rev

deriveJSON defaultOptions ''Package

mkPackageFromDump :: [String] -> Package
mkPackageFromDump pkgLines = Package (T.pack n) (T.pack v) ms --fieldMap
  where
    fs@((_,[n]):(_,[v]):_) = parsePkgInfo pkgLines
    fieldMap = M.fromList fs
    ms = T.words $ T.unlines $ map T.pack $ filter (not . null) $
           fromMaybe [] $ M.lookup "exposed-modules" fieldMap

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
