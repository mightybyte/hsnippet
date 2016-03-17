{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

------------------------------------------------------------------------------
import qualified Data.Text.IO         as T
import           System.Environment
import           HSnippet.BuildTypes
------------------------------------------------------------------------------


main :: IO ()
main = do
    (file:_) <- getArgs
    code <- T.readFile file
    (out,good) <- buildSnippet runProcessBatch code
    putStrLn out
    putStrLn $ if good then "Build successful" else "Snippet build failed"
