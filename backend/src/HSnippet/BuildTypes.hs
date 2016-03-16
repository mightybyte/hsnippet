{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HSnippet.BuildTypes where

------------------------------------------------------------------------------
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Digest.Pure.SHA
import           Data.Monoid
import           Data.String.Conv
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Data.Text            (Text)
import           System.Directory
import           System.FilePath
import           System.Process
------------------------------------------------------------------------------
import           HSnippet.Shared.Types.Package
------------------------------------------------------------------------------

data SnippetBlob = SnippetBlob
    { sbContents :: Text
    , sbHash     :: Digest SHA1State
    }

mkSnippetBlob :: Text -> SnippetBlob
mkSnippetBlob t = SnippetBlob t (sha1 $ toS t)

buildRoot :: FilePath
buildRoot = "userbuild"

sbName :: SnippetBlob -> String
sbName SnippetBlob{..} = "Snippet_" ++ showDigest sbHash

sbInnerRoot :: SnippetBlob -> String
sbInnerRoot sb = "snippets/" ++ sbName sb

sbRoot :: SnippetBlob -> String
sbRoot sb = buildRoot </> sbInnerRoot sb

sbOutput :: SnippetBlob -> String
sbOutput sb = sbRoot sb </> "run-stdout.txt"

sbSnippetFile :: SnippetBlob -> String
sbSnippetFile sb = buildRoot </> sbName sb <.> "snippet"

sbJsOut :: SnippetBlob -> String
sbJsOut sb = sbRoot sb </> "out.js.gz"

getSuccess :: SnippetBlob -> IO Bool
getSuccess sb = doesFileExist $ sbRoot sb </> "success"

hasBuildEnvironment :: MaybeT IO ()
hasBuildEnvironment = do
    myGuard =<< lift (doesDirectoryExist buildRoot)
    myGuard =<< lift (doesFileExist (buildRoot </> "default.nix"))
    return ()
  where
    myGuard True = return ()
    myGuard False = MaybeT $ return Nothing

getBuildEnvPackages :: IO [Package]
getBuildEnvPackages = do
    let cmd = nixShellCmd "ghcjs-pkg list"
    putStrLn "Getting packages in the snippet build environment"
    let cp = (shell cmd) { cwd = Just buildRoot }
    (_, o, _) <- readCreateProcessWithExitCode cp ""
    return $ map (Package . T.pack) $ tail $ lines o

buildSnippet :: Text -> IO (String, Bool)
buildSnippet snippet = do
    res <- runMaybeT $ do
      hasBuildEnvironment
      liftIO $ do
          putStrLn $ "Building " ++ (sbRoot sb </> file)
          createDirectoryIfMissing True (sbRoot sb)
          copyFile (buildRoot </> "template.hs") (sbRoot sb </> "Main.hs")
          T.appendFile (sbRoot sb </> "Main.hs") suffix
          let cp = (shell $ nixShellCmd $ ghcjsBuildCmd sb outDir) { cwd = Just buildRoot }
          (_, o, e) <- readCreateProcessWithExitCode cp ""
          writeFile (sbRoot sb </> "run-stdout.txt") o
          writeFile (sbRoot sb </> "run-stderr.txt") e
          exists <- doesDirectoryExist (sbRoot sb </> "Main.jsexe")
          return (exists, o, e)
    case res of
      Just (True, o, _) -> do
          setupResults sb
          writeFile (sbRoot sb </> "success") "success"
          return (o, True)
      Just (False, _, e) -> return (e, False)
      Nothing -> return ("Failed with no output", False)
  where
    sb = mkSnippetBlob snippet
    file = sbName sb <.> "snippet"
    outDir = sbInnerRoot sb </> "dist"
    suffix = T.unlines
      [ snippet
      , "main :: IO ()"
      , "main = appMain \"snippet-output\" app"
      ]

nixShellCmd :: String -> String
nixShellCmd cmd =
    unwords $ "nix-shell" : nixArgs
  where
    nixArgs = [ "-A", "env"
              , "--pure"
              , "-j", "8"
              , "-I", "../deps"
              , "--command"
              , "\"" ++ cmd ++ "; exit $?\""
              ]

ghcjsBuildCmd :: SnippetBlob -> String -> String
ghcjsBuildCmd sb outDir = unwords
    [ "ghcjs"
    , "--make"
    , "-j4"
    , "-static"
    , "-outputdir", outDir
    , "-odir", outDir
    , "-hidir", outDir
    , "-stubdir", outDir
    , "-isnippets"
    , "-I" ++ sbInnerRoot sb
    , "-XHaskell2010"
    , sbInnerRoot sb </> "Main.hs"
    , "-O2"
    , "-Wall"
    , "-fno-warn-unused-imports"
    , "-fno-warn-unused-do-bind"
    , "-fno-warn-orphans"
--    , "&>"
--    , sbOutput sb
    ]

setupResults :: SnippetBlob -> IO ()
setupResults sb = do
    _ <- system $ unwords
           [ "cat"
           , jsexe </> "out.js"
           , jsexe </> "runmain.js"
           , ">", sbRoot sb </> "out.js"
           ]
    copyFile (jsexe </> "rts.js") (sbRoot sb </> "rts.js")
    copyFile (jsexe </> "lib.js") (sbRoot sb </> "lib.js")
    _ <- rawSystem "gzip" ["-k", "-f", sbRoot sb </> "out.js"]
    copyFile (buildRoot </> "template.html") (sbRoot sb </> "index.html")
    T.appendFile (sbRoot sb </> "index.html") (T.pack htmlSuffix)
  where
    jsexe = sbRoot sb </> "Main.jsexe"
    htmlSuffix = "<script language='javascript' src='/" <> (sbInnerRoot sb </> "out.js") <>
                 "' defer></script></html>"

