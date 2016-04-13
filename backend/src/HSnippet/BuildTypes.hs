{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HSnippet.BuildTypes where

------------------------------------------------------------------------------
import           Control.Exception
import           Control.Lens (over, each, _Right)
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Digest.Pure.SHA
import           Data.List.Split
import           Data.Monoid
import           Data.String.Conv
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Data.Text            (Text)
--import           GHC.IO.Handle
import           Network.WebSockets
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process
------------------------------------------------------------------------------
import           HSnippet.Shared.OutputParser
import           HSnippet.Shared.Types.BuildMessage
import           HSnippet.Shared.Types.Package
import           HSnippet.Shared.Types.SnippetContents
import           HSnippet.Shared.WsApi
import           HSnippet.Websocket
------------------------------------------------------------------------------

data SnippetBlob = SnippetBlob
    { sbContents :: Text
    , sbImports  :: Text
    , sbHash     :: Digest SHA1State
    }

mkSnippetBlob :: SnippetContents -> SnippetBlob
mkSnippetBlob sc = SnippetBlob (scCode sc) (scImports sc) (sha1 $ toS t)
  where
    t = scImports sc <> scCode sc

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
    return $ map (mkPackage . T.pack) $ filter (not . null) $ tail $ lines o

getPackageDump :: IO [Package]
getPackageDump = do
    let cmd = nixShellCmd $ "ghcjs-pkg dump"
    putStrLn $ "Getting packages and modules"
    let cp = (shell cmd) { cwd = Just buildRoot }
    (_, o, _) <- readCreateProcessWithExitCode cp ""
    return $ map mkPackageFromDump $ splitOn ["---"] $ lines o

buildSnippet
    :: (CreateProcess -> SnippetBlob -> Int -> IO (Bool, String, String))
    -> SnippetContents
    -> IO (String, Bool)
buildSnippet runner snippet = do
    res <- runMaybeT $ do
      hasBuildEnvironment
      liftIO $ do
          putStrLn $ "Building " ++ (sbRoot sb </> file)
          createDirectoryIfMissing True (sbRoot sb)
          let mainFile = sbRoot sb </> "Main.hs"
          header <- T.readFile (buildRoot </> "template.hs")
          let prefix = T.unlines [header, sbImports sb, divider]
          let !fullSnippet = prefix <> T.unlines
                [ sbContents sb
                , "main :: IO ()"
                , "main = appMain \"snippet-output\" app"
                ]
          T.writeFile mainFile fullSnippet
          let cp = (shell $ nixShellCmd $ ghcjsBuildCmd sb outDir)
                     { cwd = Just buildRoot }
          runner cp sb (length $ T.lines prefix)
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
    divider = T.unlines
      [ "import           HSnippet.Lib"
      , ""
      , "importAccessDenied :: Bool"
      , "importAccessDenied = True"
      ]

------------------------------------------------------------------------------
runProcessIncremental
    :: Connection
    -> CreateProcess
    -> SnippetBlob
    -> Int
    -> IO (Bool, String, String)
runProcessIncremental conn cp sb templateLines = do
    (_, Just o, Just e, _) <- createProcess $
      cp { std_out = CreatePipe, std_err = CreatePipe }
    catch (loop o e) handler1
    oStr <- catch (readFile oFile) handler2
    eStr <- catch (readFile eFile) handler2
    exists <- doesDirectoryExist (sbRoot sb </> "Main.jsexe")
    return (exists, oStr, eStr)
  where
    oFile = (sbRoot sb </> "run-stdout.txt")
    eFile = (sbRoot sb </> "run-stderr.txt")
    loop o e = do
      oOpen <- checkSendLine Down_BuildOutLine o oFile
      eOpen <- checkSendLine Down_BuildOutLine e eFile
      if oOpen || eOpen then loop o e else return ()
    cleanupStr = sbInnerRoot sb <> "/"
    cleanupFilenames = T.replace (T.pack cleanupStr) ""
    checkSendLine msg h f = do
      closed <- hIsClosed h
      if closed
        then return False
        else do input <- grabLines [] h
                when (not $ null input) $ do
                  appendFile f $ unlines input
                  let parsed = outParser $
                        map (cleanupFilenames . T.pack) input
                  wsSend conn $ msg $ fixLineNumbers templateLines parsed
                return True

fixLineNumbers
    :: Int
    -> [Either Text BuildMessage]
    -> [Either Text BuildMessage]
fixLineNumbers n = over (each . _Right . bmLine) (subtract n)

grabLines :: [String] -> Handle -> IO [String]
grabLines accum h = do
    hasInput <- hWaitForInput h 10
    if hasInput
      then do
        line <- hGetLine h
        if null line
           then return $ reverse accum
           else grabLines (line : accum) h
      else return $ reverse accum

handler1 :: SomeException -> IO ()
handler1 _ = return ()

handler2 :: SomeException -> IO String
handler2 _ = return ""

runProcessBatch
    :: CreateProcess
    -> SnippetBlob
    -> Int
    -> IO (Bool, String, String)
runProcessBatch cp sb _ = do
    (_, o, e) <- readCreateProcessWithExitCode cp ""
    writeFile (sbRoot sb </> "run-stdout.txt") o
    writeFile (sbRoot sb </> "run-stderr.txt") e
    exists <- doesDirectoryExist (sbRoot sb </> "Main.jsexe")
    return (exists, o, e)

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
    htmlPrefix <- T.readFile (buildRoot </> "template.html")
    T.writeFile (sbRoot sb </> "index.html") (htmlPrefix <> T.pack htmlSuffix)
  where
    jsexe = sbRoot sb </> "Main.jsexe"
    htmlSuffix = "<script language='javascript' src='/" <> (sbInnerRoot sb </> "out.js") <>
                 "' defer></script></html>"

