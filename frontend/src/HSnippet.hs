{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE RecursiveDo              #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE UndecidableInstances     #-}

module HSnippet where

------------------------------------------------------------------------------
import           Control.Error
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans
import           Data.Char
import           Data.Dependent.Sum (DSum (..))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Data.String.Conv
import           Data.Text (Text)
import qualified Data.Text as T
import           GHCJS.DOM.Types hiding (Event, Text)
#ifdef ghcjs_HOST_OS
import           GHCJS.Foreign.Callback
import           GHCJS.Types
#endif
import           Reflex
import           Reflex.Host.Class
import           Reflex.Dom
import           Reflex.Dom.Contrib.Utils
import           Reflex.Dom.Contrib.Widgets.EditInPlace
import           Text.Printf
------------------------------------------------------------------------------
import           HSnippet.FrontendState
import           HSnippet.Lib
import           HSnippet.Tabs
import           HSnippet.Shared.Types.ExampleSnippet
import           HSnippet.Shared.Types.BuildMessage
import           HSnippet.Shared.Types.BuildResults
import           HSnippet.Shared.Types.Package
------------------------------------------------------------------------------

data Menu t = Menu
    { runEvent :: Event t ()
    , loadExample :: Event t ExampleSnippet
    }

runApp :: MonadWidget t m => App t m ()
runApp = do
    rec mData <- menu fs2
        fs2 <- elAttr "div" ("class" =: "ui two column padded grid" <>
                      "style" =: "height: calc(100% - 40px)") $ do
          rec inData <- leftColumn (loadExample mData) (roErrorJump ro)
              fs1 <- stateManager inData (runEvent mData)
              ro <- rightColumn fs1
          return fs1
    return ()

menu :: MonadWidget t m => FrontendState t -> m (Menu t)
menu fs = do
    divClass "ui blue inverted attached borderless menu" $ do
      elClass "span" "item active" $ text "HSnippet"
      elClass "span" "item" $ do
        rec title <- holdDyn "Untitled" titleEdits
            titleEdits <- editInPlace (constant True) title
        return ()
      divClass "right menu" $ do
        runAttrs <- mapDyn mkRunAttrs (fsBuildStatus fs)
        loadClick <- examplesDropdown (fsExamples fs)
        runClick <- icon "play" runAttrs "Run"
        elAttr "a" ("class" =: "item" <> "href" =: "/logout") $
          text "Sign Out"
        return $ Menu runClick loadClick
  where
    mkRunAttrs Building = ("class" =: "disabled")
    mkRunAttrs _ = mempty

examplesDropdown
    :: MonadWidget t m
    => Dynamic t [ExampleSnippet]
    -> m (Event t ExampleSnippet)
examplesDropdown exs = do
    divClass "ui simple dropdown item" $ do
      divClass "text" $ text "Examples"
      divClass "menu" $ do
        res <- widgetHold (return never) $
          (liftM leftmost . mapM singleExample) <$> updated exs
        return $ switch $ current res

singleExample
    :: MonadWidget t m
    => ExampleSnippet
    -> m (Event t ExampleSnippet)
singleExample es = do
    (e, _) <- elAttr' "div" ("class" =: "item") $
                text $ exampleName es
    return $ es <$ domEvent Click e


newtype AceRef = AceRef { unAceRef :: JSVal }

data ACE t = ACE
    { aceRef :: Dynamic t (Maybe AceRef)
    , aceValue :: Dynamic t String
    }

------------------------------------------------------------------------------
startACE :: String -> IO AceRef
#ifdef ghcjs_HOST_OS
startACE = js_startACE . toJSString

foreign import javascript unsafe
  "(function(){ var a = ace['edit']($1); a.session.setMode(\"ace/mode/haskell\"); return a; })()"
  js_startACE :: JSString -> IO AceRef
#else
startACE = error "startACE: can only be used with GHCJS"
#endif

------------------------------------------------------------------------------
moveCursorToPosition :: AceRef -> (Int, Int) -> IO ()
#ifdef ghcjs_HOST_OS
moveCursorToPosition a (r,c) = js_moveCursorToPosition a r c

foreign import javascript unsafe
  "(function(){ $1['gotoLine']($2, $3, true); })()"
  js_moveCursorToPosition :: AceRef -> Int -> Int -> IO ()
#else
moveursorToPosition = error "moveCursorToPosition: can only be used with GHCJS"
#endif

------------------------------------------------------------------------------
aceGetValue :: AceRef -> IO String
#ifdef ghcjs_HOST_OS
aceGetValue a = fromJSString <$> js_aceGetValue a

foreign import javascript unsafe
  "(function(){ return $1['getValue'](); })()"
  js_aceGetValue :: AceRef -> IO JSString
#else
aceGetValue = error "aceGetValue: can only be used with GHCJS"
#endif

------------------------------------------------------------------------------
setupValueListener :: MonadWidget t m => AceRef -> m (Event t String)
#ifdef ghcjs_HOST_OS
setupValueListener ace = do
    postGui <- askPostGui
    runWithActions <- askRunWithActions
    e <- newEventWithTrigger $ \et -> do
          cb <- asyncCallback1 $ \_ -> liftIO $ do
              v <- aceGetValue ace
              postGui $ runWithActions [et :=> Identity v]
          js_setupValueListener ace cb
          return (return ())
          -- TODO Probably need some kind of unsubscribe mechanism
          --return $ liftIO unsubscribe
    return $! e

foreign import javascript unsafe
  "(function(){ $1['on'](\"change\", $2); })()"
  js_setupValueListener :: AceRef -> Callback (JSVal -> IO ()) -> IO ()
#else
setupValueListener = error "setupValueListener: can only be used with GHCJS"
#endif


------------------------------------------------------------------------------
aceWidget :: MonadWidget t m => String -> m (ACE t)
aceWidget initContents = do
    let elemId = "editor"
    elAttr "pre" ("id" =: elemId <> "class" =: "ui segment") $ text initContents

    --------------------------------------------------------------------------
    --ace <- liftIO $ startACE elemId
    --editorUpdates <- setupValueListener ace

    pb <- getPostBuild
    aceUpdates <- performEvent (liftIO (startACE "editor") <$ pb)
    res <- widgetHold (return never) $ setupValueListener <$> aceUpdates
    aceDyn <- holdDyn Nothing $ Just <$> aceUpdates
    updatesDyn <- holdDyn initContents $ switchPromptlyDyn res
    --------------------------------------------------------------------------

    return $ ACE aceDyn updatesDyn


------------------------------------------------------------------------------
aceMoveCursor :: MonadWidget t m => ACE t -> Event t (Int,Int) -> m ()
aceMoveCursor ace posE =
    performEvent_ $ attachDynWith f (aceRef ace) posE
  where
    f Nothing pos = return ()
    f (Just ref) pos =
      liftIO $ moveCursorToPosition ref pos


------------------------------------------------------------------------------
leftColumn
    :: MonadWidget t m
    => Event t ExampleSnippet
    -> Event t (Int,Int)
    -> m (Snippet t)
leftColumn newExample pos = do
    ace <- divClass "left column full-height" $
      elClass "form" "ui form full-height" $ do
        elAttr "div" ("style" =: "height: 100%") $ do
          --elAttr "div" ("class" =: "field") importsWidget
          elAttr "div" ("class" =: "field") $ do
            ace <- aceWidget example
            divClass "ace-results" $ dynText $ aceValue ace
            aceMoveCursor ace pos
            return ace
            --textArea $ def & attributes .~ (constDyn $ "class" =: "code full-height")
            --               & textAreaConfig_initialValue .~ example
            --               & setValue .~ (exampleCode <$> newExample)
    --return $ Snippet (value ta)
    return $ Snippet $ aceValue ace
  where
    example = unlines
      [ "app :: MonadWidget t m => App t m ()"
      , "app = do"
      , "  ti <- textInput $ TextInputConfig \"range\" \"4\" never"
      , "                    (constDyn $ \"min\" =: \"1\" <> \"max\" =: \"6\")"
      , "  n <- holdDyn (4::Int) (read <$> updated (value ti))"
      , "  let diagramSize = D.mkSizeSpec2D (Just 600) (Just 600)"
      , "      f = reflexDia (def & sizeSpec .~ diagramSize) . example"
      , "  el \"div\" $ widgetHoldHelper f 4 (updated n)"
      , "  return ()"
      , ""
      , "hilbert 0 = mempty"
      , "hilbert n = hilbert' (n-1) D.# D.reflectY <> D.vrule 1"
      , "         <> hilbert  (n-1) <> D.hrule 1"
      , "         <> hilbert  (n-1) <> D.vrule (-1)"
      , "         <> hilbert' (n-1) D.# D.reflectX"
      , "  where"
      , "    hilbert' m = hilbert m D.# D.rotateBy (1/4)"
      , ""
      , "example n = D.frame 1 . D.lw D.thin . D.lc D.darkred . D.fc D.white"
      , "                  . D.strokeT $ hilbert n"
      ]

importsWidget :: MonadWidget t m => m ()
importsWidget = do
    divClass "ui form" $ do
        importDropdown
    return ()


data ImportType = PlainImport
                | Qualified
                | Hiding
                | Explicit
  deriving (Eq,Ord,Show,Read)


importTypeNames :: Map ImportType String
importTypeNames = M.fromList
    [ (PlainImport, "")
    , (Qualified, "qualified")
    , (Hiding, "hiding")
    , (Explicit, "explicit")
    ]


------------------------------------------------------------------------------
importDropdown
    :: MonadWidget t m
    => m (Event t ())
importDropdown = do
    let initial = PlainImport
    divClass "fields" $ do
      divClass "field" $ el "label" $ text "Import"
      v <- divClass "three wide field" $ do
        dropdown initial (constDyn importTypeNames) $
                 def & attributes .~ constDyn ("class" =: "ui fluid dropdown")
      widgetHoldHelper importDetails initial (updated $ value v)
    return never
--  where
--    opt t = elAttr "option" ("value" =: t) $ text t

importDetails :: MonadWidget t m => ImportType -> m ()
importDetails PlainImport = return ()
importDetails Qualified = do
    divClass "one wide field" $ text " as "
    divClass "three wide field" $ textInput def
    return ()
importDetails Hiding = do
    divClass "three wide field" $ textInput def
    return ()
importDetails Explicit = do
    divClass "three wide field" $ textInput def
    return ()

-- | Output of the right column
data RightOutput t = RightOutput
    { roErrorJump :: Event t (Int,Int)
    }

rightColumn :: MonadWidget t m => FrontendState t -> m (RightOutput t)
rightColumn fs = do
    divClass "right column full-height" $ rightTabs fs

data OutputTab = AppTab | OutTab | PackagesTab
  deriving (Eq,Show,Ord,Enum)

showTab :: OutputTab -> String
showTab AppTab = "App"
showTab OutTab = "Console"
showTab PackagesTab = "Packages"

setTabFromBuildStatus :: BuildStatus -> OutputTab
setTabFromBuildStatus (Built br) =
    if brSuccess br
      then AppTab
      else OutTab
setTabFromBuildStatus Building = OutTab
setTabFromBuildStatus _ = OutTab

instance MonadWidget t m => Tab t m OutputTab where
    tabIndicator t active = do
      let staticAttrs = "class" =: "item"
      attrs <- addActiveClass active (constDyn staticAttrs)
      (e,_) <- elDynAttr' "a" attrs (text $ showTab t)
      return $ domEvent Click e

rightTabs :: MonadWidget t m => FrontendState t -> m (RightOutput t)
rightTabs fs = do
    let buildStatus = fsBuildStatus fs
    curTab <- divClass "ui top attached menu" $ do
      tabBar OutTab [AppTab, OutTab, PackagesTab] never
             (setTabFromBuildStatus <$> updated buildStatus)
    tabPane tabAttrs curTab AppTab $ do
      divClass "segment full-height" $ do
        widgetHoldHelper jsOutput NotBuilt $ updated buildStatus
    errorJump <- tabPane tabAttrs curTab OutTab $ do
      divClass "grey segment full-height console-out" $ do
        elAttr "div" incrAttrs $ buildMessagesWidget fs
    tabPane tabAttrs curTab PackagesTab $ do
      divClass "segment full-height console-out" $ do
        packagesTab fs
    return $ RightOutput errorJump
  where
    incrAttrs = "class" =: "grey segment console-out" <>
                "style" =: "height: 100%"
    tabAttrs = "class" =: "ui bottom attached segment" <>
               "style" =: "height: calc(100% - 40px)"

------------------------------------------------------------------------------
buildMessagesWidget
    :: MonadWidget t m
    => FrontendState t
    -> m (Event t (Int,Int))
buildMessagesWidget fs = do
    ee <- elClass "table" "ui compact table" $
      el "tbody" $
        dyn =<< mapDyn (mapM buildMessageWidget) (fsBuildOut fs)
    return . switch =<< hold never (leftmost <$> ee)

buildMessageWidget
    :: MonadWidget t m
    => Either Text BuildMessage
    -> m (Event t (Int,Int))
buildMessageWidget (Left t) = do
    el "tr" $ do
      el "td" blank
      el "td" $ el "pre" $ text $ toS t
    return never
buildMessageWidget (Right bm) = do
    let klass = case _bmType bm of
                  BuildError -> "error"
                  BuildWarning -> "warning"
    rec isOpen <- toggle False clk
        (clk, jump) <- elAttr "tr" ("class" =: klass) $ do
          (e, _) <- elAttr' "td" ("class" =: "collapsing" <>
                               "style" =: "cursor:pointer") $ do
            iconAttrs <- forDyn isOpen $ \open ->
              if open
                 then ("class" =: "minus icon")
                 else ("class" =: "plus icon")
            elDynAttr "i" iconAttrs blank
          lnk2 <- el "td" $ el "pre" $ do
            lnk1 <- link $ printf "%s line %d, col %d"
                             (over (element 0) toUpper klass)
                             (_bmLine bm) (_bmCol bm)
            text $ ": " <>
              (maybe "" (T.unpack . T.strip) $ atMay (_bmLines bm) 1)
            return lnk1
          return (domEvent Click e, lnk2)
    attrs <- forDyn isOpen $ \open ->
      if open
        then ("class" =: klass)
        else ("class" =: klass <> "style" =: "display: none")
    elDynAttr "tr" attrs $ do
      elClass "td" "collapsing" blank
      el "td" $ el "pre" $ text (T.unpack $ T.unlines $ _bmLines bm)
    return $ (_bmLine bm, _bmCol bm - 1) <$ _link_clicked jump

jsOutput :: MonadWidget t m => BuildStatus -> m ()
jsOutput NotBuilt = do
    el "p" $ text "This is where the snippet's output will go."
jsOutput Building = loading
jsOutput BuildFailed = do
    elClass "h2" "red text" $ text "Error"
    elClass "p" "text" $ text "Build failed"
jsOutput (Built br) = do
    let file = "/snippets/"++brSnippetHash br++"/index.html"
    elAttr "iframe" ("src" =: file) blank

packagesTab
    :: MonadWidget t m
    => FrontendState t
    -> m ()
packagesTab fs = do
    packageMap <- mapDyn (M.fromList . zip [0..]) $ fsPackages fs
    elClass "table" "ui striped table" $ do
      el "thead" $
        el "tr" $ do
          el "th" $ text "Package"
          el "th" $ text "Version"
          el "th" $ text "Docs"
      listWithKey packageMap packageInfoWidget
      return ()

packageInfoWidget
    :: MonadWidget t m
    => Int
    -> Dynamic t Package
    -> m ()
packageInfoWidget _ package = do
    name <- mapDyn (toS . packageName) package
    version <- mapDyn (toS . packageVersion) package
    haddockAttrs <- mapDyn mkHaddock package
    el "tr" $ do
      el "td" $ dynText name
      el "td" $ dynText version
      el "td" $ elDynAttr "a" haddockAttrs $ text "docs"
  where
    mkHaddock Package{..} =
      "href" =: (toS $ "http://hackage.haskell.org/package/" <>
                       packageName <> "-" <> packageVersion) <>
      "target" =: "_blank"

icon
    :: MonadWidget t m
    => String
    -> Dynamic t (Map String String)
    -> String
    -> m (Event t ())
icon nm attrs txt = do
    as <- mapDyn (M.insertWith (\n o -> unwords [o,n]) "class" "item") attrs
    (e,_) <- elDynAttr' "a" as $ do
      elClass "i" (unwords [nm, "icon"]) blank
      text txt
    return $ domEvent Click e

loading :: MonadWidget t m => m ()
loading = do
    divClass "ui segment full-height" $ do
      divClass "ui active dimmer full-height" $
        divClass "ui large text loader" $ text "Building..."
      replicateM_ 3 $ el "p" blank
