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
import           Data.Char
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Data.String.Conv
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex
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
import           HSnippet.SnippetInput
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
          rec inData <- leftColumn fs1 (loadExample mData) (roErrorJump ro)
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


-- | Output of the right column
data RightOutput t = RightOutput
    { roErrorJump :: Event t (Int,Int)
    }

rightColumn :: MonadWidget t m => FrontendState t -> m (RightOutput t)
rightColumn fs = do
    divClass "right column full-height" $ rightTabs fs

data OutputTab = AppTab | OutTab | PackagesTab
  deriving (Eq,Show,Ord,Enum)

showOutputTab :: OutputTab -> String
showOutputTab AppTab = "App"
showOutputTab OutTab = "Console"
showOutputTab PackagesTab = "Packages"

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
      (e,_) <- elDynAttr' "a" attrs (text $ showOutputTab t)
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
