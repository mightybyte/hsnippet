{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module HSnippet where

------------------------------------------------------------------------------
import           Control.Monad
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Contrib.Utils
import           Reflex.Dom.Contrib.Widgets.EditInPlace
------------------------------------------------------------------------------
import           HSnippet.Lib
import           HSnippet.Tabs
import           HSnippet.Shared.Types.BuildResults
import           HSnippet.XmlHttpRequest
------------------------------------------------------------------------------

data Menu t = Menu
    { runEvent :: Event t ()
    }

data Snippet t = Snippet
    { snippetCode :: Dynamic t String
    }

data BuildStatus = NotBuilt | Building | BuildFailed | Built BuildResults
  deriving (Eq,Show,Ord)

data Output t = Output
    { buildStatus :: Dynamic t BuildStatus
    }

appController
    :: MonadWidget t m
    => Menu t
    -> Snippet t
    -> m (Output t)
appController mData inData = do
    newCode <- buildCode $ tagDyn (snippetCode inData) (runEvent mData)
    code <- holdDyn Nothing $ traceEvent "newCode" newCode
    status <- holdDyn NotBuilt $ leftmost
      [ Building <$ runEvent mData
      , maybe BuildFailed Built <$> updated code
      ]
    return $ Output status

runApp :: MonadWidget t m => App t m ()
runApp = do
    rec mData <- menu out
        out <- elAttr "div" ("class" =: "ui two column padded grid" <>
                      "style" =: "height: calc(100% - 40px)") $ do
          inData <- leftColumn
          outData <- appController mData inData
          rightColumn outData
          return outData
    return ()

menu :: MonadWidget t m => Output t -> m (Menu t)
menu outData = do
    divClass "ui blue inverted attached borderless menu" $ do
      elClass "span" "item active" $ text "HSnippet"
      elClass "span" "item" $ do
        rec title <- holdDyn "Untitled" titleEdits
            titleEdits <- editInPlace (constant True) title
        return ()
      divClass "right menu" $ do
        runAttrs <- mapDyn mkRunAttrs (buildStatus outData)
        runClick <- icon "play" runAttrs "Run"
        elAttr "a" ("class" =: "item" <> "href" =: "/logout") $
          text "Sign Out"
        return $ Menu runClick
  where
    mkRunAttrs Building = ("class" =: "disabled")
    mkRunAttrs _ = mempty

------------------------------------------------------------------------------
leftColumn :: MonadWidget t m => m (Snippet t)
leftColumn = do
    ta <- divClass "left column full-height" $
      elClass "form" "ui form full-height" $ do
        elAttr "div" ("class" =: "field" <>
                      "style" =: "height: 100%") $ do
          textArea $ def & attributes .~ (constDyn $ "class" =: "code full-height")
                         & textAreaConfig_initialValue .~ example
    return $ Snippet (value ta)
  where
    --example = "main = appMain $ text \"aoeu\""
    example = unlines
      [ "main = appMain $ do"
      , "  rec str <- holdDyn \"Click to edit me\" edits"
      , "      edits <- editInPlace (constant True) str"
      , "  return ()"
      ]

rightColumn :: MonadWidget t m => Output t -> m ()
rightColumn out = do
    divClass "right column full-height" $ rightTabs out
    return ()

data OutputTab = ConsoleTab | AppTab
  deriving (Eq,Show,Ord,Enum)

showTab :: OutputTab -> String
showTab ConsoleTab = "Console"
showTab AppTab = "App"

setTabFromBuildStatus :: BuildStatus -> OutputTab
setTabFromBuildStatus (Built br) =
    if brSuccess br
      then AppTab
      else ConsoleTab
setTabFromBuildStatus _ = ConsoleTab

instance MonadWidget t m => Tab t m OutputTab where
    tabIndicator t active = do
      let staticAttrs = "class" =: "item"
      attrs <- addActiveClass active (constDyn staticAttrs)
      (e,_) <- elDynAttr' "a" attrs (text $ showTab t)
      return $ domEvent Click e

rightTabs :: MonadWidget t m => Output t -> m ()
rightTabs Output{..} = do
    curTab <- divClass "ui top attached menu" $ do
      tabBar ConsoleTab [ConsoleTab, AppTab] never
             (setTabFromBuildStatus <$> updated buildStatus)
    tabPane tabAttrs curTab ConsoleTab $ do
      divClass "grey segment full-height console-out" $ do
        widgetHoldHelper consoleOutput NotBuilt $ updated buildStatus
    tabPane tabAttrs curTab AppTab $ do
      divClass "segment full-height" $ do
        widgetHoldHelper jsOutput NotBuilt $ updated buildStatus
    return ()
  where
    tabAttrs = "class" =: "ui bottom attached segment" <>
               "style" =: "height: calc(100% - 40px)"

consoleOutput :: MonadWidget t m => BuildStatus -> m ()
consoleOutput NotBuilt = do
    el "p" $ text "This is where the snippet's output will go."
consoleOutput Building = loading
consoleOutput BuildFailed = do
    elClass "h2" "red text" $ text "Server Error"
    elClass "p" "text" $ text "There was an unexpected problem"
consoleOutput (Built br) = do
    el "pre" $ text $ brConsoleOut br

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
