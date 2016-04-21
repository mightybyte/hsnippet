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

module HSnippet.SnippetInput where

------------------------------------------------------------------------------
import           Data.Monoid
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           ACE
import           HSnippet.FrontendState
import           HSnippet.Imports
import           HSnippet.Tabs
import           HSnippet.Shared.Types.ExampleSnippet
import           HSnippet.Shared.Types.Package
------------------------------------------------------------------------------


data InputTab = ImportsTab | CodeTab
  deriving (Eq,Show,Ord,Enum)

showInputTab :: InputTab -> String
showInputTab ImportsTab = "Imports"
showInputTab CodeTab = "Code"

instance MonadWidget t m => Tab t m InputTab where
    tabIndicator t active = do
      let staticAttrs = "class" =: "item"
      attrs <- addActiveClass active (constDyn staticAttrs)
      (e,_) <- elDynAttr' "a" attrs (text $ showInputTab t)
      return $ domEvent Click e

------------------------------------------------------------------------------
leftColumn
    :: MonadWidget t m
    => FrontendState t
    -> Event t ExampleSnippet
    -> Event t (Int,Int)
    -> m (LeftColumnOut t)
leftColumn fs newExample pos = do
    divClass "left column full-height" $ do
      curTab <- divClass "ui top attached menu" $ do
        tabBar CodeTab [CodeTab, ImportsTab] never never
      code <- tabPane tabAttrs curTab CodeTab $ codeInput newExample pos
      iOut <- tabPane tabAttrs curTab ImportsTab $ importsWidget fs
      return $ LeftColumnOut code (ioutImports iOut) (ioutRefresh iOut)
                             (ioutModuleUpdates iOut)
  where
    tabAttrs = "class" =: "ui bottom attached segment" <>
               "style" =: "height: calc(100% - 40px); overflow-y: auto"


codeInput
    :: MonadWidget t m
    => Event t ExampleSnippet
    -> Event t (Int,Int)
    -> m (Dynamic t String)
codeInput newExample pos = do
    ace <- elClass "form" "ui form full-height" $ do
      elAttr "div" ("style" =: "height: 100%") $ do
        elAttr "div" ("class" =: "field") $ do
          ace <- aceWidget example
          divClass "ace-results" $ dynText $ aceValue ace
          aceMoveCursor ace pos
          aceSetValue ace (exampleCode <$> newExample)
          return ace
    return $ aceValue ace
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
