{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module HSnippet where

------------------------------------------------------------------------------
import           Data.Monoid
import           Reflex.Dom hiding (getKeyEvent)
import           Reflex.Dom.Contrib.KeyEvent
------------------------------------------------------------------------------

data AppState t = AppState
    { bsKeyDown :: Event t KeyEvent
    , bsKeyPress :: Event t KeyEvent
    }

runApp :: MonadWidget t m => m ()
runApp = do
    menu
    elAttr "div" ("class" =: "ui two column padded grid" <>
                  "style" =: "height: calc(100% - 40px)") $ do
      leftColumn
      rightColumn

menu :: MonadWidget t m => m ()
menu = do
    divClass "ui blue inverted attached menu" $ do
      elClass "span" "item" $ text "HSnippet"
      divClass "right menu" $
        elAttr "a" ("class" =: "item" <> "href" =: "/logout") $
          text "Sign Out"

leftColumn :: MonadWidget t m => m ()
leftColumn = do
    fullHeight "div" "left column" $
      fullHeight "form" "ui form" $ do
        divClass "field" $
          elAttr "input" ("type" =: "text" <> "placeholder" =: "Snippet Name") blank
        elAttr "div" ("class" =: "field" <>
                      "style" =: "height: calc(100% - 52px)") $ do
          fullHeight "textarea" "snippet-code" blank

rightColumn :: MonadWidget t m => m ()
rightColumn = do
    fullHeight "div" "grey right column" $ do
      el "p" $ text "This is where the snippet's output will go."


fullHeight :: MonadWidget t m => String -> String -> m a -> m a
fullHeight e cls = elAttr e ("class" =: cls <> "style" =: "height:100%; max-height:100%")
