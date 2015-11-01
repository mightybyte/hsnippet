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
    divClass "left column full-height" $
      elClass "form" "ui form full-height" $ do
        divClass "field" $
          elAttr "input" ("type" =: "text" <> "placeholder" =: "Snippet Name") blank
        elAttr "div" ("class" =: "field" <>
                      "style" =: "height: calc(100% - 52px)") $ do
          elClass "textarea" "full-height" blank

rightColumn :: MonadWidget t m => m ()
rightColumn = do
    divClass "grey right column full-height" $ do
      el "p" $ text "This is where the snippet's output will go."


