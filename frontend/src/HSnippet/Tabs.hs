{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-|

An API for constructing a tab bar where the list of tabs in the bar is
determined dynamically.

-}

module HSnippet.Tabs
  ( Tab(..)
  , tabBar
  , tabPane
  , addDisplayNone
  , addActiveClass
  ) where

------------------------------------------------------------------------------
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------
import           Reflex.Dom.Contrib.Utils
------------------------------------------------------------------------------


------------------------------------------------------------------------------
class Eq tab => Tab t m tab where
    tabIndicator :: tab -> Dynamic t Bool -> m (Event t ())


------------------------------------------------------------------------------
tabBar
    :: forall t m tab. (MonadWidget t m, Tab t m tab)
    => tab
    -- ^ Initial open tab
    -> [tab]
    -> Event t [tab]
    -- ^ Dynamic list of the displayed tabs
    -> Event t tab
    -- ^ Event updating the currently selected tab
    -> m (Dynamic t tab)
tabBar initialSelected initialTabs tabs curTab = do
    rec let tabFunc = mapM (mkTab currentTab)
        foo <- widgetHoldHelper tabFunc initialTabs tabs
        let bar :: Event t tab = switch $ fmap leftmost $ current foo
        currentTab <- holdDyn initialSelected $ leftmost [bar, curTab]
    return currentTab


------------------------------------------------------------------------------
mkTab
    :: (MonadWidget t m, Tab t m tab)
    => Dynamic t tab
    -> tab
    -> m (Event t tab)
mkTab currentTab t = do
    isSelected <- mapDyn (==t) currentTab
    e <- tabIndicator t isSelected
    return (t <$ e)


------------------------------------------------------------------------------
tabPane
    :: (MonadWidget t m, Eq tab)
    => Map String String
    -> Dynamic t tab
    -> tab
    -> m a
    -> m a
tabPane staticAttrs currentTab t child = do
    attrs <- addDisplayNone (constDyn staticAttrs) =<<
             mapDyn (==t) currentTab
    elDynAttr "div" attrs child


addDisplayNone
    :: MonadWidget t m
    => Dynamic t (Map String String)
    -> Dynamic t Bool
    -> m (Dynamic t (Map String String))
addDisplayNone attrs isActive = combineDyn f isActive attrs
  where
    f True as = as
    f False as = M.insert "style" "display: none" as


addActiveClass
    :: MonadWidget t m
    => Dynamic t Bool
    -> Dynamic t (Map String String)
    -> m (Dynamic t (Map String String))
addActiveClass isActive attrs = combineDyn f isActive attrs
  where
    f True as = M.insertWith (\n o -> unwords [o,n]) "class" "active" as
    f False as = as
