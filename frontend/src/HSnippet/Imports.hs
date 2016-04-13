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

module HSnippet.Imports where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.Trans
import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Data.Ord
import           Data.String.Conv
import           GHCJS.DOM.Types hiding (Event, Text)
#ifdef ghcjs_HOST_OS
import           GHCJS.Types
#endif
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Contrib.Utils
import           Safe
------------------------------------------------------------------------------
import           HSnippet.Shared.Types.Package
import           HSnippet.Shared.Types.SnippetImport
------------------------------------------------------------------------------


------------------------------------------------------------------------------
activateSemUiDropdown :: String -> IO ()
#ifdef ghcjs_HOST_OS
activateSemUiDropdown = js_activateSemUiDropdown . toJSString

foreign import javascript unsafe
  "$($1).dropdown({fullTextSearch: true});"
  js_activateSemUiDropdown :: JSString -> IO ()
#else
activateSemUiDropdown =
  error "activateSemUiDropdown: can only be used with GHCJS"
#endif


defaultImports :: Map Int SnippetImport
defaultImports = M.fromList $ zip [0..]
    [ p "Control.Lens"
    , p "Control.Monad"
    , p "Data.Aeson"
    , p "Data.Aeson.TH"
    , p "Data.Colour.Palette.BrewerSet"
    , e "Data.Map" ["Map"]
    , q "Data.Map" "M"
    , p "Data.String.Conv"
    , e "Data.Text" ["Text"]
    , q "Data.Text" "T"
    , p "Data.Monoid"
    , p "Diagrams.Backend.Reflex"
    , q "Diagrams.Prelude" "D"
    , p "GHC.Generics"
    , p "Reflex"
    , p "Reflex.Dom"
    , p "Reflex.Dom.Contrib.KeyEvent"
    , p "Reflex.Dom.Contrib.Pagination"
    , p "Reflex.Dom.Contrib.Utils"
    , p "Reflex.Dom.Contrib.Widgets.EditInPlace"
    , p "Reflex.Dom.Contrib.Widgets.Svg"
    , p "Reflex.Dom.Contrib.Xhr"
    ]
  where
    p m = SnippetImport m NoExtra
    q m a = SnippetImport m (QualifiedName a)
    e m fs = SnippetImport m (ExplicitSymbols fs)

------------------------------------------------------------------------------
importsWidget
    :: MonadWidget t m
    => Dynamic t [Package]
    -> m (Dynamic t [SnippetImport])
importsWidget packages = do
    divClass "ui small form" $ do
      newImport <- importDropdown packages
      let order = sortBy (comparing importModuleName)
      let addNew i im = if M.null im
                          then M.singleton (0 :: Int) i
                          else M.insert (fst (M.findMax im) + 1) i im
      rec imports <- foldDyn ($) defaultImports $ leftmost
                       [ addNew <$> newImport
                       , M.delete <$> delImport
                       ]
          res <- divClass "ui list" $ listViewWithKey imports $ \_ iDyn -> do
            elClass "pre" "item" $ do
              (minusEl,_) <- elAttr' "i" ("class" =: "minus icon") blank
              dynText =<< mapDyn renderImport iDyn
              return $ domEvent Click minusEl
          let delImport = fmapMaybe id $ fmap fst . headMay . M.toList <$> res
      mapDyn (order . M.elems) imports


data ImportType = PlainImport
                | Qualified
                | Hiding
                | Explicit
  deriving (Eq,Ord,Show,Read)


importTypeNames :: Map ImportType String
importTypeNames = M.fromList
    [ (PlainImport, "plain")
    , (Qualified, "qualified")
--    , (Hiding, "hiding")
--    , (Explicit, "explicit")
    ]

funcList :: [String]
funcList = ["Maybe", "Either", "listToMaybe", "catMaybes"]

mkMap :: [String] -> Map String String
mkMap = M.fromList . map (\nm -> (nm, nm))

packageToModules :: Package -> [(Maybe String, String)]
packageToModules Package{..} = map p packageModules
  where
    p t = let n = toS t in (Just n, n)


moduleMap :: [Package] -> Map (Maybe String) String
moduleMap ps = M.fromList $ d : concatMap packageToModules ps
  where
    d = (Nothing, "")

-- | Wrapper around the reflex-dom dropdown that calls the sem-ui dropdown
-- function after the element is built.
semUiDropdown
    :: (Ord a, Read a, Show a, MonadWidget t m)
    => String
       -- ^ Element id.  Ideally this should be randomly generated instead
       -- of passed in as an argument, but for now this approach is easier.
    -> a
       -- ^ Initial value
    -> Dynamic t (Map a String)
    -> Map String [Char]
    -> m (Dynamic t a)
semUiDropdown elId iv vals attrs = do
    d <- dropdown iv vals $ def &
      attributes .~ (constDyn $ attrs <> ("id" =: elId))
    pb <- getPostBuild
    performEvent_ (liftIO (activateSemUiDropdown ('#':elId)) <$ pb)
    return $ value d

------------------------------------------------------------------------------
importDropdown
    :: MonadWidget t m
    => Dynamic t [Package]
    -> m (Event t SnippetImport)
importDropdown ps = do
    let initial = PlainImport
    divClass "fields" $ do
      rec attrs <- mapDyn mkAttrs $ value v
          n <- elDynAttr "div" attrs $ do
            el "label" $ text "Module Name"
            mm <- mapDyn moduleMap ps
            semUiDropdown "import-serach" Nothing mm
              ("class" =: "ui search dropdown")
          v <- divClass "three wide field" $ do
            el "label" $ text "Import Type"
            dropdown initial (constDyn importTypeNames) $
                     def & attributes .~ constDyn ("class" =: "ui fluid dropdown")
          ie <- widgetHoldHelper (importDetails ps) initial (updated $ value v)
          si <- combineDyn (\nm e -> SnippetImport <$> nm <*> pure e) n $
                           joinDyn ie
      clk <- divClass "one wide field" $ do
        elDynHtml' "label" $ constDyn "&nbsp;"
        (e,_) <- elAttr' "button" ("class" =: "ui icon button") $
          elClass "i" "plus icon" $ blank
        return $ domEvent Click e
      return $ fmapMaybe id $ tagDyn si clk
  where
    mkAttrs PlainImport = "class" =: "twelve wide field"
    mkAttrs Qualified = "class" =: "fourteen wide field"
    mkAttrs _ = "class" =: "eight wide field"

importDetails
    :: MonadWidget t m
    => Dynamic t [Package]
    -> ImportType
    -> m (Dynamic t ImportExtra)
importDetails _ PlainImport = return (constDyn NoExtra)
importDetails _ Qualified = do
  divClass "two wide field" $ do
    el "label" $ text "As"
    mapDyn QualifiedName . value =<< textInput def
importDetails _ Hiding = do
  divClass "four wide field" $ do
    el "label" $ text "Hiding"
    v <- semUiDropdown "hiding-symbols" (head funcList) (constDyn $ mkMap funcList) $
        ("multiple" =: " " <> "class" =: "ui fluid dropdown")
    mapDyn (HidingSymbols . splitOn "," . filter (not . isSpace)) v
importDetails _ Explicit = do
  divClass "four wide field" $ do
    el "label" $ text "Symbols"
    v <- semUiDropdown "explicit-symbols" (head funcList) (constDyn $ mkMap funcList) $
             ("multiple" =: " " <> "class" =: "ui fluid dropdown")
    mapDyn (ExplicitSymbols . splitOn "," . filter (not . isSpace)) v

