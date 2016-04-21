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
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.String.Conv
import qualified Data.Text as T
import           GHCJS.DOM.Types hiding (Event, Text)
#ifdef ghcjs_HOST_OS
import           GHCJS.Types
#endif
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Contrib.Utils
import           Safe
------------------------------------------------------------------------------
import           HSnippet.FrontendState
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

data ImportsOut t = ImportsOut
    { ioutImports       :: Dynamic t [SnippetImport]
    , ioutRefresh       :: Event t ()
    , ioutModuleUpdates :: Event t Module
    }

------------------------------------------------------------------------------
importsWidget
    :: MonadWidget t m
    => FrontendState t
    -> m (ImportsOut t)
importsWidget fs = do
    divClass "ui small form" $ do
      (newImport, refreshImports, moduleUpdates) <- importDropdown fs
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
      sImports <- mapDyn (order . M.elems) imports
      return $ ImportsOut sImports refreshImports moduleUpdates


data ImportType = PlainImport
                | Qualified
                | Hiding
                | Explicit
  deriving (Eq,Ord,Show,Read)


importTypeNames :: Map ImportType String
importTypeNames = M.fromList
    [ (PlainImport, "plain")
    , (Qualified, "qualified")
    , (Hiding, "hiding")
    , (Explicit, "explicit")
    ]

funcList :: [String]
funcList = ["Maybe", "Either", "listToMaybe", "catMaybes"]

mkMap :: [String] -> Map String String
mkMap = M.fromList . map (\nm -> (nm, nm))

packageToModules :: Package -> [(Maybe String, String)]
packageToModules Package{..} = map (p . moduleName) packageModules
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
    let reactivate = leftmost [pb, () <$ updated vals]
    performEvent_ (liftIO (activateSemUiDropdown ('#':elId)) <$ reactivate)
    return $ value d

------------------------------------------------------------------------------
importDropdown
    :: MonadWidget t m
    => FrontendState t
    -> m (Event t SnippetImport, Event t (), Event t Module)
importDropdown FrontendState{..} = do
    let initial = PlainImport
    divClass "fields" $ do
      return (never, never, never)
      --rec attrs <- mapDyn mkAttrs $ value v
      --    (n, refresh) <- elDynAttr "div" attrs $ do
      --      clk <- el "label" $ do
      --        text "Module Name"
      --        (e,_) <- elAttr' "i" ("class" =: "refresh icon") blank
      --        return $ domEvent Click e
      --      mm <- mapDyn moduleMap fsPackages
      --      res <- semUiDropdown "import-search" Nothing mm
      --        ("class" =: "ui search dropdown")
      --      return (res, clk)
      --    es <- holdDyn M.empty $ attachWith getExports
      --            (current fsModuleExports) (fmapMaybe id $ updated n)
      --    v <- divClass "three wide field" $ do
      --      el "label" $ text "Import Type"
      --      dropdown initial (constDyn importTypeNames) $
      --               def & attributes .~ constDyn ("class" =: "ui fluid dropdown")
      --    ie <- widgetHoldHelper (importDetails (constDyn mempty)) initial (updated $ value v)
      --    --ie <- widgetHoldHelper (importDetails es) initial (updated $ value v)
      --    si <- combineDyn (\nm e -> SnippetImport <$> nm <*> pure e) n $
      --                     joinDyn ie
      --clk <- divClass "one wide field" $ do
      --  elDynHtml' "label" $ constDyn "&nbsp;"
      --  (e,_) <- elAttr' "button" ("class" =: "ui icon button") $
      --    elClass "i" "plus icon" blank
      --  return $ domEvent Click e
      --return (fmapMaybe id $ tagDyn si clk, refresh, Module . toS <$> fmapMaybe id (updated n))
  where
    mkAttrs PlainImport = "class" =: "twelve wide field"
    mkAttrs Qualified = "class" =: "fourteen wide field"
    mkAttrs _ = "class" =: "eight wide field"

getExports :: Map Module [Export] -> String -> Map String String
getExports mm m = M.fromList $ map (\n -> (n,n)) ns
  where
    ns = T.unpack . exportName <$> fromMaybe [] es
    es = M.lookup (Module $ T.pack m) mm


importDetails
    :: MonadWidget t m
    => Dynamic t (Map String String)
    -> ImportType
    -> m (Dynamic t ImportExtra)
importDetails _ PlainImport = return (constDyn NoExtra)
importDetails _ Qualified = do
  divClass "two wide field" $ do
    el "label" $ text "As"
    mapDyn QualifiedName . value =<< textInput def
importDetails exports Hiding = do
  divClass "four wide field" $ do
    el "label" $ text "Hiding"
    v <- semUiDropdown "hiding-symbols" "" exports $
        ("multiple" =: " " <> "class" =: "ui fluid dropdown")
    mapDyn (HidingSymbols . splitOn "," . filter (not . isSpace)) v
importDetails exports Explicit = do
  divClass "four wide field" $ do
    el "label" $ text "Symbols"
    v <- semUiDropdown "explicit-symbols" "" exports $
             ("multiple" =: " " <> "class" =: "ui fluid dropdown")
    mapDyn (ExplicitSymbols . splitOn "," . filter (not . isSpace)) v

