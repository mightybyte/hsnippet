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
import           ACE
import           HSnippet.FrontendState
import           HSnippet.Tabs
import           HSnippet.Shared.Types.ExampleSnippet
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
    => Dynamic t [Package]
    -> Event t ExampleSnippet
    -> Event t (Int,Int)
    -> m (Snippet t)
leftColumn pkgs newExample pos = do
    divClass "left column full-height" $ do
      curTab <- divClass "ui top attached menu" $ do
        tabBar CodeTab [CodeTab, ImportsTab] never never
      res <- tabPane tabAttrs curTab CodeTab $ codeInput newExample pos
      tabPane tabAttrs curTab ImportsTab (importsWidget pkgs)
      return res
  where
    tabAttrs = "class" =: "ui bottom attached segment" <>
               "style" =: "height: calc(100% - 40px)"


codeInput
    :: MonadWidget t m
    => Event t ExampleSnippet
    -> Event t (Int,Int)
    -> m (Snippet t)
codeInput newExample pos = do
    ace <- elClass "form" "ui form full-height" $ do
      elAttr "div" ("style" =: "height: 100%") $ do
        elAttr "div" ("class" =: "field") $ do
          ace <- aceWidget example
          divClass "ace-results" $ dynText $ aceValue ace
          aceMoveCursor ace pos
          aceSetValue ace (exampleCode <$> newExample)
          return ace
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
      rec imports <- foldDyn ($) M.empty $ leftmost
                       [ addNew <$> newImport
                       , M.delete <$> delImport
                       ]
          res <- divClass "ui list" $ listViewWithKey imports $ \k iDyn -> do
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
    [ (PlainImport, "")
    , (Qualified, "qualified")
    , (Hiding, "hiding")
    , (Explicit, "explicit")
    ]

tempModuleList :: [String]
tempModuleList =
    [ "Control.Lens"
    , "Control.Monad"
    , "Data.Aeson"
    , "Data.Aeson.TH"
    , "Data.Colour.Palette.BrewerSet"
    , "Data.Map"
    , "Data.String.Conv"
    , "Data.Text"
    , "Data.Monoid"
    , "Diagrams.Backend.Reflex"
    , "Diagrams.Prelude"
    , "GHC.Generics"
    , "Reflex"
    , "Reflex.Dom"
    , "Reflex.Dom.Contrib.KeyEvent"
    , "Reflex.Dom.Contrib.Pagination"
    , "Reflex.Dom.Contrib.Utils"
    , "Reflex.Dom.Contrib.Widgets.EditInPlace"
    , "Reflex.Dom.Contrib.Widgets.Svg"
    , "Reflex.Dom.Contrib.Xhr"
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

testMap :: Map (Maybe String) String
testMap = M.fromList $ d : map (\n -> (Just n, n)) tempModuleList
  where
    d = (Nothing, "")

-- | Wrapper around the reflex-dom dropdown that calls the sem-ui dropdown
-- function after the element is built.
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

