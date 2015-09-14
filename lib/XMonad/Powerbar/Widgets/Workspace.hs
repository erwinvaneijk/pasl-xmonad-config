{-# LANGUAGE OverloadedStrings #-}
module XMonad.Powerbar.Widgets.Workspace
    ( layoutWidget    , layoutQuery
    , titleWidget     , titleQuery
    , workspacesWidget, workspacesQuery
    ) where

import XMonad.Powerbar.View
import XMonad.Powerbar.Style
import XMonad.Powerbar.Lens
import XMonad.Powerbar.Widgets.Common

import Data.Maybe                   (isJust)
import Data.Monoid                  ((<>))
import Data.Text                    (Text)
import XMonad.Powerbar.Widgets.Icon (iconCell)
import XMonad.Util.NamedWindows     (getName)
import XMonad.Util.WorkspaceCompare (getSortByIndex, getSortByXineramaPhysicalRule)
import qualified XMonad          as X
import qualified XMonad.StackSet as XS


--------------------------------------------------------------------------------
-- Layout
--
layoutWidget :: X.X View
layoutWidget = do
  layout <- X.description . XS.layout
         . XS.workspace . XS.current . X.windowset <$> X.get
  return $ VAction VButtonLeft (X.sendMessage X.NextLayout)
         . VAttr ".layout" (toJSON layout)
         $ textSpaced layout

layoutQuery :: StyleQuery ()
layoutQuery = do layout <- matchAttr' ".layout" _String
                 colorBg .= blue
                 let i name = do
                       fontSize += 2
                       iconCell name
                 case layout of
                   "Tall"                    -> i "octicon-mirror"
                   "Mirror Tall"             -> i "octicon-unfold"
                   "Full"                    -> i "octicon-screen-full"
                   "Simple Float"            -> i "octicon-browser"
                   "DefaultDecoration Float" -> i "octicon-browser"
                   _                         -> return ()


--------------------------------------------------------------------------------
-- Title
--
titleWidget :: X.X View
titleWidget = do
  window <- XS.peek . X.windowset <$> X.get
  titleMaybe <- maybe (return Nothing) (fmap (Just . show) . getName) window
  return $ case titleMaybe of
             Nothing    -> VEmpty
             Just title ->
                 let titleView = VAction VButtonScrollUp (X.windows XS.focusUp) .
                                 VAction VButtonScrollDown (X.windows XS.focusDown) .
                                 textSpaced . trim 40 $ title
                     closeView = VAction VButtonLeft X.kill .
                                 VAttr ".title-close" Null $
                                 VText " [X] "
                 in VAttr ".title" (toJSON title) (titleView <> closeView)
        where trim :: Int -> String -> String
              trim _     []     = []
              trim 0     _      = "..."
              trim count (c:cs) = c : trim (count - 1) cs

titleQuery :: StyleQuery ()
titleQuery = applyFirst [ do matchAttr ".title"
                             fontFamily .= "Ge Inspira"
                             colorFg .= whiteL
                             colorBg .= blackL
                        , do matchAttr ".title-close"
                             fontSize += 2
                             colorFg .= redL
                             iconCell "ion-close"
                        ]


--------------------------------------------------------------------------------
-- Workspaces
--
workspacesWidget :: X.X View
workspacesWidget = do
  ws <- X.gets X.windowset
  let -- visible workspaces
      visible = XS.tag . XS.workspace <$> XS.visible ws
      -- all workspaces
      workspaces = map XS.workspace (XS.current ws:XS.visible ws) ++ XS.hidden ws
      -- convert workspace to view
      wType w | XS.tag w == XS.currentTag ws = "focused" :: Text
              | XS.tag w `elem` visible     = "visible"
              | isJust (XS.stack w)         = "unfocused"
              | otherwise                   = "invisible"
      wView w = VAction VButtonLeft (X.windows $ XS.greedyView . XS.tag $ w)
              . VAttr ".workspace-type" (toJSON . wType $ w)
              . VAttr ".workspace"      (toJSON . XS.tag $ w)
              . textSpaced
              . XS.tag $ w
  sort <- case visible of
           []  -> getSortByIndex
           _   -> getSortByXineramaPhysicalRule
  return $ VAttr ".workspaces" "" . mconcat . fmap wView . sort $ workspaces

workspacesQuery :: StyleQuery ()
workspacesQuery = do
  tupe <- matchAttr ".workspace-type"
  colorFg .= whiteL
  colorBg .= blackL
  case tupe of
    "invisible" -> replace VEmpty
    "visible"   -> colorBg .= green
    "focused"   -> colorBg .= greenL
    _           -> return ()
