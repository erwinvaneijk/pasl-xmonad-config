module XMonad.Powerbar.Defaults
    ( defaultConfig
    , defaultWidget
    , defaultQuery
    ) where

import XMonad.Powerbar.Lens
import XMonad.Powerbar.View
import XMonad.Powerbar.Style
import XMonad.Powerbar.Widgets
import XMonad.Powerbar.Config

import Data.Monoid  ((<>))
import Data.Text    (unpack)
import qualified XMonad as X


-- | Default powerbar configuration
defaultConfig :: Config
defaultConfig = emptyConfig & styleQuery .~ defaultQuery


-- | Default set of widgets
defaultWidget :: AsValue c => c -> X.X (View, View, View)
defaultWidget cfg = do
  left <- VAttr ".left-bar" Null . sepRightMany <$> sequence
         [ workspacesWidget
         , layoutWidget
         , titleWidget]
  middle <- VAttr ".middle-bar" Null . mconcat <$> sequence
           [ clockWidget ]
  right <- widgetsFromConfig cfg >>= folder ".right-bar"
  return (left, middle, sepLeft <> VAttr ".right-bar" Null right)


-- | Default query
defaultQuery :: StyleQuery ()
defaultQuery =
    applyFirst [ do matchAttr ".bar"
                    colorFg    .= whiteL
                    colorBg    .= black
                    fontSize   .= 8
                    fontFamily .= "Pragmata Pro"
               , do name <- matchAttr' ".icon" (_String . to unpack)
                    fontSize += 2
                    iconCell name
               , sepQuery
               , workspacesQuery
               , layoutQuery
               , titleQuery
               , clockQuery
               , folderQuery
               ]
