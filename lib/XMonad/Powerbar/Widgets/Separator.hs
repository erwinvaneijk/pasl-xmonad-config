{-# LANGUAGE RankNTypes, LambdaCase, DeriveDataTypeable #-}
module XMonad.Powerbar.Widgets.Separator
    ( sepLeft
    , sepLeftMany
    , sepRight
    , sepRightMany
    , sepQuery
    , folder
    , foldState
    , folderQuery
    , foldQuery
    ) where

import XMonad.Powerbar.Lens
import XMonad.Powerbar.View
import XMonad.Powerbar.Style
import XMonad.Powerbar.Config (extState)
import XMonad.Powerbar.Widgets.Common

import Control.Monad (when)
import Data.Monoid   ((<>))
import Data.Typeable (Typeable)
import XMonad.Powerbar.Widgets.Icon (icon)
import qualified XMonad as X
import qualified Data.Map as M
import qualified Data.Text as T


-- | Left separator
sepLeft :: View
sepLeft = VAttr ".sep-left" Null (VText "<")

-- | Left separator for many views
sepLeftMany :: Foldable f => f View -> View
sepLeftMany = foldMap withSep
    where withSep VEmpty = VEmpty
          withSep v      = sepLeft <> v

-- | Right seperator
sepRight :: View
sepRight = VAttr ".sep-right" Null (VText ">")

-- | Left separator for many views
sepRightMany :: Foldable f => f View -> View
sepRightMany = foldMap withSep
    where withSep VEmpty = VEmpty
          withSep v      = v <> sepRight

-- | Separators query
sepQuery :: StyleQuery ()
sepQuery = applyFirst [right, rightEnd, left, leftEnd]
    where
      -- capture background color and intent to render right separator
      right = do matchAttr ".sep-right"
                 bubble
                 bg <- (^. colorBg) <$> leftStyle
                 replace $ VAttr ".sep-right-end" (toJSON bg) VEmpty
      -- render right separator
      rightEnd = do leftBg  <- matchAttr' ".sep-right-end" (_String . to T.unpack)
                    rightBg <- (^. colorBg) <$> rightStyle
                    if leftBg == rightBg
                    then do
                      colorFg <~ (^. colorFg) <$> rightStyle
                      colorBg .= rightBg
                      icon "power-right-bracket"
                    else do
                      colorFg .= leftBg
                      colorBg .= rightBg
                      fontSize += 3
                      icon "power-right"
      -- capture background color and intent to render left sepearator
      left = do matchAttr ".sep-left"
                bubble
                bg <- (^. colorBg) <$> leftStyle
                replace $ VAttr ".sep-left-end" (toJSON bg) VEmpty
      -- render left separator
      leftEnd = do leftBg  <- matchAttr' ".sep-left-end" (_String . to T.unpack)
                   rightBg <- (^. colorBg) <$> rightStyle
                   if leftBg == rightBg
                   then do
                     colorFg <~ (^. colorFg) <$> rightStyle
                     colorBg .= rightBg
                     icon "power-left-bracket"
                   else do
                     colorFg  .= rightBg
                     colorBg  .= leftBg
                     fontSize += 3
                     icon "power-left"


--------------------------------------------------------------------------------
-- Make foldable view
--

newtype FolderState = FolderState {_fs :: M.Map String (M.Map Int Bool)
                                  } deriving Typeable

folderState :: String -> Lens' FolderState (M.Map Int Bool)
folderState name = lens _fs (\a b -> a {_fs = b}) . at name . non M.empty

instance X.ExtensionClass FolderState where
    initialValue = FolderState M.empty


-- | List of items that can be folded/unfolded by left click
folder :: String   -- fodler name
       -> [View]   -- child views
       -> X.X View
folder name vs =
    flip foldMap (zip [0..] vs) . wrap <$> use (extState . folderState name)
    where
      -- compose views
      wrap :: M.Map Int Bool -> (Int, View) -> View
      wrap _  (_, VEmpty) = VEmpty
      wrap fs (i, v)      =
          let left  = fs ^. at (i-1) . non False
              right = fs ^. at i     . non False
              sep   = VAttr ".folder-sep" (toJSON (left, right, i, name)) VEmpty
              item  = VAttr ".folder-item" (toJSON (right, i, name))
                    . VAction VButtonLeft (foldState name i %= not) $ v
          in VAttr ".folder" (toJSON name) $ (if i == 0 then id else (sep <>)) item


-- | If we are located in folded item
foldQuery :: StyleQuery Bool
foldQuery = do foldItem <- foldItemQ
               return $ case foldItem of
                          Nothing           -> False
                          Just (True, _, _) -> True
                          _                 -> False
    where foldItemQ :: StyleQuery (Maybe (Bool, Int, String))
          foldItemQ = lookupAttr' ".folder-item" (_JSON . to Just) <|> return Nothing


foldState :: String -> Int -> Traversal' X.XState Bool
foldState name index = extState . folderState name . at index . non False


-- | Default folder query
folderQuery :: StyleQuery ()
folderQuery = applyFirst [ do matchAttr ".folder"
                              colorBg .= black
                         , do Just (left, right, _, _) <- foldSepQ
                              replace $ case (left, right) of
                                  (False, False) -> VAttr ".folder-ff" Null VEmpty
                                  (True, True)   -> sepLeft <> VAttr ".folder-tt" Null (VText "")
                                  _              -> sepLeft
                         , do matchAttr ".folder-tt"
                              colorFg .= black
                              colorBg .= blackL
                         , do matchAttr ".folder-ff"
                              colorBg .= black
                              width <~ Just . (`div` 4) <$> cellHeight
                         , do f <- foldQuery
                              when f $ colorBg .= blackL
                         ]
    where foldSepQ :: StyleQuery (Maybe (Bool, Bool, Int, String))
          foldSepQ = firstOf _JSON <$> matchAttr ".folder-sep"
