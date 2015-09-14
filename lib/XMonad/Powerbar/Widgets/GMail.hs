{-# LANGUAGE OverloadedStrings, TupleSections, DeriveDataTypeable #-}
module XMonad.Powerbar.Widgets.GMail
    ( gmailMessages
    , gmailMessagesCached
    , gmailWidget
    , gmailQuery
    ) where

import XMonad.Powerbar.Lens
import XMonad.Powerbar.View
import XMonad.Powerbar.Async
import XMonad.Powerbar.Style
import XMonad.Powerbar.Widgets.Common

import Control.Monad                (when)
import Control.Monad.Trans          (lift)
import Control.Monad.IO.Class       (MonadIO)
import Data.Maybe                   (fromMaybe)
import Data.Monoid                  ((<>))
import Data.Typeable                (Typeable)

import qualified Data.Map as M
import qualified XMonad   as X


type Messages = Either String [M.Map String String]

data GMailState = GMailState
    { _needUpdate :: Bool
    , _messages   :: Messages
    } deriving (Typeable, Show)


needUpdate :: Lens' GMailState Bool
needUpdate f v = (\c -> v {_needUpdate = c}) `fmap` f (_needUpdate v)
messages :: Lens' GMailState Messages
messages f v = (\c -> v {_messages = c}) `fmap` f (_messages v)


-- | Initial gmail state
mkGMailState :: GMailState
mkGMailState = GMailState True (Right [])


-- | Check gmail with external programm gmail-check must be installed in PATH
gmailMessages :: (Functor m, MonadIO m) => String -> String -> m Messages
gmailMessages login pass = parse <$> execute "gmail-check" [login, pass] ""
    where
      parse :: Maybe String -> Messages
      parse res = fromMaybe (Left "Failed to parse gmail-check output")
                  $ res >>= firstOf _JSON

-- | Get cache gmail messages
gmailMessagesCached :: X.X Messages
gmailMessagesCached =
    X.gets $ \st ->
    fromMaybe (Left "Not cached") (st ^? widgetState' "GMail" mkGMailState . messages)

-- | Widget that shows number of unread messages from gmail
-- takes login and password as its arguments.
gmailWidget :: String -> String -> X.X View
gmailWidget login pass =
    widget name 120 gmailQuery mkGMailState $ do
      -- check if update is needed
      u <- needUpdate %%= (,True)
      when u . lift . lift $ update
      -- construct view
      m <- use messages
      case m ^? _Left of
        Just err -> X.io . putStrLn $ "[powerbar] gmail-check failed: " ++ err
        Nothing  -> return ()
      return . VAction VButtonLeft (X.spawn "chromium https://mail.google.com")
             . VAction VButtonRight update
             . VAttr ".gmail" (toJSON m)
             $ VText (show . length $ m ^. _Right)
    where
      name = "GMail"
      state :: Traversal' X.XState GMailState
      state = widgetState' name mkGMailState
      -- update widget state with email checker results
      update :: X.X ()
      update = asyncRun $ do
                 state . messages <~ async (gmailMessages login pass)
                 state . needUpdate .= False
                 lift $ widgetUpdate name


gmailQuery :: StyleQuery ()
gmailQuery = do res <- matchAttr ".gmail"
                let msgs :: Maybe Messages
                    msgs = res ^? _JSON
                    count = length (msgs ^. _Just . _Right)
                case fromMaybe (Left "gmail attr parse error") msgs of
                  Right [] -> do
                    colorFg .= blackL
                    replace iconNone
                  Right _ -> replace $ icon <> VText (show count)
                  Left  _ -> do
                    colorFg .= redL
                    replace iconNone
    where
      icon     = VAttr ".icon" "octicon-mail-read" VEmpty
      iconNone = VAttr ".icon" "octicon-mail" VEmpty

