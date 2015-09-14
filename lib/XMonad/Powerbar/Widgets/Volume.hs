{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TemplateHaskell #-}
module XMonad.Powerbar.Widgets.Volume
    ( getVolume
    , volumeWidget
    , volumeQuery
    , volumeChange
    , volumeToggleMute
    , VolumeState
    , sink
    , muted
    , baseVolume
    , curVolume
    , volumeState
    ) where

import XMonad.Powerbar.View
import XMonad.Powerbar.Style
import XMonad.Powerbar.Lens
import XMonad.Powerbar.Widgets.Common
import XMonad.Powerbar.Widgets.Icon (iconCell)

import Data.Aeson.TH
import Control.Monad       (liftM, when)
import Data.Function       (fix)
import Data.Maybe          (fromMaybe)
import Data.Monoid         ((<>))
import Data.Typeable       (Typeable)
import Text.Printf         (printf)
import Control.Monad.IO.Class (MonadIO)

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as C
import qualified XMonad as X


-- | Get curret volume (parse pacmd out - very flimsy way to check volume)
getVolume :: MonadIO m => m (Maybe VolumeState)
getVolume = do
  info <- (C.pack . fromMaybe "") `liftM` execute "pacmd" [] "info\n"
  return $ either (const Nothing) Just $ A.parseOnly parseVolume info
  where parseVolume :: A.Parser VolumeState
        parseVolume = do
              -- determine default sink name
              seekName "Default sink name"
              sinkName <- line
              let sinkFull = "<" <> sinkName <> ">"
              -- seek to default sink block
              fix $ \self -> do
                        seekName "name"
                        sinkFull' <- line
                        when (sinkFull /= sinkFull') self
              -- "volume: front-left: 32770 / 50% / -18.06 dB"
              seekName "volume"
              A.skipWhile $ not . A.isDigit
              vol <- A.decimal
              -- "base volume: 65536 / 100% / 0.00 dB"
              seekName "base volume"
              baseVol <- A.decimal
              -- muted
              seekName "muted"
              mutedTag <- line
              return $ VolumeState (C.unpack sinkName) (mutedTag == "yes") baseVol vol

        -- Rest of the line
        line :: A.Parser C.ByteString
        line = A.takeWhile (/= '\n') <* A.option '\n' (A.char '\n')

        -- Seek property with specified name
        seekName :: C.ByteString -> A.Parser ()
        seekName name = do
              A.skipWhile $ A.inClass " \t"
              name' <- A.takeTill $ A.inClass "\n:"
              sep   <- A.anyChar
              if sep /= ':' || name' /= name
              then seekName name
              else A.skipWhile $ A.inClass " \t"


data VolumeState = VolumeState { _sink       :: String
                               , _muted      :: Bool
                               , _baseVolume :: Int
                               , _curVolume  :: Int
                               } deriving (Show, Read, Typeable)

instance Monoid VolumeState where
    mempty      = VolumeState "" False 0 0
    mappend _ s = s

$(deriveJSON defaultOptions ''VolumeState)

volumeName :: String
volumeName = "Volume"

volumeState :: Traversal' X.XState VolumeState
volumeState = widgetState' volumeName mempty

sink :: Lens' VolumeState String
sink f v = (\c -> v {_sink = c}) `fmap` f (_sink v)
muted :: Lens' VolumeState Bool
muted f v = (\c -> v {_muted = c}) `fmap` f (_muted v)
baseVolume :: Lens' VolumeState Int
baseVolume f v = (\c -> v {_baseVolume = c}) `fmap` f (_baseVolume v)
curVolume :: Lens' VolumeState Int
curVolume f v = (\c -> v {_curVolume = c}) `fmap` f (_curVolume v)

-- | Show current volume of default sink
volumeWidget :: X.X View
volumeWidget =
    widget volumeName 60 volumeQuery mempty $
           do st <- widgetMaybe getVolume
              id .= st
              return $ VAction VButtonScrollUp   (volumeChange 10)
                     . VAction VButtonScrollDown (volumeChange (-10))
                     . VAction VButtonLeft       volumeToggleMute
                     . VAttr ".volume" (toJSON st)
                     $ VText (show st)


volumeQuery :: StyleQuery ()
volumeQuery =
    do state <- matchAttr' ".volume" _JSON :: StyleQuery VolumeState
       let volume   = if state ^. muted
                      then 0
                      else state ^. curVolume * 100 `div`
                                     state ^. baseVolume
           name v | v > 50 = "-up"
                  | v > 25 = "-down"
                  | v > 0  = ""
                  | v == 0  = "-mute"
                  | otherwise = "-up"
       fontSize += 4
       iconCell $ "typcn-volume" ++ name volume


-- | Change by specfied percent from base volume
volumeChange :: Int -> X.X ()
volumeChange delta = do
  st <- use volumeState
  let vol  = st ^. curVolume + (st ^. baseVolume * delta `div` 100)
      cmds :: [String]
      cmds = [ if st ^. muted
               then printf "set-sink-mute %s false\n" (st ^. sink)
               else ""
             , printf "set-sink-volume %s %d\n" (st ^. sink) (max vol 0)
             , printf "play-sample audio-volume-change %s\n" (st ^. sink)
             ]
  execute "pacmd" [] $ mconcat cmds
  widgetUpdate volumeName


-- | Toggle muted state on default sink
volumeToggleMute :: X.X ()
volumeToggleMute = do
  st <- use volumeState
  let cmds = [ printf "set-sink-mute %s %s\n"
                      (st ^. sink) (show . not . (^. muted) $ st)
             , printf "play-sample audio-volume-change %s\n" (st ^. sink)
             ]
  execute "pacmd" [] $ mconcat cmds
  widgetUpdate volumeName
