-- | Create bar process and manage notificaiton delivery.
-- NOTE:
--  It is important to note that XMonad is a single threaded
--  application and as the result it will block all threads
--  in foreign calls (mainly in Graphics.X11.XLib.Event.nextEvent),
--  so as the result we cannot use threads. That is why we keep
--  current message dispatcher in XState and deliver message notificaitons
--  via custom client messages to root window.
--
{-# LANGUAGE FlexibleContexts #-}
module XMonad.Powerbar.Bar (powerbar, getScreenSize) where

import Control.Concurrent    (forkIO, threadDelay)
import Control.DeepSeq       (($!!))
import Control.Exception     (finally, bracket)
import Control.Monad         (unless, when, join, forever, forM)
import Data.Function         (fix)
import Data.Monoid           (All(..), (<>))
import Graphics.X11.Xinerama (getScreenInfo)
import Text.Read             (readMaybe)
import System.IO             (hPutStrLn, hGetLine, hIsEOF, hClose
                             ,hSetEncoding, utf8
                             ,hSetBuffering, BufferMode(..))
import System.Posix.IO       (FdOption(..), setFdOption
                             ,fdToHandle, handleToFd)
import qualified System.Process      as P
import qualified Data.Traversable    as T
import qualified Graphics.X11.Xrandr as XR

import XMonad.Powerbar.Style
import XMonad.Powerbar.Lens
import XMonad.Powerbar.Async        (asyncHook)
import XMonad.Powerbar.View         (View(..))
import XMonad.Powerbar.Widgets      (sepRight, widgetUpdateAll)
import XMonad.Powerbar.Render       (renderX, renderIO)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Hooks.ManageDocks     (AvoidStruts, avoidStruts
                                    ,manageDocks)
import qualified XMonad                 as X
import qualified XMonad.Powerbar.Config as C


-- | Start power bar and use it inside closure
powerbar' :: C.Config -> (C.Config -> ((View, View, View) -> X.X ()) -> IO a) -> IO a
powerbar' confInitial body = do
  conf <- updateSizes confInitial
  let style    = runStyleQueryEmpty (conf ^. C.styleQuery) [(".bar", Null)]
      dzenProc = P.proc "dzen2"
                 ["-ta", "l"
                 ,"-fn", fontPattern style
                 ,"-fg", style ^. colorFg
                 ,"-bg", style ^. colorBg
                 ,"-h" , conf  ^. C.height . to show
                 ,"-w" , conf  ^. C.screenSize . to (show . \(w,_,_,_) -> w)
                 ,"-xs", show $ conf ^. C.screenIndex
                 ,"-e" , "" -- disable close on right-click, and other
                 ]
  -- start dzen toolbar (no need waitForProcess as SIGCHLD is masked)
  (Just hin', Just hout', _, _) <- P.createProcess $ dzenProc
                                  { P.std_in  = P.CreatePipe
                                  , P.std_out = P.CreatePipe
                                  }
  [hin, hout] <- forM [hin', hout'] $ \h' -> do
    -- make handles are closed on xmonad restart, and
    -- dzen bar is terminated
    fd <- handleToFd h'
    setFdOption fd CloseOnExec True
    h <- fdToHandle fd
    -- set utf8 encoding and line buffering
    hSetEncoding h utf8
    hSetBuffering h LineBuffering
    return h

  -- We have to use forkProcess instead of forkIO as XMonad is
  -- single threaded. Command is read from stdout of dzen bar
  -- parsed and sent as client message to XMonad.
  let sendEvent dpy win atom code =
          do X.allocaXEvent $ \ev ->
                 do X.setEventType ev X.clientMessage
                    X.setClientMessageEvent ev win atom 32
                         (fromIntegral code) X.currentTime
                    X.sendEvent dpy win False X.structureNotifyMask ev
             X.sync dpy False
  X.xfork $ bracket (X.openDisplay "") X.closeDisplay $ \dpy ->
      do hClose hin
         root <- X.rootWindow dpy $ X.defaultScreen dpy
         atom <- X.internAtom dpy "XMONAD_POWERBAR" False
         -- send wake ups
         forkIO . forever $ do
           threadDelay (fromEnum . (* 1000000) $ conf ^. C.pollDelay)
           sendEvent dpy root atom (4096 :: Integer)
         -- send events received from dzen
         fix $ \self ->
             do eof <- hIsEOF hout
                unless eof $ do
                  cmdMaybe <- readMaybe <$> hGetLine hout :: IO (Maybe Int)
                  case cmdMaybe of
                    Just cmd -> sendEvent dpy root atom cmd
                    Nothing -> return ()
                  self
  hClose hout
  let attrs = VAttr ".bar-height" . toJSON $ conf ^. C.height
      commit (l, m, r) = do
        text <- renderX (attrs l, attrs m, attrs r)
        X.io . hPutStrLn hin $!! text
  body conf commit `finally` hClose hin


{- | Start powerbar and pass modified powerbar config, xmonad config and
     commit hook where xmonad supposed to be started.

Usage example
> main :: IO ()
> main = powerbar C.defaultConfig X.defaultConfig $ \pconf xconf render ->
>           xmonad $ xconf {
>               -- my xmonad settings
>           }
-}
powerbar :: X.LayoutClass l X.Window
         => C.Config
         -> X.XConfig l
         -> (C.Config -> X.XConfig (ModifiedLayout AvoidStruts l)
                     -> ((View, View, View) -> X.X ()) -> IO a)
         -> IO a
powerbar pbConf xConf body =
    powerbar' pbConf $ \pbConf' commit ->
        let startupHook = C.config .= pbConf' >> widgetUpdateAll
            xConf' = xConf
                     { X.layoutHook      = avoidStruts (X.layoutHook xConf)
                     , X.handleEventHook = eventHook <> asyncHook <> X.handleEventHook xConf
                     , X.manageHook      = manageDocks <> X.manageHook xConf
                     , X.startupHook     = startupHook <> X.startupHook xConf
                     }
        in body pbConf' xConf' commit


-- | Powerbar actions handler
eventHook :: X.Event -> X.X All
eventHook (X.ClientMessageEvent {X.ev_message_type = mt , X.ev_data = dt})
    = do atom <- X.getAtom "XMONAD_POWERBAR"
         when (mt == atom && dt /= []) $
              do let action = C.action . fromIntegral . head $ dt
                 X.userCodeDef () action
         return $ All True
eventHook _ = return $ All True


-- | Update calculated sizes (bar height, screen resolution/multiplier)
updateSizes :: C.Config -> IO C.Config
updateSizes conf = do
  (_, _, h) <- renderIO (conf ^. C.styleQuery <> left <> right) sep
  size@(wpx, _, wmm, _) <- getScreenSize
  return $ conf & C.height     .~ h
                & C.screenSize .~ size
                & C.screenMult .~ if wmm < 0 then 1 else wpx `div` min wpx (wmm * 4)
    where
      left  = do matchAttr ".sep-hight-left"
                 colorBg .= "#000000"
                 colorFg .= "#ffffff"
      right = do matchAttr ".sep-hight-right"
                 colorBg .= "#ffffff"
                 colorFg .= "#000000"
      sep   = VAttr ".bar" Null
              $ mconcat [ VAttr ".sep-hight-left" Null (VText " ")
                        , sepRight
                        , VAttr ".sep-hight-right" Null (VText " ")]


-- | Get primary output xrandr info
-- Returns sceen size in pixels and in mm
getScreenSize :: IO (Int, Int, Int, Int)
getScreenSize =
    bracket (X.openDisplay "") X.closeDisplay $ \dpy -> do
      root <- X.rootWindow dpy $ X.defaultScreen dpy
      xrandr <- xrandrInfo dpy root
      case xrandr of
        Just info -> return info
        Nothing   -> x11Info dpy
    where
      -- xrandr based screen info
      xrandrInfo :: X.Display -> X.Window -> IO (Maybe (Int, Int, Int, Int))
      xrandrInfo dpy root = do
        X.xSetErrorHandler
        out  <- XR.xrrGetOutputPrimary dpy root
        res  <- XR.xrrGetScreenResources dpy root
        info <- fmap join . T.sequence $
               XR.xrrGetOutputInfo dpy <$> res <*> Just out
        crtc  <- fmap join . T.sequence $
                XR.xrrGetCrtcInfo dpy <$> res <*> (XR.xrr_oi_crtc <$> info)
        let width     = fromIntegral . XR.xrr_ci_width     <$> crtc
            height    = fromIntegral . XR.xrr_ci_height    <$> crtc
            width_mm  = fromIntegral . XR.xrr_oi_mm_width  <$> info
            height_mm = fromIntegral . XR.xrr_oi_mm_height <$> info
        return $ (,,,) <$> width <*> height <*> width_mm <*> height_mm
      -- x11 based info
      x11Info :: X.Display -> IO (Int, Int, Int, Int)
      x11Info dpy = do
                  screen <- head <$> getScreenInfo dpy
                  return (fromIntegral . X.rect_width $ screen,
                          fromIntegral . X.rect_height $ screen,
                          -1, -1)
