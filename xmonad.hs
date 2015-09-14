{-# LANGUAGE OverloadedStrings, LambdaCase, DeriveDataTypeable, TupleSections #-}
module Main (main) where

import Control.Concurrent    (threadDelay)
import Control.Monad         (join)
import Control.Monad.Trans   (lift)
import Data.Char             (chr)
import Data.List             (sort)
import Data.Maybe            (fromMaybe)
import Data.Monoid           ((<>))
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Text             (unpack)
import Data.String           (fromString)
import System.Directory      (getHomeDirectory, doesFileExist)
import System.Environment    (getEnv, setEnv, getEnvironment)
import System.FilePath       ((</>))
import System.Posix.Unistd   (getSystemID, nodeName)
import Text.Printf           (printf)
import qualified Control.Monad.Trans.State  as ST
import qualified Data.Aeson                 as A
import qualified Data.Map                   as M
import qualified Data.Foldable              as T
import qualified Data.Traversable           as T
import qualified Data.Text                  as TX
import qualified Data.HashMap.Strict        as HM
import qualified Data.ByteString.Lazy.Char8 as B

import XMonad.Powerbar
import XMonad hiding (XConfig(..), defaultConfig, config)
import qualified XMonad as X
import qualified XMonad.StackSet as W

import XMonad.Hooks.EwmhDesktops        (ewmh)
import XMonad.Hooks.ManageHelpers       (transience')
import XMonad.Hooks.SetWMName           (setWMName)
import XMonad.Layout.Decoration         (Theme(..), defaultTheme, shrinkText)
import XMonad.Layout.DecorationMadness  (floatDefault)
import XMonad.Layout.NoBorders          (smartBorders)
import XMonad.Layout.PerWorkspace       (onWorkspace)
import XMonad.Util.EZConfig             (removeKeysP, additionalKeysP)
import XMonad.Util.Run                  (safeSpawn)
import XMonad.Util.NamedWindows         (getName)


--------------------------------------------------------------------------------
-- Constants
--
-- workspaces
workspaces :: [String]
workspaces = ["alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta", "iota"]

workspacesShort :: M.Map String String
workspacesShort = M.fromList $ zip workspaces [[chr c] | c <- [945..]]


-- | Traverse all string values inside JSON value
traverseStrings :: Traversal' A.Value A.Value
traverseStrings _ A.Null         = pure A.Null
traverseStrings _ b@(A.Bool _)   = pure b
traverseStrings _ n@(A.Number _) = pure n
traverseStrings f s@(A.String _) = f s
traverseStrings f a@(A.Array  _) = (_Array  . traverse . traverseStrings) f a
traverseStrings f o@(A.Object _) = (_Object . traverse . traverseStrings) f o


-- | My settings
mkSettings :: IO A.Value
mkSettings = do
  -- read settings from settings.json
  file <- (</> "settings.json") <$> getXMonadDir
  exists <- doesFileExist file
  input  <- if not exists then return . A.object $ []
         else fromMaybe (A.object []) . firstOf _Value <$> readFile file
  -- set vars
  vars <- flip ST.execStateT (fromMaybe (A.object []) . firstOf (key "vars") $ input) $
         do -- colors
            T.forM (M.toList colors) $ \(name, color) ->
                fromString ("color-" ++ name) .= color
            -- host specific
            host <- nodeName <$> X.io getSystemID
            "host-name" .= host
            case host of
              "fiend"   ->
                  do "dpi"     .= (227 :: Int)
                     "browser" .= ("chromium" :: String)
              "banshee" ->
                  do "dpi"       .= (101 :: Int)
                     "font-size" .= (10  :: Int)
              _         -> return ()
            -- dmenu
            fontSize <- gets (fromMaybe (8 :: Int) . firstOf "font-size")
            fontName <- gets (fromMaybe ("monospace" :: String) . firstOf "font-name")
            "dmenu-font" .= (printf "%s-%d" fontName fontSize :: String)
  let varsTable = fromMaybe HM.empty $ firstOf _Object vars
  return $ input & traverseStrings %~ \val ->
      case val of
        A.String var -> if TX.length var > 0 && TX.head var /= '$' then val
                       else fromMaybe val $ HM.lookup (TX.drop 1 var) varsTable
        _ -> val

newtype Settings = Settings { _settings :: A.Value } deriving (Typeable)

instance X.ExtensionClass Settings where
    initialValue = Settings (A.object [])

settings :: Lens' X.XState A.Value
settings = extState . cfg
    where cfg :: Lens' Settings A.Value
          cfg f v = (\c -> v {_settings = c}) `fmap` f (_settings v)


--------------------------------------------------------------------------------
-- Main
--
main :: IO ()
main = do
  hConf <- mkSettings
  -- start powerbar and xmonad
  powerbar (powerConfig hConf) X.defaultConfig $ \pbConf xConf commit ->
      xmonad . ewmh $ xConf
                    { X.borderWidth = 2 * (fromIntegral . (^. screenMult) $ pbConf)
                    , X.normalBorderColor  = black
                    , X.focusedBorderColor = green
                    , X.modMask     = mod4Mask
                    , X.terminal    = "gnome-terminal"
                    , X.workspaces  = workspaces
                    -- hooks
                    , X.layoutHook  = smartBorders
                                    . onWorkspace "alpha" (floatDefault shrinkText (decoTheme pbConf))
                                    $ X.layoutHook xConf
                    , X.logHook     = X.logHook xConf     <> logHook commit
                    , X.manageHook  = X.manageHook xConf  <> manageHook
                    , X.startupHook = X.startupHook xConf <> startupHook hConf
                    } `overrideKeys` (keysAdd, keysRemove)


-- | Decoration theme
decoTheme :: Config -> Theme
decoTheme pbConf = defaultTheme { fontName            = "xft:Ge Inspira:size=8"
                                , activeColor         = green
                                , activeBorderColor   = greenL
                                , inactiveColor       = black
                                , inactiveBorderColor = blackL
                                , decoHeight          = pbConf ^. height . to fromIntegral
                                }


-- | Powerbar config
powerConfig :: A.Value -> Config
powerConfig hConf = defaultConfig & styleQuery <>~ powerQuery
                                  & setFontSize
                                  & screenIndex .~ 2
    where
      -- set fontsize from host config
      setFontSize :: Config -> Config
      setFontSize = case hConf ^? key "font-size" . _Int of
                      Nothing   -> id
                      Just size -> styleQuery <>~ (matchAttr ".bar" >> fontSize .= size)
      -- my style overrides
      powerQuery :: StyleQuery ()
      powerQuery = applyFirst
                   [ do name  <- matchAttr' ".workspace" (_String . to unpack)
                        cell
                        if name == "alpha"
                        then do
                          fontSize += 2
                          icon "octicon-browser"
                        else T.forM_ (M.lookup name workspacesShort) (replace . VText)
                   , do lang <- matchAttr' ".ibus" (_String . to unpack)
                        colorBg .= blackL
                        colorFg .= if lang == "RU" then redL else greenL
                        fontSize += 3
                        iconCell "typcn-flag"
                   , do matchAttr ".clock-time"
                        fontFamily .= "Cuprum"
                   , do matchAttr ".right-bar"
                        fontFamily .= "Cuprum"
                   , do opened <- matchAttr "fold"
                        case opened of
                          "True" -> colorBg .= red
                          _      -> colorBg .= green
                   ]


-- | Powerbar rendering hook
logHook :: ((View, View, View) -> X.X ()) -> X.X ()
logHook commit = timerProbe $
  do widgetsConfig <- fromMaybe (A.object []) <$> gets (firstOf (settings . key "widgets"))
     -- timer <- timerView
     (left, middle, right) <- defaultWidget widgetsConfig
     commit (left, middle, right {-<> timer-})


-- | Initialize gnome session & compton
startupHook :: A.Value -> X.X ()
startupHook hConf = do
  -- install settings
  settings .= hConf
  -- mark first widget (ibus) as unfolded
  foldState ".right-bar" 0 .= True
  -- java applications workaround
  setWMName "LG3D"
  X.io $ setEnv "_JAVA_AWT_WM_NONREPARENTING" "1"
  -- update executable path
  home <- X.io getHomeDirectory
  X.io $ getEnv "PATH" >>= setEnv "PATH" . (++ ":" ++ home </> ".bin")
  -- gnome session
  gnomeRegister
  -- compton
  safeSpawn "compton" ["-f", "-I", "0.10", "-O", "0.10"]
  -- ibus
  execute "ibus-daemon" ["--xim", "--daemonize", "--replace", "--restart"] ""
  -- xrdb
  execute "xrdb" ["-load", home </> ".Xresources"] ""
  dpi <- gets $ fromMaybe (120 :: Int) . firstOf (settings . "dpi")
  execute "xrdb" ["-merge"] $ printf "Xft.dpi: %d" dpi
  -- wallpaper
  safeSpawn "feh" ["--bg-fill", home </> "Shared/Pictures/wallpaper"]
  safeSpawn "xsetroot" ["-cursor_name", "left_ptr"]
  -- volume change samples
  execute "pacmd" [] "load-sample audio-volume-change \
                      \/usr/share/sounds/freedesktop/stereo/audio-volume-change.oga\n"
  -- notification
  m <- use (config . screenMult)
  h <- use (config . height)
  f <- use (settings . "font-name") :: X.X String
  execute   "pkill" ["dunst"] "" -- wait for completion
  safeSpawn "dunst" ["-fn"      , printf "%s %d" f (8 * m)
                    ,"-geometry", printf "%dx5-0+%d" (300 * m) h
                    ]
  liftIO $ threadDelay 1000 -- hack to encure dunst is strated
  spawn "notify-send Xmonad  Started/Recompiled  --icon=emblem-system"


-- | Window manage hook
manageHook :: ManageHook
manageHook = composeAll
             [ resource  =? "skype"       --> doShift "alpha" <> doFloat
             , className =? "mpv"         --> doFloat
             , className =? "vcl"         --> doFloat
             , role      =? "pop-up"      --> doFloat
             , className =? "Notify-osd"  --> doIgnore
             , className =? "stalonetray" --> doIgnore
             , className =? "dunst"       --> doIgnore
             , className =? "pavel-win"   --> doSink
             , hasName      "hangouts"    --> doShift "alpha" <> doFloat
             , className =? "popcorntime" --> viewShift "iota"
             , transience'
             ]
    where role = stringProperty "WM_WINDOW_ROLE"
          viewShift = doF . ((.) <$> W.greedyView <*> W.shift)

doSink :: ManageHook
doSink = ask >>= doF . W.sink

hasName :: String -> Query Bool
hasName name = do
  res   <- resource
  names <- liftX $ use (settings . "class-to-name")
  return . (== name) $ M.findWithDefault res res names

--------------------------------------------------------------------------------
-- Key bindings
--
-- | Just like additionalKeysP but also overrides alredy defined bindings
overrideKeys :: X.XConfig l -> ([(String, X ())], [String]) -> X.XConfig l
overrideKeys cfg (add, remove) =
    flip additionalKeysP add . flip removeKeysP (remove ++ (fst <$> add)) $ cfg


keysRemove :: [String]
keysRemove = [ "M-q"   -- restart xmonad "M-c q"
             , "M-S-q" -- quit xmonad    "None"
             , "M-p"   -- run dmenu      "M-z"
             , "M-S-p" -- run gmrun      "None"
             ]

keysAdd :: [(String, X ())]
keysAdd =
    [ ("M-z", dmenuRun)
    -- debuging
    , ("M-d u", widgetUpdateAll)
    , ("M-d h", asks (show . X.layoutHook . X.config) >>= X.io . putStrLn)
    , ("M-d S-h", use (config . height) >>= X.io . print)
    , ("M-d d", asyncRun $ dmenuChoose "test" ["one", "two", "tree"] return >>= liftIO . print)
    , ("M-d s", use settings >>= liftIO . B.putStrLn . A.encode)
    , ("M-d m", use (config . screenMult) >>= X.io . print)
    -- apps
    , ("M-x c", kill)
    , ("M-x e", spawn "GDK_SCALE=1 emacs")
    , ("M-x h", asks (X.terminal . X.config) >>= spawn . (++ " -e htop"))
    , ("M-x i", use (settings . "browser") >>= flip safeSpawn ["--incognito"])
    , ("M-x n", spawn "nautilus")
    , ("M-x t", asks (X.terminal . X.config) >>= spawn)
    , ("M-x w", use (settings . "browser") >>= flip safeSpawn [])
    , ("M-x g", dmenuGMail)
    , ("M-x r", safeSpawn "xfreerdp" [])
    -- actions
    , ("M-c l", safeSpawn "i3lock" ["--dpms", "--color", blackL])
    , ("M-c q", spawn "xmonad --recompile && xmonad --restart")
    , ("M-c w", dmenuWindows)
    -- layout switching
    , ("M-<Space>", sendMessage NextLayout)
    , ("M-M1-<Space>", X.asks (X.layoutHook . X.config) >>= setLayout)
    -- keyboard layout
    , ("M1-<Space>", iBusNextEngine)
    -- media keys
    , ("<XF86AudioRaiseVolume>"  , volumeChange 10)
    , ("<XF86AudioLowerVolume>"  , volumeChange (-10))
    , ("<XF86AudioMute>"         , volumeToggleMute)
    , ("<XF86MonBrightnessUp>"   , return () )
    , ("<XF86MonBrightnessDown>" , return () )
    ]


--------------------------------------------------------------------------------
-- DMenu
--
-- | Asynchronous dmenu chooser (can be called from hot key)
dmenuChoose :: T.Traversable t => String -- | badge
            -> t a                      -- | items to choose from
            -> (a -> [String])           -- | format item to list of collumns
            -> Async (Either String a)  -- | item choosen or string inputed
dmenuChoose badge items format = do opts <- use (settings . "dmenu" . to (++ ["-p", badge]))
                                    fmap pick . async $ execute "dmenu" opts entries
    where enum v = do (i, s) <- ST.get
                      _2 .= let s' = length <$> format v
                            in if length s > length s'
                               then zipWith max s (s' ++ repeat 0)
                               else zipWith max (s ++ repeat 0) s'
                      _1 += 1
                      return (i, v)
          (indexed, (count, widths)) = ST.runState (T.mapM enum items) (0,[])
          formatLine :: Int -> String -> String
          formatLine i = printf "%s %s\n" (take (length . show $ count) $ show i ++ repeat ' ')
          entries = flip T.foldMap indexed                                $ \(i,v) ->
                    formatLine i $ flip T.foldMap (zip widths $ format v) $ \(w,s) ->
                    take w $ fmap (\c -> if c == '\n' then ' ' else c) s ++ repeat ' ' ++ " "
          pick item = maybe (Left . fromMaybe "" $ item) Right $
                     do index <- item >>= firstOf (traverse . _1) . reads :: Maybe Int
                        items ^? idx index

-- | Run command with dmenu
dmenuRun :: X.X ()
dmenuRun =
    use (settings . "dmenu") >>= safeSpawn "dmenu_run" . (++ ["-p" , ">_ "])

-- | GMail dmenu
dmenuGMail :: X.X ()
dmenuGMail = do
  msgsEither <- gmailMessagesCached
  browser    <- use (settings . "browser")
  case msgsEither of
    Left  _    -> return ()
    Right msgs -> asyncRun $
                 do msg <- dmenuChoose "asl.pavel@" msgs $ \msg ->
                          fromMaybe [] $ do subj   <- M.lookup "Subject" msg
                                            from   <- M.lookup "From"    msg
                                            return ["| ", from, " | ", subj]
                    maybe (return ()) (safeSpawn browser . (:[])) $
                          msg ^? _Right . at "URL" . _Just

-- | Windows dmenu
dmenuWindows :: X.X ()
dmenuWindows = do
  let winsTagged ws    = (W.tag ws,) <$> (W.integrate' . W.stack $ ws)
      winInfo (_, win) = do name <- show <$> getName win
                            klass <- withDisplay $ \d -> fmap resName $ io $ getClassHint d win
                            return (win, (name, klass))
  classToName <- gets $ fromMaybe M.empty . firstOf (settings . "class-to-name")
  wins  <- gets $ sort . concatMap winsTagged . W.workspaces . windowset
  infos <- M.fromList <$> T.mapM winInfo wins
  asyncRun $ do win <- dmenuChoose "WINS" wins $ \(tag, win) ->
                      let (name, klass) = M.findWithDefault ("", "") win infos
                      in [ "| ", M.findWithDefault tag tag workspacesShort
                         , " | ", M.findWithDefault klass klass classToName
                         , " | ", name]
                case win of
                  Right (_, w) -> lift $ windows (W.focusWindow w)
                  _            -> return ()


{-
  -- stalone tray (lock on binray file, to prevent from starting more then once)
  safeSpawn "flock" ["-n", staloneBinary, staloneBinary,
                     "--icon-size", show height,
                     "--icon-gravity", "E",
                     "--geometry", printf "%dx1-0+0" barTraySize,
                     "--sticky",
                     "--skip-taskbar",
                     "--background", color "theme-tray-bg"]
-}

--------------------------------------------------------------------------------
-- Utilities
--
-- | Register xmonad within gnome-session
gnomeRegister :: MonadIO m => m ()
gnomeRegister = io $ do
  x <- lookup "DESKTOP_AUTOSTART_ID" <$> getEnvironment
  whenJust x $ \sessionId -> safeSpawn "dbus-send"
                ["--session"
                ,"--print-reply"
                ,"--dest=org.gnome.SessionManager"
                ,"/org/gnome/SessionManager"
                ,"org.gnome.SessionManager.RegisterClient"
                ,"string:xmonad"
                ,"string:" ++ sessionId]


--------------------------------------------------------------------------------
-- Timer widget
--
-- | Performance timer widget
timerView :: X.X View
timerView = use $ widgets . at "Timer" . _Just . widgetView

timerProbe :: X.X () -> X.X ()
timerProbe op = do
  start <- X.io getPOSIXTime
  op
  stop <-  X.io getPOSIXTime
  widget "Timer" 0 query ((0, 0) :: (Int, Int)) $
         do let delta :: Int
                delta = round $ (stop - start) * 1000
            delta' <- _1 %%= (\delta' -> join (,) (delta' + delta))
            c' <- _2 %%= (\c' -> join (,) (c' + 1))
            return . (sepLeft <>)
                   . VAttr ".timer" ""
                   . (VAttr ".timer-icon" "" VEmpty <>)
                   . VText $ printf "%2d:%2d " delta (delta' `div` c')
  return ()
      where query = applyFirst [ do matchAttr ".timer"
                                    colorBg .= blackL
                                    fontFamily .= "Cuprum"
                               , do matchAttr ".timer-icon"
                                    fontSize += 2
                                    iconCell "octicon-light-bulb"
                               ]

{- Add lib to flycheck search path
Local Variables:
eval: (setq flycheck-ghc-search-path
       (list (concat (file-name-directory buffer-file-name) "lib")))
End:
-}
