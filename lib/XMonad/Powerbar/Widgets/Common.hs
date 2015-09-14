{-# LANGUAGE DeriveDataTypeable, RankNTypes, OverloadedStrings #-}
module XMonad.Powerbar.Widgets.Common
    ( cell
    , cellHeight
    , cells
    , execute
    , httpGet
    -- widget
    , WidgetContext
    , widget
    , widgetDisabled
    , widgetStamp
    , widgetView
    , widgetName
    , widgetState
    , widgetState'
    , widgetQuery
    , widgetUpdate
    , widgetUpdateAll
    , widgetMaybe
    , widgetMaybeIO
    , widgets
    , widgetsQuery
    -- colors
    , background, foreground
    , colors
    , black, red, green, yellow, blue, magenta, cyan, white
    , blackL, redL, greenL, yellowL, blueL, magentaL, cyanL, whiteL
    ) where

import XMonad.Powerbar.Style
import XMonad.Powerbar.Lens
import Control.Applicative    (Alternative(..))
import Control.Exception      (tryJust)
import Control.Monad          (join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Dynamic           (Dynamic, Typeable, toDyn, fromDyn)
import Data.Time.Clock.POSIX  (POSIXTime, getPOSIXTime)
import System.IO              (hPutStrLn, stderr)
import System.IO.Error        (ioeGetErrorType)
import Text.Printf            (printf)
import XMonad.Powerbar.View   (View(VEmpty))
import XMonad.Powerbar.Config (extState)
import XMonad.Util.Run        (runProcessWithInput)
import qualified Control.Monad.Trans.State as ST
import qualified Control.Monad.Trans.Maybe as MT
import qualified Data.ByteString.Lazy      as B
import qualified Data.Map as M
import qualified Network.HTTP.Client       as H
import qualified XMonad as X


--------------------------------------------------------------------------------
-- Widget
--
data WState = WState { _wDisabled :: Bool
                     , _wStamp    :: POSIXTime
                     , _wView     :: View
                     , _wState    :: Dynamic
                     , _wName     :: String
                     , _wQuery    :: StyleQuery ()
                     } deriving Typeable

widgetDisabled :: Lens' WState Bool
widgetDisabled f v = (\c -> v {_wDisabled = c}) `fmap` f (_wDisabled v)

widgetStamp :: Lens' WState POSIXTime
widgetStamp f v = (\c -> v {_wStamp = c}) `fmap` f (_wStamp v)

widgetView :: Lens' WState View
widgetView f v = (\c -> v {_wView = c}) `fmap` f (_wView v)

widgetName :: Lens' WState String
widgetName f v = (\c -> v {_wName = c}) `fmap` f (_wName v)

widgetState :: Lens' WState Dynamic
widgetState f v = (\c -> v {_wState = c}) `fmap` f (_wState v)

widgetQuery :: Lens' WState (StyleQuery ())
widgetQuery f v = (\c -> v {_wQuery = c}) `fmap` f (_wQuery v)

mkWState :: Typeable a => String -> StyleQuery () -> a -> WState
mkWState name query state =
    WState { _wDisabled   = False
           , _wStamp      = 0
           , _wView       = VEmpty
           , _wState      = toDyn state
           , _wName       = name
           , _wQuery      = query
           }

-- | Container for widget states that kept inside XState
data Widgets = Widgets { _widgets :: M.Map String WState } deriving Typeable

widgets :: Lens' X.XState (M.Map String WState)
widgets = extState . widgetsLens
    where
      widgetsLens :: Lens' Widgets (M.Map String WState)
      widgetsLens f v = (\c -> v {_widgets = c}) `fmap` f (_widgets v)

instance X.ExtensionClass Widgets where
    initialValue = Widgets M.empty

type WidgetContext s a = ST.StateT s (MT.MaybeT X.X) a

-- | Evaluate widget
widget :: Typeable s
       => String                -- ^ widget name
       -> POSIXTime             -- ^ widget ttl (time to live)
       -> StyleQuery ()          -- ^ widget style query
       -> s                     -- ^ widget initial state
       -> WidgetContext s View  -- ^ widget evaluation context
       -> X.X View              -- ^ widget view
widget name ttl query ist ctx = do
  -- get/create current widget state
  Just st <- use $ widgets . at name . to (<|> Just (mkWState name query ist))
  if st ^. widgetDisabled
  then return VEmpty
  else do
    now <- X.io getPOSIXTime
    if now - st ^. widgetStamp < ttl
    then return (st ^. widgetView)
    else do
      let ctxState = fromDyn (st ^. widgetState) ist
      ctxRes <- MT.runMaybeT (ST.runStateT ctx ctxState)
      case ctxRes of
        -- Evaluation failed (disable widget)
        Nothing -> do
           widgets . at name .= Just (st & widgetDisabled .~ True)
           X.trace $ printf "[xmonad.powerbar] widget %s has been disabled" name
           return VEmpty
        -- Update state and return view
        Just (ctxView, ctxState') ->
            do let st' = st & widgetStamp .~ now
                            & widgetView  .~ ctxView
                            & widgetState .~ toDyn ctxState'
               widgets . at name .= Just st'
               return ctxView


-- | Widgets style query
widgetsQuery :: X.X (StyleQuery ())
widgetsQuery = use $ widgets . traverse . widgetQuery

-- | Force update of specified widget
widgetUpdate :: String -> X.X ()
widgetUpdate name = do
    widgets . at name . _Just . widgetStamp .= 0
    join $ X.asks (X.logHook . X.config)  -- run log hook

-- | Force update of all widgets
widgetUpdateAll :: X.X ()
widgetUpdateAll = do
    widgets . traverse . widgetStamp .= 0
    join $ X.asks (X.logHook . X.config)  -- run log hook

-- | Widget state lens with default value
widgetState' :: Typeable a => String -> a -> Traversal' X.XState a
widgetState' name st = widgets . at name . _Just . widgetState . iso (`fromDyn` st) toDyn

-- | Maybe inside widget context
widgetMaybe :: WidgetContext s (Maybe a) -> WidgetContext s a
widgetMaybe = (>>= maybe empty return)

-- | Maybe IO inside widget context
widgetMaybeIO :: IO (Maybe a) -> WidgetContext s a
widgetMaybeIO = widgetMaybe . liftIO


--------------------------------------------------------------------------------
-- Utilites
--
-- | Make width of count * height
cells :: Int -> StyleQuery ()
cells count = width <~ Just . (* count) <$> cellHeight

-- | Make width equal to state
cell :: StyleQuery ()
cell = cells 1

-- | Cell height
cellHeight :: StyleQuery Int
cellHeight = lookupAttr' ".bar-height" _Integral <|> return 10


-- | Safely execute command and return output on success
execute :: MonadIO m => FilePath -> [String] -> String -> m (Maybe String)
execute exec args input = do
    res <- liftIO . tryJust (Just . ioeGetErrorType)
          $ runProcessWithInput exec args input
    case res of
      Right res' -> return . Just $ res'
      Left err   ->
          do liftIO . hPutStrLn stderr $ printf "[xmonad.powerbar] %v %v: %v"
                                         (show exec) (show args) (show err)
             return Nothing


-- | Simple http getter (better use inside Async, as it may block)
httpGet :: MonadIO m => String -> m B.ByteString
httpGet url = liftIO . H.withManager H.defaultManagerSettings $ \m ->
                do req <- H.parseUrl url
                   res <- H.httpLbs req m
                   return $ H.responseBody res


--------------------------------------------------------------------------------
-- Colors
--
foreground, background :: String
foreground = "#839496"
background = "#171a1f"

black, red, green, yellow, blue, magenta, cyan, white :: String
black   = "#3f3f4c"
red     = "#ac6a76"
green   = "#7b8c58"
yellow  = "#bc945a"
blue    = "#58698c"
magenta = "#7b5e8d"
cyan    = "#82a1b2"
white   = "#cccaca"

blackL, redL, greenL, yellowL, blueL, magentaL, cyanL, whiteL :: String
blackL   = "#525263"
redL     = "#c47987"
greenL   = "#90a366"
yellowL  = "#d4a765"
blueL    = "#7086b2"
magentaL = "#9572ab"
cyanL    = "#95b9cc"
whiteL   = "#edeff2"

colors :: M.Map String String
colors = M.fromList [("black"         , black    ),
                     ("red"           , red      ),
                     ("green"         , green    ),
                     ("yellow"        , yellow   ),
                     ("blue"          , blue     ),
                     ("magenta"       , magenta  ),
                     ("cyan"          , cyan     ),
                     ("white"         , white    ),
                     ("black-light"   , blackL   ),
                     ("red-light"     , redL     ),
                     ("green-light"   , greenL   ),
                     ("yellow-light"  , yellowL  ),
                     ("blue-light"    , blueL    ),
                     ("magenta-light" , magentaL ),
                     ("cyan-light"    , cyanL    ),
                     ("white-light"   , whiteL   )]
