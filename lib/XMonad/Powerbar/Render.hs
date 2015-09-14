{-# LANGUAGE RankNTypes, LambdaCase, TupleSections, OverloadedStrings #-}
module XMonad.Powerbar.Render (render, renderX, renderIO) where

import XMonad.Powerbar.View
import XMonad.Powerbar.Style
import XMonad.Powerbar.Lens
import XMonad.Powerbar.Widgets   (widgetsQuery)

import Control.Applicative       (many)
import Control.Arrow             ((&&&))
import Control.Exception         (bracket)
import Control.Monad             (join, guard)
import Control.Monad.IO.Class    (MonadIO, liftIO)
import Data.Maybe                (fromMaybe)
import Data.Monoid               (Endo(..), (<>))
import Graphics.X11.Xft          (xftFontOpen, xftTextExtents, XftFont)
import Graphics.X11.Xrender      (xglyphinfo_xOff, xglyphinfo_height)
import Text.Printf               (printf)
import qualified Data.Attoparsec.Text      as A
import qualified Data.Text                 as T
import qualified Data.Map                  as M
import qualified Control.Monad.Trans.State as ST
import qualified XMonad                    as X
import qualified XMonad.Powerbar.Config    as C


--------------------------------------------------------------------------------
-- Dzen renderer
--
-- Supported dzen commands
--
--  fg(color)            - foreground color
--  bg(color)            - background color
--  i(filename)          - XBM|XPM file
--  r[o](width x height) - rectangle [outline]
--  c[o](radius)         - circle [outline]
--  p[a](args)           - position [absolute] where `args` in
--                         ([(+|-)x][;(+|-)y]|_LOCK_X|_UNLOCK_X|_LEFT|_RIGHT)
--  fn(font)             - specify custom font
--  ca(button, cmd)      - clickable area
--  ba(pixels, align)    - block align where `align` in (_LEFT|_RIGHT|_CENTER)
--                         and `pixels` is the width of the block
--
type Render m a = ST.StateT RenderState m a

data RenderState = RenderState
    { _actions     :: [X.X ()]
    , _attrs       :: [(String, Value)]
    , _style       :: [Style]
    , _bubbled     :: [View]
    , _styleB      :: Maybe Style
    , _styleL      :: Style  -- left style of block
    , _styleR      :: Style  -- right style of block (correct for bubled)
    , _xOff        :: Int
    , _height      :: Int
    -- persistent values
    , _display     :: X.Display
    , _styleQuery  :: StyleQuery ()
    , _fonts       :: M.Map String XftFont
    }


mkRenderState :: X.Display -> StyleQuery () -> M.Map String XftFont -> RenderState
mkRenderState dpy qry fts =
    RenderState { _actions    = []
                , _attrs      = []
                , _style      = []
                , _bubbled    = []
                , _styleB     = Nothing
                , _styleL     = mempty
                , _styleR     = mempty
                , _xOff       = 0
                , _height     = 0
                , _display    = dpy
                , _styleQuery = qry
                , _fonts      = fts
                }

-- Render state lenses
actions :: Lens' RenderState [X.X ()]
actions f v = (\c -> v {_actions = c}) <$> f (_actions v)

attrs :: Lens' RenderState [(String, Value)]
attrs f v = (\c -> v {_attrs = c}) <$> f (_attrs v)

style :: Lens' RenderState [Style]
style f v = (\c -> v {_style = c}) <$> f (_style v)

bubbled :: Lens' RenderState [View]
bubbled f v = (\c -> v {_bubbled = c}) <$> f (_bubbled v)

styleB :: Lens' RenderState (Maybe Style)
styleB f v = (\c -> v {_styleB = c}) <$> f (_styleB v)

styleL :: Lens' RenderState Style
styleL f v = (\c -> v {_styleL = c}) <$> f (_styleL v)

styleR :: Lens' RenderState Style
styleR f v = (\c -> v {_styleR = c}) <$> f (_styleR v)

xOff :: Lens' RenderState Int
xOff f v = (\c -> v {_xOff = c}) <$> f (_xOff v)

height :: Lens' RenderState Int
height f v = (\c -> v {_height = c}) <$> f (_height v)

display :: Lens' RenderState X.Display
display f v = (\c -> v {_display = c}) <$> f (_display v)

styleQuery :: Lens' RenderState (StyleQuery ())
styleQuery f v = (\c -> v {_styleQuery = c}) <$> f (_styleQuery v)

fonts :: Lens' RenderState (M.Map String XftFont)
fonts f v = (\c -> v {_fonts = c}) <$> f (_fonts v)


-- | Render view
render :: (Functor m, MonadIO m) => View -> Render m String
render = \case
         -- view composition
    VEmpty      -> return ""
    VPair lv rv -> do
      styleB  .= Nothing
      bubbled .= []
      lt <- render lv
      lb <- use bubbled
      ls <- use styleB
      ll <- use styleL

      styleB  .= Nothing
      bubbled .= []
      rt <- render rv
      rb <- use bubbled
      rs <- use styleB
      rl <- use styleL

      case rs of
        -- no right style, bubble highter
        Nothing -> do
                bubbled .= lb ++ rb
                styleB  .= (ls <|> rs)
                return $ lt ++ rt
        -- render left bubbled views
        Just s  -> do
                styleL  .= ll
                styleR  .= s
                bubbled .= []
                bt <- join <$> mapM render lb
                styleL .= rl
                styleR .= mempty
                styleB .= (ls <|> rs)
                bubbled <>= rb
                return $ lt ++ bt ++ rt

    -- register actions
    VAction b a v -> do
      text <- render v
      if text == ""
      then return ""
      else do
        code <- actions %%= (length &&& (a:))
        return $ printf "^ca(%d,echo %d)%s^ca()"
                   ((+1) . fromEnum $ b) code text

    -- update classes and compute new style
    VAttr an av v -> do
      s  <- pick style
      s' <- runStyleQuery
           <$> use styleQuery
           <*> ((,,) <$> use styleL <*> return s <*> use styleR)
           <*> push attrs (an, av)
      if s' ^. bubble_
      then do
        bubbled <>= [appEndo (s' ^. mutate_) v]
        pop attrs
        return ""
      else do
        push style (s' & mutate_ .~ mempty)
        text <- renderAligned $ appEndo (s' ^. mutate_) v
        pop style
        pop attrs
        return $ switchStyle s s' ++ text ++ switchStyle s' s

    -- render text
    VText text -> do
      -- update size
      (w, h) <- textExtents text
      xOff   += w
      height %= max h
      -- update styles
      s <- pick style
      styleL .= s
      styleB .= Just s
      return text


-- | String required to swich style
switchStyle :: Style -> Style -> String
switchStyle s s' = fnt ++ fg ++ bg
    where fnt = if fontPattern s /= fontPattern s'
                then printf "^fn(%s)" (fontPattern s')
                else ""
          bg  = if (s ^. colorBgMaybe) /= (s' ^. colorBgMaybe)
                then printf "^bg(%s)" $ fromMaybe "" (s' ^. colorBgMaybe)
                else ""
          fg  = if (s ^. colorFgMaybe) /= (s' ^. colorFgMaybe)
                then printf "^fg(%s)" $ fromMaybe "" (s' ^. colorFgMaybe)
                else ""


-- | Render view with applying current alignemnt
renderAligned :: (Functor m, MonadIO m) => View -> Render m String
renderAligned v = do
  x <- use xOff
  s <- pick style
  style . idx (0 :: Int) . width .= Nothing -- reset width
  res <- render v
  w <- subtract x <$> use xOff
  case s ^. width of
    Nothing  -> return res
    Just w'  ->
        if w' <= w
        then return res
        else do
          xOff .= x + w'
          return $ case s ^. align of
                     SALeft   -> res' w'
                     SARight  -> res' w
                     SACenter -> res' ((w' + w) `div` 2)
              where
                res' :: Int -> String
                res' off = printf "^r(%d)^p(-%d)%s^p(%d)"
                           w' off res (off - w)


-- | Calculate text extents (x-offset, height)
textExtents :: (Functor m, MonadIO m) => String -> Render m (Int, Int)
textExtents text = do
  fnt <- fontPattern <$> pick style
  dpy <- use display
  xfonts <- use fonts
  xfont  <- case M.lookup fnt xfonts of
             Just xfont -> return xfont
             Nothing    ->  -- failed to retrieve font from cache
                   do xfont <- liftIO $ xftFontOpen dpy
                              (X.defaultScreenOfDisplay dpy) fnt
                      fonts %= M.insert fnt xfont
                      return xfont
  ext <- liftIO . xftTextExtents dpy xfont . textStrip $ text
  return ( xglyphinfo_xOff ext
         , xglyphinfo_height ext )


-- | Strip text from dzen control sequences
textStrip :: String -> String
textStrip =
    either (const "") (T.unpack . mconcat) . A.parseOnly (many chunk) . T.pack
    where chunk = do text <- A.takeWhile (/= '^')
                     sep  <- A.takeWhile (== '^')
                     guard . not $ T.null text && T.null sep
                     if sep == "^^"
                     then return $ text <> "^"
                     else A.option text $ do
                       A.skipWhile (/= '(')
                       A.skipWhile (/= ')')
                       A.anyChar
                       return text


--------------------------------------------------------------------------------
-- Manage stack
--
-- | Push value on stack, returns current stack
push :: Monad m => Lens' RenderState [a] -> a -> Render m [a]
push l v = l %%= join (,) . (v:)

-- | Pop value from stack
pop :: Monad m => Lens' RenderState [a] -> Render m ()
pop l = l %= \case {(_:vs) -> vs; _ -> []}

-- | Pick first value from stack
pick :: (Monad m, Monoid a) => Lens' RenderState [a] -> Render m a
pick l = use $ l . idx (0 :: Int)

--------------------------------------------------------------------------------
-- Specialized renderers
--
-- | Render views (aligned left, middle, right) in X monad
renderX :: (View, View, View)  -- ^ (left, middle, right) views
        -> X.X String          -- ^ dzen string and registred actions
renderX (lview, cview, rview) = do
  dpy <- X.asks X.display
  (w,_,_,_) <- use $ C.config . C.screenSize
  -- render bar
  let renderBar :: Render X.X String
      renderBar = do
        -- set bar style
        q <- use styleQuery
        push style $ runStyleQueryEmpty q [(".bar", "")]
        stext <- switchStyle mempty <$> pick style
        -- left view
        ltext <- render $ lview <> VText ""
        xOff .= 0
        -- center view
        ctext <- render $ cview <> VText ""
        cxOff <- xOff %%= (,0)
        -- rigth view
        rtext <- render $ rview <> VText ""
        rxOff <- use xOff
        -- position views
        return $ join [ printf "%s^r(%d)" stext (if w > 0 then w else 65536)
                      , printf "^p(_LEFT)%s" ltext
                      , printf "^p(_CENTER)^p(-%d)%s" (cxOff `div` 2) ctext
                      , printf "^p(_RIGHT)^p(-%d)%s" rxOff rtext
                      ]
  q <- use $ C.config . C.styleQuery
  wq <- widgetsQuery
  f <- use $ C.config . C.fonts
  (text, state) <- ST.runStateT renderBar $ mkRenderState dpy (wq <> q) f

  -- update stored values
  C.config . C.actions .= M.fromList (zip [0..] . reverse $ (state ^. actions))
  C.config . C.fonts   .= state ^. fonts

  return text


-- | Render view inside IO monad
renderIO :: StyleQuery () -> View -> IO (String, Int, Int)
renderIO q v =
    bracket (X.openDisplay "") X.closeDisplay $ \dpy -> do
      (text, state) <- ST.runStateT (render v) (mkRenderState dpy q M.empty)
      return (text, state ^. xOff , state ^. height)
