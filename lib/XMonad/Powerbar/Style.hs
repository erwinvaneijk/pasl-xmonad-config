{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, RankNTypes #-}
module XMonad.Powerbar.Style
    ( Style, StyleAlign(..)
    , colorFg   , colorFgMaybe
    , colorBg   , colorBgMaybe
    , fontFamily, fontFamilyMaybe
    , fontSize  , fontSizeMaybe
    , fontPattern
    , width
    , align
    , mutate_
    , bubble_
    -- Style Query
    , StyleQuery
    , StyleQueryT
    , runStyleQuery
    , runStyleQueryT
    , runStyleQueryEmpty
    , (<|>)
    -- Standard queries
    , matchAttr
    , matchAttr'
    , lookupAttr
    , lookupAttr'
    , leftStyle
    , rightStyle
    , applyFirst
    , applyAll
    , mutate
    , replace
    , bubble
    , swapColors
    , spaced
    ) where

import XMonad.Powerbar.Lens
import Control.Applicative    ((<|>), Alternative(..))
import Control.Monad.Identity (runIdentity, Identity)
import Data.Monoid            (Endo(..), (<>))
import Text.Printf            (printf)
import XMonad.Powerbar.View   (View(..))
import qualified Control.Monad.Trans.State  as ST
import qualified Control.Monad.Trans.Reader as RT
import qualified Control.Monad.Trans.Maybe  as MT


-- | Rendering style information
data Style = Style
    { _colorFg    :: Maybe String  -- ^ foreground color
    , _colorBg    :: Maybe String  -- ^ background color
    , _fontFamily :: Maybe String  -- ^ font family
    , _fontSize   :: Maybe Int     -- ^ font size
    , _width      :: Maybe Int     -- ^ set fixed width
    , _align      :: StyleAlign    -- ^ alignment inside nested block
    , _mutate     :: Endo View     -- ^ mutate view
    , _bubble     :: Bool          -- ^ postpone rendering
    }


instance Monoid Style where
    mempty =
        Style { _colorFg    = Nothing
              , _colorBg    = Nothing
              , _fontFamily = Nothing
              , _fontSize   = Nothing
              , _width      = Nothing
              , _align      = SACenter
              , _mutate     = mempty
              , _bubble     = False
              }
    -- left style takes precedence
    mappend s s' =
        Style { _colorFg    = _colorFg    s <|> _colorFg    s'
              , _colorBg    = _colorBg    s <|> _colorBg    s'
              , _fontFamily = _fontFamily s <|> _fontFamily s'
              , _fontSize   = _fontSize   s <|> _fontSize   s'
              , _width      = _width      s <|> _width      s'
              , _align      = _align      s
              , _mutate     = _mutate     s <>  _mutate     s'
              , _bubble     = _bubble     s ||   _bubble     s'
              }


instance Show Style where
    show s = "Style{" ++ (unwords . mconcat $ props) ++ "}"
        where
          format :: (String, Style -> Maybe String) -> [String]
          format (name, getter) = maybe [] ((:[]) . printf "%s:%s" name) $ getter s
          props = format <$> [("colorFg"   , _colorFg)
                             ,("colorBg"   , _colorBg)
                             ,("fontFamily", _fontFamily)
                             ,("fontSize"  , fmap show . _fontSize)
                             ,("align"     , \st -> const (show . _align $ st) <$> _width st)
                             ,("width"     , fmap show . _width)]


-- | Alignment inside block with width
data StyleAlign = SALeft
                | SACenter
                | SARight

instance Show StyleAlign where
    show SALeft   = "left"
    show SACenter = "center"
    show SARight  = "right"


-- | Lenses for accessing style properties
colorFgMaybe :: Lens' Style (Maybe String)
colorFgMaybe f v = (\c -> v {_colorFg = c}) <$> f (_colorFg v)
colorFg :: Lens' Style String
colorFg = colorFgMaybe . non ":none"

colorBgMaybe :: Lens' Style (Maybe String)
colorBgMaybe f v = (\c -> v {_colorBg = c}) <$> f (_colorBg v)
colorBg :: Lens' Style String
colorBg = colorBgMaybe . non ":none"

fontFamilyMaybe :: Lens' Style (Maybe String)
fontFamilyMaybe f v = (\c -> v {_fontFamily = c}) <$> f (_fontFamily v)
fontFamily :: Lens' Style String
fontFamily = fontFamilyMaybe . non ":none"

fontSizeMaybe :: Lens' Style (Maybe Int)
fontSizeMaybe f v = (\c -> v {_fontSize = c}) <$> f (_fontSize v)
fontSize :: Lens' Style Int
fontSize = fontSizeMaybe . non 8

fontPattern :: Style -> String
fontPattern s = printf "%s:size=%d" (s ^. fontFamily) (s ^. fontSize)

width :: Lens' Style (Maybe Int)
width f v = (\c -> v {_width = c}) <$> f (_width v)

align :: Lens' Style StyleAlign
align f v = (\c -> v {_align = c}) <$> f (_align v)

-- Only used by rendering logic
mutate_ :: Lens' Style (Endo View)
mutate_ f v = (\c -> v {_mutate = c}) <$> f (_mutate v)

-- Only used by rendering logic
bubble_ :: Lens' Style Bool
bubble_ f v = (\c -> v {_bubble = c}) <$> f (_bubble v)


--------------------------------------------------------------------------------
-- Style Query
--
-- | Style query calculates new style from previous style and set of attributes
type StyleQueryT m a =
    RT.ReaderT (Style, Style, [(String, Value)]) (ST.StateT Style (MT.MaybeT m)) a

type StyleQuery a = StyleQueryT Identity a

instance (Functor m, Monad m, Monoid a) => Monoid (StyleQueryT m a) where
    mempty = return mempty
    mappend q0 q1 = mappend <$> (q0 <|> mempty) <*> (q1 <|> mempty)

-- | Run style query
runStyleQueryT :: (Functor m, Monad m)
               => StyleQueryT m a       -- ^ query to run
               -> (Style, Style, Style) -- ^ left, current, right styles
               -> [(String,Value)]      -- ^ attributes
               -> m Style               -- ^ calcualated style
runStyleQueryT q (ls, s, rs) as = do
    maybeState <- MT.runMaybeT
                 . flip ST.runStateT s
                 . flip RT.runReaderT (ls, rs, as) $ q
    return $ case maybeState of
               Just (_, s') -> s'
               _            -> s

-- | Run style query inside identity monad
runStyleQuery :: StyleQuery a -> (Style, Style, Style) -> [(String, Value)] -> Style
runStyleQuery q ss as = runIdentity $ runStyleQueryT q ss as

-- | Run style query with mempty current and privious styles
runStyleQueryEmpty :: StyleQuery a -> [(String, Value)] -> Style
runStyleQueryEmpty q = runStyleQuery q (mempty, mempty, mempty)


--------------------------------------------------------------------------------
-- Standard queries
--
-- | Match attribute by name
matchAttr :: (Functor m, Monad m) => String -> StyleQueryT m Value
matchAttr name = do
   (_, _, (name', value'):_) <- RT.ask
   if name' == name
   then return value'
   else empty

-- | Match and traverse attribute by name
matchAttr' :: (Functor m, Monad m) => String -> Fold' Value v -> StyleQueryT m v
matchAttr' name focus = do
    value <- firstOf focus <$> matchAttr name
    maybe empty return value

-- | Lookup attribute by name
lookupAttr :: (Functor m, Monad m) => String -> StyleQueryT m Value
lookupAttr name = do
  (_, _, attrs) <- RT.ask
  maybe empty return $ lookup name attrs

-- | Lookup and traverse attribute by name
lookupAttr' :: (Functor m, Monad m) => String -> Fold' Value v -> StyleQueryT m v
lookupAttr' name focus = do
  value <- firstOf focus <$> lookupAttr name
  maybe empty return value

-- | Get previous style
leftStyle :: (Functor m, Monad m) => StyleQueryT m Style
leftStyle = (\(s, _, _) -> s) <$> RT.ask

-- | Get next style (only valid for bubbled views)
rightStyle :: (Functor m, Monad m) => StyleQueryT m Style
rightStyle = (\(_, s, _) -> s) <$> RT.ask

-- | Evaluate first successful query
applyFirst :: (Functor m, Monad m) => [StyleQueryT m a] -> StyleQueryT m a
applyFirst = foldl (<|>) empty

-- | Evaluate all successful queries
applyAll :: (Functor m, Monad m, Monoid a) => [StyleQueryT m a] -> StyleQueryT m a
applyAll = foldl (<>) mempty

-- | Mutate view with provided endomorphism
mutate :: Monad m => (View -> View) -> StyleQueryT m ()
mutate m = mutate_ %= (Endo m <>)

-- | Replace view with provided one
replace :: (Functor m, Monad m) => View -> StyleQueryT m ()
replace v = mutate (const v)

-- | Postpone rendering (used to corectly render separators with depends
--  on the background colors of separated chunks)
bubble :: Monad m => StyleQueryT m ()
bubble = bubble_ .= True

-- | Swap background and foreground colors
swapColors :: (Functor m, Monad m) => StyleQueryT m ()
swapColors = do
  fg <- use colorFg
  bg <- use colorBg
  colorFg .= bg
  colorBg .= fg

-- | Make view enclosed in spaces
spaced :: Monad m => Int -> StyleQueryT m ()
spaced count = mutate $ (space <>) . (<> space)
    where space = VText (replicate count ' ')
