{-# LANGUAGE DeriveDataTypeable #-}
module XMonad.Powerbar.Config
    ( Config
    , emptyConfig
    -- config lenses
    , pollDelay
    , styleQuery
    , height
    , screenSize
    , screenMult
    , screenIndex
    , fonts
    , actions
    , action
    -- access config inside X monad
    , config
    , extState
    ) where

import XMonad.Powerbar.Lens

import Control.Applicative   (empty)
import Control.Monad         (join)
import Data.Maybe            (fromMaybe)
import Data.Typeable         (Typeable, cast, typeOf)
import Graphics.X11.Xft      (XftFont)
import Text.Read             (readMaybe)
import XMonad.Powerbar.Style (StyleQuery)
import qualified Data.Map as M
import qualified XMonad   as X


-- | Powerbar configuration
data Config = Config
    { _pollDelay   :: Rational                   -- ^ delays between polls (in seconds)
    , _styleQuery  :: StyleQuery ()               -- ^ style query
    -- Following values are used internaly and will be overwritten
    , _height      :: Int                        -- ^ powerbar height
    , _screenSize  :: (Int, Int, Int, Int)       -- ^ (width px, height px, width mm, height mm)
    , _screenMult  :: Int                        -- ^ pixel multiplier for HIDPI screens
    , _screenIndex :: Int                        -- ^ show on specified screen
    , _fonts       :: M.Map String XftFont       -- ^ fonts cache (used to calculate view size)
    , _actions     :: M.Map Int (X.X ())         -- ^ currently installed actions
    } deriving Typeable


-- Config lenses
pollDelay :: Lens' Config Rational
pollDelay f v = (\c -> v {_pollDelay = c}) `fmap` f (_pollDelay v)

styleQuery :: Lens' Config (StyleQuery ())
styleQuery f v = (\c -> v {_styleQuery = c}) `fmap` f (_styleQuery v)

height :: Lens' Config Int
height f v = (\c -> v {_height = c}) `fmap` f (_height v)

screenSize :: Lens' Config (Int, Int, Int, Int)
screenSize f v = (\c -> v {_screenSize = c}) `fmap` f (_screenSize v)

screenMult :: Lens' Config Int
screenMult f v = (\c -> v {_screenMult = c}) `fmap` f (_screenMult v)

screenIndex :: Lens' Config Int
screenIndex f v = (\c -> v {_screenIndex = c}) `fmap` f (_screenIndex v)

fonts :: Lens' Config (M.Map String XftFont)
fonts f v = (\c -> v {_fonts = c}) `fmap` f (_fonts v)

actions :: Lens' Config (M.Map Int (X.X ()))
actions f v = (\c -> v {_actions = c}) `fmap` f (_actions v)


-- | Evaluate action by its code
action :: Int -> X.X ()
action code = do
  a <- use $ config . actions . at code
  fromMaybe (return ()) a
  join $ X.asks (X.logHook . X.config) -- run log hook


--------------------------------------------------------------------------------
--  Config lens (manipulate config inside X monad state)
--
emptyConfig :: Config
emptyConfig = Config { _pollDelay   = 5
                     , _styleQuery  = empty
                     , _height      = -1
                     , _screenSize  = (-1, -1, -1, -1)
                     , _screenMult  = 1
                     , _screenIndex = 0
                     , _fonts       = M.empty
                     , _actions     = M.empty
                     }

instance X.ExtensionClass Config where
    initialValue = emptyConfig

config :: Lens' X.XState Config
config = extState

--------------------------------------------------------------------------------
-- Extensible state lens
--
extState :: X.ExtensionClass a => Lens' X.XState a
extState f v = setter v `fmap` f (getter undefined v)
    where
      setter :: X.ExtensionClass a => X.XState -> a -> X.XState
      setter s c = s {X.extensibleState = insert (X.extensibleState s)}
          where insert = M.insert (show . typeOf $ c) (Right . X.extensionType $ c)
      getter :: X.ExtensionClass a => a -> X.XState -> a
      getter a s =
          case M.lookup (show . typeOf $ a) . X.extensibleState $ s of
            Just (Right (X.StateExtension c))      -> fromMaybe X.initialValue (cast c)
            Just (Right (X.PersistentExtension c)) -> fromMaybe X.initialValue (cast c)
            Just (Left str) | X.PersistentExtension x <- X.extensionType a ->
              fromMaybe X.initialValue $ cast =<< readMaybe str `asTypeOf` Just x
            _ -> X.initialValue
