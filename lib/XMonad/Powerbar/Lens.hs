{-# LANGUAGE
  RankNTypes,
  FlexibleInstances,
  LambdaCase,
  ScopedTypeVariables,
  GADTs #-}
module XMonad.Powerbar.Lens
    ( module L
    , non
    , idx
    , range
    , match
    , traverse
    -- JSON
    , Value(..)
    , toJSON
    , fromJSON
    , AsJSON(..)
    , AsValue(..)
    , _Null
    , _Number
    , _Double
    , _Float
    , _Integral
    , _Int
    , _Bool
    , _String
    , _Object
    , key
    , key'
    , _Array
    , nth
    ) where
import Lens.Family2           as L
import Lens.Family2.Stock     as L
import Lens.Family2.State     as L
import Lens.Family2.Unchecked as L

import Data.Aeson
import Data.Default
import Data.Maybe          (fromMaybe)
import Data.Text           (Text)
import Data.String         (IsString(..))
import qualified Control.Monad.State        as ST
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString            as BS
import qualified Data.HashMap.Strict        as H
import qualified Data.Scientific            as S
import qualified Data.Traversable           as T
import qualified Data.Vector                as V


-- | Lens for @Maybe a@ from its default value
non :: Eq a => a -> Lens' (Maybe a) a
non d f m = (\v -> if v == d then Nothing else Just v) `fmap` f (fromMaybe d m)

-- | Traversal for specified index of Traversable
idx :: (Ord i, Num i, T.Traversable t) => i -> Traversal' (t a) a
idx i = range i (i+1)

-- | Traveral for specified index range of Traversable
range :: (Ord i, Num i, T.Traversable t) => i -> i -> Traversal' (t v) v
range begin end f =  T.sequenceA . flip ST.evalState 0 . T.mapM withIndex
    where withIndex v = do
            i <- id %%= \s -> (s, s+1)
            return (if i >= begin && i < end then f v else pure v)

-- | Make traversal from modify and match function
match :: (a -> Maybe b) -> (a -> b -> a) -> Traversal' a b
match matcher setter f v =
    case matcher v of
      Nothing -> pure v
      Just v' -> setter v <$> f v'

--------------------------------------------------------------------------------
-- JSON Traversals
--
-- | Types that could be treated as json
class AsJSON t where
    _JSON  :: (ToJSON a, FromJSON a) => Traversal' t a

instance AsJSON Value where
    _JSON f v = case fromJSON v of
                  Success c -> toJSON <$> f c
                  _         -> pure v

instance AsJSON B.ByteString where
    _JSON f v = maybe (pure v) (fmap encode . f) (decode v)

instance AsJSON BS.ByteString where
    _JSON = iso B.fromStrict B.toStrict . _JSON

instance AsJSON String where
    _JSON = iso B.pack B.unpack . _JSON


-- | Type that could be treaded as json value
class AsValue t where
    _Value :: Traversal' t Value

instance AsValue Value where
    _Value = id

instance AsValue B.ByteString where
    _Value = _JSON

instance AsValue BS.ByteString where
    _Value = _JSON

instance AsValue String where
    _Value = _JSON


_Null :: AsValue t => Traversal' t ()
_Null = _Value . match (\case Null -> Just (); _ -> Nothing) (\_ _ -> Null)

_Number :: AsValue t => Traversal' t S.Scientific
_Number = _Value . match (\case Number v -> Just v; _ -> Nothing) (const Number)

_Integral :: (AsValue t, Integral v) => Traversal' t v
_Integral = _Number . iso floor fromIntegral

_Int :: AsValue t => Traversal' t Int
_Int = _Integral

_Double :: AsValue t => Traversal' t Double
_Double = _Number . iso S.toRealFloat realToFrac

_Float :: AsValue t => Traversal' t Float
_Float = _Number . iso S.toRealFloat realToFrac

_Bool :: AsValue t => Traversal' t Bool
_Bool = _Value . match (\case Bool v -> Just v; _ -> Nothing) (const Bool)

_String :: AsValue t => Traversal' t Text
_String = _Value . match (\case String v -> Just v; _ -> Nothing) (const String)

_Object :: AsValue t => Traversal' t (H.HashMap Text Value)
_Object = _Value . match (\case Object v -> Just v; _ -> Nothing) (const Object)

key :: AsValue t => Text -> Traversal' t Value
key k = _Object . match (H.lookup k) (flip $ H.insert k)

key' :: AsValue t => Text -> Traversal' t (Maybe Value)
key' k = _Object . _at
    where _at f m = maybe (H.delete k m) (\v -> H.insert k v m) `fmap` f (H.lookup k m)

_Array :: AsValue t => Traversal' t (V.Vector Value)
_Array = _Value . match (\case Array v -> Just v; _ -> Nothing) (const Array)

nth :: AsValue t => Int -> Traversal' t Value
nth i = _Array . match (V.!? i) (\vs v -> vs V.// [(i, v)])

-- | It is possible to use string literal as a traversable with default value
instance ( a ~ a'
         , b ~ b'
         , FromJSON a, ToJSON a, Default a
         , AsValue b
         , Applicative f
         ) => IsString ((a -> f a') -> b -> f b')
    where
      fromString k = key' (fromString k) . non (toJSON (def :: a)) . _JSON
