{-# LANGUAGE LambdaCase #-}
module XMonad.Powerbar.View
    ( View(..)
    , _VLeft
    , _VRight
    , _VText
    , _View
    , ViewButton(..)
    , textSpaced
    ) where

import XMonad.Powerbar.Lens
import Text.Printf         (printf)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B
import qualified XMonad as X


-- | Renderable view tree
data View  = VPair !View !View                   -- ^ Monoid.mappend
           | VEmpty                              -- ^ Monoid.empty
           | VAction !ViewButton (X.X ()) !View  -- ^ Associate action with child view
           | VAttr !String !A.Value !View        -- ^ Associate attribute with child view
           | VText !String                       -- ^ View containing text

_VLeft :: Traversal' View View
_VLeft = match (\case VPair v _ -> Just v; _ -> Nothing) (\(VPair _ r) l -> VPair l r)

_VRight :: Traversal' View View
_VRight = match (\case VPair _ v -> Just v; _ -> Nothing) (\(VPair l _) r -> VPair l r)

_VText :: Traversal' View String
_VText = match (\case VText t -> Just t; _ -> Nothing) (const VText)

_View :: Traversal' View View
_View f (VPair l r)     = VPair <$> f l <*> f r
_View f (VAction b a v) = VAction b a <$> f v
_View f (VAttr n a v)   = VAttr n a <$> f v
_View _ v               = pure v

instance Show View where
    show (VPair VEmpty v) = show v
    show (VPair v VEmpty) = show v
    show (VPair v v'    ) = printf "%s %s" (show v) (show v')
    show VEmpty           = "E"
    show (VAction b _ v ) = printf "(A button:%s %s)" (show b) (show v)
    show (VAttr an av v ) = printf "(T %s:%s %s)" an (B.unpack . A.encode $ av) (show v)
    show (VText t       ) = printf "\"%s\"" t


instance Monoid View where
    mempty  = VEmpty
    mappend = VPair


data ViewButton = VButtonLeft
                | VButtonMiddle
                | VButtonRight
                | VButtonScrollUp
                | VButtonScrollDown
                  deriving (Show, Enum)


-- | Create text view enclosed in spaces
textSpaced :: String -> View
textSpaced text = VAttr ".spaced" Null (VText . printf " %s " $ text)
