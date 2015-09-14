{-# LANGUAGE OverloadedStrings #-}
module XMonad.Powerbar.Widgets.Clock
    ( clockWidget
    , clockQuery
    ) where

import XMonad.Powerbar.Lens
import XMonad.Powerbar.View
import XMonad.Powerbar.Style
import Data.Monoid   ((<>))
import Data.Time     (ZonedTime, utcToLocalZonedTime, getCurrentTime, formatTime,defaultTimeLocale )
import Data.Text     (pack)
import qualified XMonad as X


clockWidget :: X.X View
clockWidget = do
  now <- X.io $ utcToLocalZonedTime =<< getCurrentTime
  return $ VAttr ".clock" (toJSON now) (VText . show $ now)


clockQuery :: StyleQuery ()
clockQuery = do now <- matchAttr' ".clock" _JSON :: StyleQuery ZonedTime
                fontFamily .= "Cuprum"
                let time = formatTime defaultTimeLocale "%a %R" now
                    date = pack $ formatTime defaultTimeLocale "%d" now
                replace $ VText time <> VAttr ".icon" (String $ "leksico-" <> date) VEmpty
