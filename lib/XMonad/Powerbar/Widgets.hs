{-# LANGUAGE OverloadedStrings #-}
module XMonad.Powerbar.Widgets (widgetsFromConfig, module W) where

import XMonad.Powerbar.Widgets.Battery   as W
import XMonad.Powerbar.Widgets.Clock     as W
import XMonad.Powerbar.Widgets.Common    as W
import XMonad.Powerbar.Widgets.GMail     as W
import XMonad.Powerbar.Widgets.IBus      as W
import XMonad.Powerbar.Widgets.Icon      as W
import XMonad.Powerbar.Widgets.Perf      as W
import XMonad.Powerbar.Widgets.Separator as W
import XMonad.Powerbar.Widgets.Volume    as W
import XMonad.Powerbar.Widgets.Weather   as W
import XMonad.Powerbar.Widgets.Workspace as W
import XMonad.Powerbar.Widgets.CurrencyRate as W

import XMonad.Powerbar.Lens
import Data.Text            (Text)
import XMonad.Powerbar.View (View)
import qualified XMonad as X


-- | Create widget from config
--
-- Config format:
-- { "widgets": ["ibus", "perf", "weather", "gmail", "volume", "battery"],
--   "configs": {
--     "weather" : {"location": "Moscow"},
--     "ibus"    : {"engines": [["xkb:us::eng", "EN"], ["m17n:ru:translit", "RU"]]},
--     "gmail"   : {"login": "user@gmail.com", "pass": "my-super-secret-password"},
--   }
-- }
widgetsFromConfig :: AsValue c => c -> X.X [View]
widgetsFromConfig cfg = sequence $ cfg ^.. key "widgets"
                                         . _Array
                                         . traverse
                                         . _String
                                         . to fromConfig
                                         . traverse
    where fromConfig :: Text -> Maybe (X.X View)
          fromConfig "perf"    = Just W.perfWidget
          fromConfig "battery" = Just W.batteryWidget
          fromConfig "volume"  = Just W.volumeWidget
          fromConfig "ibus"    = cfg ^? key "configs"
                                      . key "ibus"
                                      . key "engines"
                                      . _JSON
                                      . to iBusWidget
          fromConfig "weather" = cfg ^? key "configs"
                                      . key "weather"
                                      . key "location"
                                      . _JSON
                                      . to weatherWidget
          fromConfig "gmail"   = do auth  <- cfg ^? key "configs" . key "gmail"
                                    login <- auth ^? key "login" . _JSON
                                    pass  <- auth ^? key "pass" . _JSON
                                    return $ gmailWidget login pass
          fromConfig _         = Nothing
