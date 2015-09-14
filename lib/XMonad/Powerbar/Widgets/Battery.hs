{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module XMonad.Powerbar.Widgets.Battery
    ( batteryWidget
    , batteryQuery
    ) where

import XMonad.Powerbar.Style
import XMonad.Powerbar.View
import XMonad.Powerbar.Lens
import Data.Int                       (Int64)
import Data.Monoid                    ((<>))
import Data.Text                      (unpack)
import Data.Typeable                  (Typeable)
import Text.Printf                    (printf)
import XMonad.Powerbar.Widgets.Separator (foldQuery)
import XMonad.Powerbar.Widgets.Common (widget, widgetMaybe, widgetMaybeIO)
import qualified XMonad as X
import qualified Data.Aeson as A
import qualified System.Information.Battery as B


newtype BatteryState = BatteryState { _ctx :: Maybe B.BatteryContext
                                    } deriving Typeable

ctx :: Lens' BatteryState (Maybe B.BatteryContext)
ctx f v = (\c -> v {_ctx = c}) `fmap` f (_ctx v)

-- | Show current battery status
batteryWidget :: X.X View
batteryWidget =
    widget "Battery" 60 batteryQuery (BatteryState Nothing) $ do
      -- get battery info
      c <- widgetMaybe (use ctx) <|> widgetMaybeIO B.batteryContextNew
      i <- widgetMaybeIO $ B.getBatteryInfo c
      ctx .= Just c
      -- construct view
      let charing = case B.batteryState i of
                      B.BatteryStateCharging -> True
                      _                      -> False
          time    = if charing
                    then B.batteryTimeToFull i
                    else B.batteryTimeToEmpty i
          state   = A.object [ "charging"   A..= charing
                             , "time"       A..= time
                             , "time-hours" A..= secToHours time
                             , "percentage" A..= B.batteryPercentage i
                             ]
      return $ VAttr ".battery" state VEmpty


-- | Query for battery widget
batteryQuery :: StyleQuery ()
batteryQuery = do
  state <- matchAttr ".battery"
  fold  <- foldQuery
  let percName p | p > 75    = "full"
                 | p > 50    = "high"
                 | p > 25    = "mid"
                 | otherwise = "low"
      battery = do
        charging <- state ^? key "charging" . _Bool
        time     <- state ^? key "time-hours" . _String
        perc     <- state ^? key "percentage" . _Double
        let icon :: String
            icon = if charging
                   then "typcn-plug"
                   else "typcn-battery-" ++ percName perc
        return $ VAttr ".icon" (toJSON icon) VEmpty
               <> if fold
                  then VText (unpack time)
                  else VEmpty
  maybe (return ()) replace battery


-- | Convert seconds to hours
secToHours :: Int64 -> String
secToHours s
    | s > 3600  = let (h, s') = s `divMod` 3600
                  in printf "%dh%s" h $ secToHours s'
    | s > 60    = let (m, _) = s `divMod` 60
                  in printf "%dm" m
    | otherwise = printf "%ds" s
