{-# LANGUAGE OverloadedStrings #-}
module XMonad.Powerbar.Widgets.CurrencyRate
    ( getCurrencyRates
    ) where

import XMonad.Powerbar.Lens
import XMonad.Powerbar.Widgets.Common
import Data.List    (intercalate)
import Text.Printf  (printf)
import qualified Data.Aeson as A


getCurrencyRates :: String -> [String] -> IO (Maybe A.Value)
getCurrencyRates base symbols = firstOf _Value . invertRates <$> httpGet url
    where url = printf "http://api.fixer.io/latest?base=%s&symbols=%s" base (intercalate "," symbols)
          invertRates = key "rates" . _Object . traverse . _Double %~ roundN 3 . (1/)


-- | Round fractional to specified number of digits
roundN :: (RealFrac a, Floating a) => a -> a -> a
roundN n v = (/10**n) . fromIntegral . round $ v * 10**n


{-
currencyWidget :: String -> [String] -> X.X View
currencyWidget base symbols = 
-}
