{-# LANGUAGE OverloadedStrings, TupleSections, DeriveDataTypeable #-}
module XMonad.Powerbar.Widgets.Weather
    ( getWeather
    , weatherWidget
    , weatherQuery
    , parseIcon
    , parseTemp
    ) where

import XMonad.Powerbar.Lens
import XMonad.Powerbar.View
import XMonad.Powerbar.Async
import XMonad.Powerbar.Style
import XMonad.Powerbar.Widgets.Common
import XMonad.Powerbar.Widgets.Separator (foldQuery)
import XMonad.Powerbar.Widgets.Icon (iconCell)
import Control.Monad       (when)
import Control.Monad.Trans (lift)
import Data.List           (sort)
import Data.Maybe          (fromMaybe)
import Data.Monoid         ((<>))
import Data.Text           (unpack)
import Data.Typeable       (Typeable)
import Text.Printf         (printf)
import qualified XMonad    as X

data WeatherState = WeatherState
    { _needUpdate :: Bool
    , _weather    :: Maybe Value
    } deriving (Show, Typeable)


needUpdate :: Lens' WeatherState Bool
needUpdate f v = (\c -> v {_needUpdate = c}) `fmap` f (_needUpdate v)
weather :: Lens' WeatherState (Maybe Value)
weather f v = (\c -> v {_weather = c}) `fmap` f (_weather v)


-- | Convert name of the city to its weather url
-- See http://openweathermap.org/weather-data for format description
weatherUrl :: String -> String
weatherUrl = ("http://api.openweathermap.org/data/2.5/weather?q=" ++)


-- | Get weather for specified city
getWeather :: String -> IO (Maybe Value)
getWeather = fmap (firstOf _Value) . httpGet . weatherUrl


-- | Weather widget for specified city
weatherWidget :: String -> X.X View
weatherWidget city = widget name 300 weatherQuery (WeatherState True Nothing) $
                     do -- update weather if needed
                        u <- needUpdate %%= (,True)
                        when u . lift . lift $ update
                        -- construct view
                        w <- fromMaybe Null `fmap` use weather
                        let t = maybe "" (printf "%0.f\176C") (parseTemp w)
                        return . VAction VButtonRight update
                               . VAttr ".weather" (toJSON w)
                               $ VText t
    where name = "Weather"
          -- weather state lens
          state :: Traversal' X.XState WeatherState
          state = widgetState' name (WeatherState True Nothing)
          -- update widget state and force update
          update :: X.X ()
          update = asyncRun $ do
                     state . weather <~ async (getWeather city)
                     state . needUpdate .= False
                     lift $ widgetUpdate name


-- | Style weather widget
weatherQuery :: StyleQuery ()
weatherQuery =
    do w <- matchAttr ".weather"
       v <- foldQuery
       let refreshView = do
             fontSize += 4
             iconCell "climacon-cloud-refresh"
           weatherView = do
             -- temperature
             t <- VText . printf "%0.f\176C" <$> parseTemp w
             -- icon
             i <- (\n -> VAttr ".icon" (toJSON n) VEmpty) <$> parseIcon w
             -- description
             d <- firstOf (idx (0 :: Int)) . sort $
                 w ^.. key "weather" . _Array . traverse
                     . key "description" . _String . to ((" "++) . unpack)
             return . replace $ i <> t <> if v then textSpaced d else VEmpty
       fromMaybe refreshView weatherView


-- | Parse weather end return temperature in celcius
parseTemp :: Value -> Maybe Double
parseTemp = firstOf $ key "main" . key "temp" . _Double . to (subtract 273.16)


-- | Parse weather and return icon name
parseIcon :: Value -> Maybe String
parseIcon w = do
  code    <- firstOf (idx (0 :: Int)) . sort $
            w ^.. key "weather" . _Array . traverse . key "id" . _Integral
  now     <- w ^? key "dt" . _Integral :: Maybe Int
  sunset  <- w ^? key "sys" . key "sunset"  . _Integral :: Maybe Int
  sunrise <- w ^? key "sys" . key "sunrise" . _Integral :: Maybe Int
  let (name, withDN) = weatherCodeName code
      dn = if now > sunrise && now < sunset
           then "-sun"
           else "-moon"
  return $ "climacon" ++ if withDN then name ++ dn else name


-- weather conditions http://openweathermap.org/weather-conditions
weatherCodeName :: Integer -> (String, Bool)
weatherCodeName c
    | c >= 200 && c < 300             = ("-lightning"        , True)
    | c >= 300 && c < 400             = ("-drizzle"          , True)
    | c `elem` [500,501,520]        = ("-rain"             , True)
    | c `elem` [502,521]            = ("-showers"          , True)
    | c `elem` [503,504,522,531]    = ("-downpour"         , True)
    | c `elem` [511,611,612,616]    = ("-sleet"            , True)
    | c `elem` [600,601,620]        = ("-flurries"         , True)
    | c `elem` [602,621,622]        = ("-snow"             , True)
    | c `elem` [701,711,721]        = ("-haze"             , True)
    | c `elem` [741]                = ("-fog"              , True)
    | c `elem` [731,751,761,762
               ,771,901,902,905]
      || c > 953                     = ("-wind"             , True)
    | c `elem` [781,900]            = ("-tornado"          , False)
    | c == 800 || (c > 950 && c < 954) = (""                 , True) -- sun or moon
    | c `elem` [801,802,803,804]    = ("-cloud"            , True)
    | c == 906                       = ("-hail"             , True)
    | c == 903                       = ("-snowflake"        , False)
    | c == 904                       = ("-thermometer-full" , False)
    | otherwise                     = ("-unknown"          , False)
