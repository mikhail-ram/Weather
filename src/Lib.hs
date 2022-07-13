{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( getConfig
    , getWeather
    ) where

import qualified Data.Text as T
import Network.Wreq
import Data.Aeson
import Control.Lens

data Config = Config { apiKey    :: T.Text
                     , latitude  :: T.Text
                     , longitude :: T.Text }


data WeatherResponse = WeatherResponse { weather     :: T.Text
                                       , description :: T.Text
                                       , temperature :: Double
                                       , feelsLike   :: Double
                                       , pressure    :: Double
                                       , humidity    :: Double
                                       , visibility  :: Double
                                       , cloudiness  :: Double
                                       , name        :: T.Text }

instance Show WeatherResponse where
    show response = string
      where
        string = T.unpack . T.concat $ [ bold . name $ response
                                       , "\\n"
                                       , bold . T.toTitle . description $ response
                                       , " at "
                                       , bold . joinDouble temperature $ "°C"
                                       , ", feels like "
                                       , bold . joinDouble feelsLike $ "°C"
                                       , ".\\nPressure at "
                                       , bold . joinDouble pressure $ "hPa"
                                       , " and humidity at "
                                       , bold . joinDouble humidity $ "%"
                                       , ".\\nVisibility is "
                                       , bold . joinDouble visibility $ "m"
                                       , " and cloudiness is "
                                       , bold . joinDouble cloudiness $ "%"
                                       , "." ]
        bold text = T.concat ["\\033[1m", text, "\\033[0m"]
        joinDouble f unit = T.pack $ show (f  response) ++ unit
    
instance FromJSON WeatherResponse where
    parseJSON = withObject "weather response" f
      where
        f o = do
            weatherO     <- o               .: "weather"
            weather      <- (head weatherO) .: "main"
            description  <- (head weatherO) .: "description"
            temperatureO <- o               .: "main"
            temperature  <- temperatureO    .: "temp"
            feelsLike    <- temperatureO    .: "feels_like"
            pressure     <- temperatureO    .: "pressure"
            humidity     <- temperatureO    .: "humidity"
            visibility   <- o               .: "visibility"
            cloudsO      <- o               .: "clouds"
            cloudiness   <- cloudsO         .: "all"
            name         <- o               .: "name"
            return WeatherResponse { weather     = weather
                                   , description = description
                                   , temperature = temperature
                                   , feelsLike   = feelsLike
                                   , pressure    = pressure
                                   , humidity    = humidity
                                   , visibility  = visibility
                                   , cloudiness  = cloudiness
                                   , name        = name }

getConfig :: String -> Config
getConfig file = Config { apiKey    = apiKey
                        , latitude  = latitude
                        , longitude = longitude }
  where
    splitStrip                    = (map T.strip) . T.splitOn "="
    pairs                         = map splitStrip . T.lines . T.pack $ file
    [apiKey, latitude, longitude] = map (!! 1) pairs

getWeather config = do
    r <- ((get . T.unpack) (endpoint))
    case decode (r ^. responseBody) of
        Just w  -> print (w :: WeatherResponse)
        Nothing -> fail "Weather JSON could not be parsed"
  where
    endpoint = T.concat
        [ "https://api.openweathermap.org/data/2.5/weather?lat="
        , latitude config
        , "&lon="
        , longitude config
        , "&appid="
        , apiKey config
        , "&units=metric" ]
