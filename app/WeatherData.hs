{-# LANGUAGE RecordWildCards #-}

module WeatherData where

import Data.Time (TimeZone, UTCTime)
import Temperature (Temperature)
import WeatherConditions (WeatherCondition)

data WeatherData = WeatherData
  { date :: UTCTime,
    forcastTime :: Maybe UTCTime,
    timeZone :: TimeZone,
    location :: (Maybe Float, Maybe Float),
    country :: Maybe String,
    temperature :: Temperature,
    humidity :: Float,
    pressure :: Float,
    windSpeed :: Float,
    windDirection :: Float,
    weatherDescriptions :: [WeatherCondition],
    weatherIcons :: [String]
  }

instance Show WeatherData where
  show WeatherData {..} =
    unlines
      [ "Date: " ++ show date,
        "Forcast Time: " ++ maybe "N/A" show forcastTime,
        "Location: " ++ show location,
        "Country: " ++ maybe "N/A" id country,
        "Temperature: " ++ show temperature,
        "Humidity: " ++ show humidity ++ "%",
        "Pressure: " ++ show pressure ++ "hPa",
        "Wind Speed: " ++ show windSpeed ++ "m/s",
        "Wind Direction: " ++ show windDirection ++ "°",
        "Weather Descriptions: " ++ show weatherDescriptions,
        "Weather Icons: " ++ show weatherIcons
      ]
