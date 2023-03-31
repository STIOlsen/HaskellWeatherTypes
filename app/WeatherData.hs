{-# LANGUAGE RecordWildCards #-}

module WeatherData
  ( WeatherData (..),
    averageTemperature,
  )
where

import Data.List (groupBy, intercalate)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, toGregorian, utctDay)
import Data.Time.Calendar (Day, toGregorian)
import Data.Time.Clock
import Data.Time.Clock (UTCTime (..), utctDay)
import Data.Time.LocalTime (TimeOfDay (..), TimeZone (..), getCurrentTimeZone, localTimeOfDay, timeZoneMinutes, utcToLocalTime)
import Temperature (Temperature (..), toCelsius, toFahrenheit, toKelvin)
import Text.Printf (printf)

data WeatherData = WeatherData
  { date :: UTCTime,
    forcastTime :: Maybe UTCTime,
    location :: (Maybe Float, Maybe Float),
    country :: Maybe String,
    temperature :: Temperature,
    humidity :: Float,
    pressure :: Float,
    windSpeed :: Float,
    windDirection :: Float,
    weatherDescriptions :: [String],
    weatherIcons :: [String]
  }

instance Show WeatherData where
  show (WeatherData {..}) =
    "Date: " ++ show date ++ "\n"
      ++ "Forcast Time: "
      ++ maybe "N/A" show forcastTime
      ++ "\n"
      ++ "Location: "
      ++ show location
      ++ "\n"
      ++ "Country: "
      ++ maybe "N/A" id country
      ++ "\n"
      ++ "Temperature: "
      ++ show temperature
      ++ "\n"
      ++ "Humidity: "
      ++ show humidity
      ++ "%\n"
      ++ "Pressure: "
      ++ show pressure
      ++ "hPa\n"
      ++ "Wind Speed: "
      ++ show windSpeed
      ++ "m/s\n"
      ++ "Wind Direction: "
      ++ show windDirection
      ++ "°\n"
      ++ "Weather Descriptions: "
      ++ show weatherDescriptions
      ++ "\n"
      ++ "Weather Icons: "
      ++ show weatherIcons
      ++ "\n"

averageTemperature :: [WeatherData] -> Temperature -> Temperature
averageTemperature [] _ = error "No weather data"
averageTemperature weatherData unit = case unit of
  (Celsius _) -> toCelsius average
  (Kelvin _) -> toKelvin average
  (Fahrenheit _) -> toFahrenheit average
  where
    average = totalSum / fromIntegral (length weatherData)
    totalSum = sum (map (toCelsius . temperature) weatherData)
