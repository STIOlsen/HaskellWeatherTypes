{-# LANGUAGE RecordWildCards #-}

module WeatherDataUtils
  ( averageTemperature,
    windSpeedRange,
    maxHumidity,
    getAverageTemperatures,
    getDayByDayForecasts,
    getDayHourlyForecasts,
    getHour,
  )
where

import Data.List (groupBy, sort, sortOn)
import Data.Time (Day, UTCTime, defaultTimeLocale, formatTime, localDay, localTimeOfDay, toGregorian, todHour, utcToLocalTime, utctDayTime)
import Data.Time.Clock
import Temperature (Temperature (..), convertTemperature, getCelsius, toCelsius)
import WeatherData (WeatherData (..))

averageTemperature :: [WeatherData] -> Temperature -> Temperature
averageTemperature [] _ = error "No weather data"
averageTemperature weatherData unit = convertTemperature unit averageCelcius
  where
    averageCelcius = Celsius (sumCelsius / fromIntegral (length weatherData))
    temperatures = map temperature weatherData
    sumCelsius = sum (map (getCelsius . toCelsius) temperatures)

getAverageTemperatures :: [WeatherData] -> [(String, [(Int, Temperature)])]
getAverageTemperatures forcasts =
  let dayHourlyForecasts = getDayHourlyForecasts forcasts
   in map (\(date, ws) -> (date, map (\(hour, hws) -> (hour, averageTemperature hws (Celsius 0))) ws)) dayHourlyForecasts

windSpeedRange :: [WeatherData] -> (Float, Float)
windSpeedRange [] = error "No weather data"
windSpeedRange weatherData =
  let windSpeeds = map windSpeed weatherData
      minWindSpeed = minimum windSpeeds
      maxWindSpeed = maximum windSpeeds
   in (minWindSpeed, maxWindSpeed)

maxHumidity :: [WeatherData] -> Float
maxHumidity [] = error "No weather data"
maxHumidity weatherData =
  let humidities = map humidity weatherData
   in maximum humidities

type Hour = Int

type Date = String

getHour :: WeatherData -> Hour
getHour wd = todHour . localTimeOfDay $ utcToLocalTime (timeZone wd) (date wd)

getHours :: [WeatherData] -> [Hour]
getHours = sort . map getHour

getHourlyForcasts :: [WeatherData] -> [(Hour, [WeatherData])]
getHourlyForcasts =
  map (\xs -> (getHour (head xs), xs))
    . groupBy (\w1 w2 -> getHour w1 == getHour w2)
    . sortOn getHour

getDay :: WeatherData -> Day
getDay wd = localDay $ utcToLocalTime (timeZone wd) (date wd)

getDayString :: WeatherData -> Date
getDayString = formatTime defaultTimeLocale "%Y-%m-%d" . getDay

getDayByDayForecasts :: [WeatherData] -> [(Date, [WeatherData])]
getDayByDayForecasts =
  map (\xs -> (getDayString (head xs), xs))
    . groupBy (\w1 w2 -> getDay w1 == getDay w2)
    . sortOn getDay

getDayHourlyForecasts :: [WeatherData] -> [(Date, [(Hour, [WeatherData])])]
getDayHourlyForecasts weeatherDatas =
  let dayByDayForecasts = getDayByDayForecasts weeatherDatas
      dayHourlyForecasts = map (\(day, xs) -> (day, getHourlyForcasts xs)) dayByDayForecasts
   in dayHourlyForecasts
