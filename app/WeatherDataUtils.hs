{-# LANGUAGE RecordWildCards #-}

module WeatherDataUtils where

import Data.List (groupBy, sort, sortOn)
import Data.Time (Day, UTCTime, defaultTimeLocale, formatTime, localDay, localTimeOfDay, toGregorian, todHour, utcToLocalTime, utctDayTime)
import Data.Time.Clock ()
import Temperature (Temperature (..), convertTemperature, getCelsius, toCelsius)
import WeatherData (WeatherData (..))

-- | Calculates the average temperature from a list of WeatherData
-- | The average temperature is returned in the specified unit
averageTemperature :: [WeatherData] -> Temperature -> Temperature
averageTemperature [] _ = error "No weather data"
averageTemperature weatherData unit = convertTemperature unit averageCelcius
  where
    averageCelcius = Celsius (sumCelsius / fromIntegral (length weatherData))
    temperatures = map temperature weatherData
    sumCelsius = sum (map (getCelsius . toCelsius) temperatures)

-- | Calculates windpeed range from a list of WeatherData
windSpeedRange :: [WeatherData] -> (Float, Float)
windSpeedRange [] = error "No weather data"
windSpeedRange weatherData =
  let windSpeeds = map windSpeed weatherData
      minWindSpeed = minimum windSpeeds
      maxWindSpeed = maximum windSpeeds
   in (minWindSpeed, maxWindSpeed)

-- | Maximum humidity from a list of WeatherData
maxHumidity :: [WeatherData] -> Float
maxHumidity [] = error "No weather data"
maxHumidity weatherData =
  let humidities = map humidity weatherData
   in maximum humidities

type Hour = Int

type HourRange = (Hour, Hour)

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
