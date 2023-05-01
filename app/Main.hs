{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import ApiKey (apiKey)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock
import Data.Time.LocalTime (timeToTimeOfDay)
import OpenweatherMapClient (LocationReq (..), getForecast, getWeather)
import Parser (pWeatherCondition, parseWeatherConditions)
import System.IO (hFlush, stdout)
import Temperature
import Text.Megaparsec (parse)
import UmbrellaCondition (UmbrellaCondition (..), umbrellaCondition, umbrellaConditions)
import WeatherConditions (WeatherCondition (..))
import WeatherData (WeatherData (..))
import WeatherDataUtils

main :: IO ()
main =
  do
    putStrLn "How would you like to look up the weather?"
    putStrLn "1. By city name"
    putStrLn "2. By coordinates"
    putStr "Enter the number corresponding to your choice: "
    hFlush stdout -- Ensure the prompt is displayed before waiting for input
    choice <- getLine

    request <- case choice of
      "1" -> byCityName
      "2" -> byCoordinates
      _ -> do
        putStrLn "Invalid choice. Exiting."
        return $ ByCityName "" -- Dummy request to satisfy the type checker
    weatherDataIO <- getWeather request
    forcastIO <- getForecast request
    case weatherDataIO of
      Nothing -> putStrLn "Failed to fetch weather data."
      Just wd -> do
        print ("--------- Current weather -----------")
        print wd
        putStrLn ("umbrella recomendation:")
        print $ umbrellaCondition wd

    case forcastIO of
      Nothing -> putStrLn "Failed to fetch forcast data."
      Just forcasts -> do
        print ("---------- Forcast (only the first one ) ----------")
        print $ head forcasts
        print ("--------------------")
        print ("Average Temperature of Forcasts:")
        print $ averageTemperature forcasts (Celsius 0)
        print ("Wind Speed Range:")
        print $ windSpeedRange forcasts
        print ("Max Humidity:")
        print $ maxHumidity forcasts
        print ("Get day hourly forcast:")
        print $ getAverageTemperatures forcasts
        print ("--------------------")
        print ("Get day by day forcast:")
        print $ getDayByDayForecasts forcasts
        print ("--------------------")
        print ("Umbrella recomendation:")
        print $ map (\(date, wds) -> (date, umbrellaConditions wds)) (getDayByDayForecasts forcasts)

byCityName :: IO LocationReq
byCityName = do
  putStrLn "Enter the city name:"
  cityName <- getLine
  return $ ByCityName cityName

byCoordinates :: IO LocationReq
byCoordinates = do
  putStrLn "Enter the latitude:"
  latitude <- read <$> getLine
  putStrLn "Enter the longitude:"
  longitude <- read <$> getLine
  return $ ByCoordinates latitude longitude
