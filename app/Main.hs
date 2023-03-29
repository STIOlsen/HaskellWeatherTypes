{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import ApiKey (apiKey)
import OpenweatherMapClient (LocationReq (..), getForecast, getWeather)
import System.IO (hFlush, stdout)
import Temperature
import WeatherData (WeatherData (..), averageTemperature)

main :: IO ()
main = do
  putStrLn "How would you like to look up the weather?"
  putStrLn "1. By city name"
  putStrLn "3. By coordinates"
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
    Just wd -> print wd
  case forcastIO of
    Nothing -> putStrLn "Failed to fetch forcast data."
    Just f@(forcast : forcasts) -> do
      print ("--------------------")
      print ("Forcast:")
      print $ forcast
      print ("--------------------")
      print ("Average Temperature:")
      print $ length f
      print $ averageTemperature f (Celsius 0)

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
