module Main where

import OpenweatherMapClient (getForecast, getWeather, getWeatherLocation)
import Temperature (Temperature (..))
import UmbrellaCondition (UmbrellaCondition (..), forecastUmbrellaConditions, umbrellaCondition)
import WeatherDataUtils (averageTemperature, getDayByDayForecasts, windSpeedRange)

main :: IO ()
main =
  do
    request <- getWeatherLocation
    weatherDataIO <- getWeather request
    forcastIO <- getForecast request
    case (weatherDataIO, forcastIO) of
      (Nothing, Nothing) -> fail "Failed to fetch weather data."
      (Nothing, Just _) -> fail "Failed to fetch current weather data."
      (Just _, Nothing) -> fail "Failed to fetch forcast weather data."
      (Just current, Just forcasts) -> do
        putStrLn ""
        putStrLn "Umberella recomendation for today:"
        print $ umbrellaCondition current
        putStrLn ""
        putStrLn "Average Temperature of forcasts:"
        print $ averageTemperature forcasts (Celsius 0)
        putStrLn ""
        putStrLn "Wind Speed Range of forcast in m/s:"
        print $ windSpeedRange forcasts
        putStrLn ""
        putStrLn "Umbrella recomendation forward :"
        putStr $ unlines $ map show forcastUmbrella
        where
          forcastUmbrella = forecastUmbrellaConditions $ getDayByDayForecasts forcasts
