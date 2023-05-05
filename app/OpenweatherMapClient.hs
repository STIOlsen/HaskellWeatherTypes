module OpenweatherMapClient where

import ApiKey (apiKey)
import Data.Time.LocalTime (TimeZone, getCurrentTimeZone)
import System.IO (hFlush, stdout)
import WeatherData (WeatherData (..))
import WeatherDataConverter (currentWeatherToWeatherData, forecastWeatherToWeatherDataList)
import Web.OpenWeatherMap.Client (getForecast, getWeather)
import Web.OpenWeatherMap.Types.City (City (timezone))
import qualified Web.OpenWeatherMap.Types.Location as OWL (Location (..))

data LocationReq
  = ByCityName String
  | ByCoordinates Double Double

-- | Convert a location request to an OpenWeatherMap location.
toLocation :: LocationReq -> OWL.Location
toLocation req = case req of
  ByCityName name -> OWL.Name name
  ByCoordinates lat lon -> OWL.Coord lat lon

-- | Get the current weather for the given location.
getWeather :: LocationReq -> IO (Maybe WeatherData)
getWeather req = do
  let location = toLocation req
  response <- Web.OpenWeatherMap.Client.getWeather apiKey location
  timezone <- getCurrentTimeZone -- users timezone
  case response of
    Left _ -> return Nothing
    Right cw -> return $ Just $ currentWeatherToWeatherData cw timezone

-- | Get the forecast for the given location.
getForecast :: LocationReq -> IO (Maybe [WeatherData])
getForecast req = do
  let location = toLocation req
  response <- Web.OpenWeatherMap.Client.getForecast apiKey location
  timezone <- getCurrentTimeZone
  case response of
    Left _ -> return Nothing
    Right fw -> return $ Just $ forecastWeatherToWeatherDataList fw timezone

-- | Prompt the user for a location request.
getWeatherLocation :: IO LocationReq
getWeatherLocation = do
  putStrLn "How would you like to look up the weather?"
  putStrLn "1. By city name"
  putStrLn "2. By coordinates"
  putStr "Enter the number corresponding to your choice: "
  hFlush stdout -- Ensure the prompt is displayed before waiting for input
  choice <- getLine
  case choice of
    "1" -> getByCityName
    "2" -> getByCoordinates
    _ -> do
      fail "Invalid choice. Exiting."

-- | Prompt the user for a city name and return a location request.
getByCityName :: IO LocationReq
getByCityName = do
  putStrLn "Enter the city name:"
  ByCityName <$> getLine

-- | Prompt the user for coordinates and return a location request.
getByCoordinates :: IO LocationReq
getByCoordinates = do
  putStrLn "Enter the latitude:"
  latitude <- read <$> getLine
  putStrLn "Enter the longitude:"
  longitude <- read <$> getLine
  return $ ByCoordinates latitude longitude