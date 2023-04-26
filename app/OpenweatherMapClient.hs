module OpenweatherMapClient where

import ApiKey (apiKey)
import Data.Time.LocalTime (TimeZone, getCurrentTimeZone)
import WeatherData (WeatherData (..))
import WeatherDataConverter (currentWeatherToWeatherData, forecastWeatherToWeatherDataList)
import Web.OpenWeatherMap.Client (getForecast, getWeather)
import Web.OpenWeatherMap.Types.City (City (timezone))
import qualified Web.OpenWeatherMap.Types.Location as OWL (Location (..))

data LocationReq
  = ByCityName String
  | ByCoordinates Double Double

toLocation :: LocationReq -> OWL.Location
toLocation req = case req of
  ByCityName name -> OWL.Name name
  ByCoordinates lat lon -> OWL.Coord lat lon

getWeather :: LocationReq -> IO (Maybe WeatherData)
getWeather req = do
  let location = toLocation req
  response <- Web.OpenWeatherMap.Client.getWeather apiKey location
  timezone <- getCurrentTimeZone
  case response of
    Left _ -> return Nothing
    Right cw -> return $ Just $ currentWeatherToWeatherData cw timezone

getForecast :: LocationReq -> IO (Maybe [WeatherData])
getForecast req = do
  let location = toLocation req
  response <- Web.OpenWeatherMap.Client.getForecast apiKey location
  timezone <- getCurrentTimeZone
  case response of
    Left _ -> return Nothing
    Right fw -> return $ Just $ forecastWeatherToWeatherDataList fw timezone