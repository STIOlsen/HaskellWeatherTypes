module WeatherDataConverter (currentWeatherToWeatherData, forecastWeatherToWeatherDataList) where

import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.LocalTime (TimeZone (..))
import Parser (parseWeatherConditions)
import Temperature (Temperature (..), getCelsius, toCelsius)
import WeatherData (WeatherData (..))
import qualified Web.OpenWeatherMap.Types.City as OWCity (City (..))
import qualified Web.OpenWeatherMap.Types.Coord as OWCoord (Coord (..))
import Web.OpenWeatherMap.Types.CurrentWeather (CurrentWeather)
import qualified Web.OpenWeatherMap.Types.CurrentWeather as OWCW (CurrentWeather (..))
import qualified Web.OpenWeatherMap.Types.Forecast as OWF (Forecast (..))
import qualified Web.OpenWeatherMap.Types.ForecastWeather as OWFW (ForecastWeather (..))
import qualified Web.OpenWeatherMap.Types.Main as OWMain (Main (..))
import qualified Web.OpenWeatherMap.Types.Sys as OWSys (Sys (..))
import qualified Web.OpenWeatherMap.Types.Weather as OWWeather (Weather (..))
import qualified Web.OpenWeatherMap.Types.Wind as OWWind (Wind (..))

-- | Converts OpenWeatherMap CurrentWeather to WeatherData
currentWeatherToWeatherData :: OWCW.CurrentWeather -> TimeZone -> WeatherData
currentWeatherToWeatherData cw tz =
  WeatherData
    { date = epochIntToUTCTime $ OWCW.dt cw,
      timeZone = tz,
      location = (lon, lat),
      country = OWSys.country $ OWCW.sys cw,
      temperature = Temperature.toCelsius $ Temperature.Kelvin $ realToFrac (OWMain.temp $ OWCW.main cw),
      humidity = realToFrac $ round $ OWMain.humidity $ OWCW.main cw,
      pressure = realToFrac $ round $ OWMain.pressure $ OWCW.main cw,
      windSpeed = realToFrac $ OWWind.speed $ OWCW.wind cw,
      windDirection = realToFrac $ round $ OWWind.deg $ OWCW.wind cw,
      weatherDescriptions = parseWeatherConditions $ fmap (OWWeather.description) (OWCW.weather cw),
      weatherIcons = fmap (OWWeather.icon) (OWCW.weather cw)
    }
  where
    coord = OWCW.coord cw
    lat = case OWCoord.lat coord of
      Just lat -> Just $ realToFrac lat
      Nothing -> Nothing
    lon = case OWCoord.lon coord of
      Just lon -> Just $ realToFrac lon
      Nothing -> Nothing

-- | Converts OpenWeatherMap ForecastWeather to WeatherData list
forecastWeatherToWeatherDataList :: OWFW.ForecastWeather -> TimeZone -> [WeatherData]
forecastWeatherToWeatherDataList fw tz = map weatherDataForForecast (OWFW.list fw)
  where
    weatherDataForForecast w =
      WeatherData
        { date = epochIntToUTCTime $ OWF.dt w,
          timeZone = tz,
          location = (lon, lat),
          country = Nothing,
          temperature = Temperature.toCelsius $ Temperature.Kelvin $ realToFrac $ OWMain.temp $ OWF.main w,
          humidity = realToFrac $ round $ OWMain.humidity $ OWF.main w,
          pressure = realToFrac $ round $ OWMain.pressure $ OWF.main w,
          windSpeed = realToFrac $ OWWind.speed $ OWF.wind w,
          windDirection = realToFrac $ round $ OWWind.deg $ OWF.wind w,
          weatherDescriptions = parseWeatherConditions $ fmap (OWWeather.description) (OWF.weather w),
          weatherIcons = fmap (OWWeather.icon) (OWF.weather w)
        }
    coord = OWCity.coord $ OWFW.city fw
    lat = case OWCoord.lat coord of
      Just lat -> Just $ realToFrac lat
      Nothing -> Nothing
    lon = case OWCoord.lon coord of
      Just lon -> Just $ realToFrac lon
      Nothing -> Nothing

-- | Converts epoch time to UTCTime
epochIntToUTCTime :: Int -> UTCTime
epochIntToUTCTime epochTime = posixSecondsToUTCTime $ realToFrac epochTime
