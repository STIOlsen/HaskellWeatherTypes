module UmbrellaCondition where

import Data.List (groupBy)
import WeatherConditions (WeatherCondition (..))
import WeatherData (WeatherData (..))
import WeatherDataUtils (Date, Hour, HourRange, getHour)

data UmbrellaCondition = UmbrellaCondition
  { recommendation :: UmbrellaRecommendation,
    wind :: Float,
    weather :: [WeatherCondition]
  }
  deriving (Eq)

data UmbrellaRecommendation
  = RainGear
  | CautionUmbrella
  | Umbrella
  | NoUmbrella
  deriving (Eq, Show)

-- | The UmbrellaCondition is determined by the weather and wind speed
umbrellaCondition :: WeatherData -> UmbrellaCondition
umbrellaCondition weatherData =
  case (any isUmbrellaNecessary weather, wind) of
    (True, speed) ->
      case speed of
        s | s < 4 -> UmbrellaCondition Umbrella wind weather
        s | s >= 4 && s <= 8 -> UmbrellaCondition CautionUmbrella wind weather
        s | s > 8 -> UmbrellaCondition RainGear wind weather
        _ -> error "Invalid wind speed."
    _ -> UmbrellaCondition NoUmbrella wind weather
  where
    weatherDescriptions = WeatherData.weatherDescriptions
    wind = WeatherData.windSpeed weatherData
    weather = WeatherData.weatherDescriptions weatherData

isUmbrellaNecessary :: WeatherCondition -> Bool
isUmbrellaNecessary (Rain _) = True
isUmbrellaNecessary (Drizzle _ (Just c)) = True
isUmbrellaNecessary (Thunderstorm _ (Just c)) = isUmbrellaNecessary c
isUmbrellaNecessary (Snow _ (Just c)) = isUmbrellaNecessary c
isUmbrellaNecessary _ = False

calculateHourlyUmbrellaConditions :: [WeatherData] -> [(Hour, UmbrellaCondition)]
calculateHourlyUmbrellaConditions weatherData =
  let umbrellaConditionsList = map umbrellaCondition weatherData
      weather = WeatherData.weatherDescriptions <$> weatherData
   in zipWith (\x y -> (getHour x, y)) weatherData umbrellaConditionsList

forecastUmbrellaConditions :: [(Date, [WeatherData])] -> [(Date, [(HourRange, UmbrellaCondition)])]
forecastUmbrellaConditions forecasts =
  let hourlyUmbrellaConditions = map (\(date, wds) -> (date, calculateHourlyUmbrellaConditions wds)) forecasts
      filterNoUmbrellaHours = filter (\(_, umbrellaCond) -> recommendation umbrellaCond /= NoUmbrella)
      noUmbrellaForecasts = map (\(date, list) -> (date, filterNoUmbrellaHours list)) hourlyUmbrellaConditions
      combinedUmbrellaConditions = map (\(date, list) -> (date, condenseUmbrellaConditionsbyHourRange list)) noUmbrellaForecasts
   in combinedUmbrellaConditions

-- | Condenses a list of hourly umbrella conditions into a list of hourly ranges and umbrella conditions
condenseUmbrellaConditionsbyHourRange :: [(Hour, UmbrellaCondition)] -> [(HourRange, UmbrellaCondition)]
condenseUmbrellaConditionsbyHourRange [] = []
condenseUmbrellaConditionsbyHourRange [(hour, umbrellaCond)] = [((hour, hour + 1), umbrellaCond)]
condenseUmbrellaConditionsbyHourRange ((hour, umbrellaCondition) : (hour_2, umreallaCond_2) : rest) =
  if recommendation umbrellaCondition == recommendation umreallaCond_2
    then condenseUmbrellaConditionsbyHourRange ((hour, umbrellaCondition) : rest)
    else ((hour, hour_2), umbrellaCondition) : condenseUmbrellaConditionsbyHourRange ((hour_2, umreallaCond_2) : rest)

instance Show UmbrellaCondition where
  show (UmbrellaCondition recommendation wind weather) =
    case recommendation of
      Umbrella -> "Bring an umbrella."
      RainGear -> "Bring rain gear."
      CautionUmbrella -> "Bring an umbrella, but be aware of winds."
      NoUmbrella -> "No umbrella needed."