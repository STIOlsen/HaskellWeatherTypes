module UmbrellaCondition where

import WeatherConditions (WeatherCondition (..))
import WeatherData (WeatherData (..))
import WeatherDataUtils (getHour)

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

umbrellaCondition :: WeatherData -> UmbrellaCondition
umbrellaCondition weatherData =
  case (isUmbrellaNecessary weatherData, wind) of
    (True, speed) ->
      case speed of
        s | s < 4 -> UmbrellaCondition Umbrella wind weather
        s | s > 4 && s < 8 -> UmbrellaCondition CautionUmbrella wind weather
        s | s >= 8 -> UmbrellaCondition RainGear wind weather
    _ -> UmbrellaCondition NoUmbrella wind weather
  where
    isUmbrellaNecessary wd =
      any
        ( ( \x -> case x of
              Rain _ -> True
              Drizzle _ -> True
              Thunderstorm _ -> True
              _ -> False
          )
        )
        (weatherDescriptions wd)
    weatherDescriptions = WeatherData.weatherDescriptions
    wind = WeatherData.windSpeed weatherData
    weather = WeatherData.weatherDescriptions weatherData

umbrellaConditions :: [WeatherData] -> UmbrellaCondition
umbrellaConditions weatherData =
  let filteredWeatherData = filter (\wd -> getHour wd >= 7 && getHour wd <= 16) weatherData
      umbrellaConditionsList = map umbrellaCondition filteredWeatherData
   in maximum umbrellaConditionsList

instance Ord UmbrellaCondition where
  compare (UmbrellaCondition recommendation wind _) (UmbrellaCondition recommendation' wind' _) =
    case compare recommendation recommendation' of
      EQ -> case compare wind wind' of
        EQ -> EQ
        LT -> LT
        GT -> GT
      LT -> LT
      GT -> GT

instance Ord UmbrellaRecommendation where
  compare RainGear RainGear = EQ
  compare CautionUmbrella CautionUmbrella = EQ
  compare Umbrella Umbrella = EQ
  compare NoUmbrella NoUmbrella = EQ
  compare RainGear _ = GT
  compare CautionUmbrella RainGear = LT
  compare CautionUmbrella _ = GT
  compare Umbrella RainGear = LT
  compare Umbrella CautionUmbrella = LT
  compare Umbrella _ = GT
  compare NoUmbrella _ = LT

{-
instance Show UmbrellaCondition where
  show (UmbrellaCondition recommendation wind weather) =
    case recommendation of
      Umbrella -> "Bring an umbrella." ++ "\n" ++ "Wind Speed: " ++ show wind ++ "m/s" ++ "\n" ++ "Weather: " ++ show weather
      RainGear -> "Bring rain gear." ++ "\n" ++ "Wind Speed: " ++ show wind ++ "m/s" ++ "\n" ++ "Weather: " ++ show weather
      CautionUmbrella -> "Bring an umbrella, but be aware of winds." ++ "\n" ++ "Wind Speed: " ++ show wind ++ "m/s" ++ "\n" ++ "Weather: " ++ show weather
      NoUmbrella -> "No umbrella needed." ++ " Weather: " ++ show weather'

-}
instance Show UmbrellaCondition where
  show (UmbrellaCondition recommendation wind weather) =
    case recommendation of
      Umbrella -> "Bring an umbrella."
      RainGear -> "Bring rain gear."
      CautionUmbrella -> "Bring an umbrella, but be aware of winds."
      NoUmbrella -> "No umbrella needed."