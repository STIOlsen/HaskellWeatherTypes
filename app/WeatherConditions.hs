module WeatherConditions where

data WeatherCondition
  = Rain String
  | Drizzle String
  | Snow String
  | Sleet String
  | Thunderstorm String
  | Atmosphere String
  | Clear String
  | Clouds String
  deriving (Eq)

atmosphereTypes :: [String]
atmosphereTypes =
  [ "mist",
    "smoke",
    "haze",
    "sand",
    "dust",
    "fog",
    "ash",
    "squall",
    "tornado"
  ]

type ConditionConstructor = String -> WeatherCondition

conditionTable :: [(String, ConditionConstructor)]
conditionTable =
  [ ("rain", Rain),
    ("drizzle", Drizzle),
    ("snow", Snow),
    ("sleet", Sleet),
    ("thunderstorm", Thunderstorm),
    ("clear", Clear),
    ("clouds", Clouds)
  ]

instance Show WeatherCondition where
  show (Rain str) = "Rain " ++ show str
  show (Drizzle str) = "Drizzle " ++ show str
  show (Snow str) = "Snow " ++ show str
  show (Sleet str) = "Sleet " ++ show str
  show (Thunderstorm str) = "Thunderstorm " ++ show str
  show (Atmosphere str) = "Atmosphere " ++ show str
  show (Clear str) = "Clear " ++ show str
  show (Clouds str) = "Clouds " ++ show str
