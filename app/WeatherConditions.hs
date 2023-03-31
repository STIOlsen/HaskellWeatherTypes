module WeatherConditions where

data Intencity = Light | Extreme | Heavy | Very | Ragged | Shower | Moderate | Freezing

data WeatherCondition
  = Rain [Intencity]
  | Drizzle [Intencity] (Maybe WeatherCondition)
  | Snow [Intencity]
  | Sleet [Intencity]
  | Thunderstorm [Intencity] (Maybe WeatherCondition)
  | Atmosphere AtmosphereType
  | Clear ClearType
  | Clouds CloudsType

data AtmosphereType = Fog | Mist | Smoke | Haze | Dust | Sand | VolcanicAsh | Squall | Tornado | SandDustWhirls
  deriving (Show, Eq)

data ClearType = ClearSky
  deriving (Show, Eq)

data CloudsType = Few | Scattered | Broken | Overcast
  deriving (Show, Eq)

instance Show Intencity where
  show Light = "light"
  show Extreme = "extreme"
  show Heavy = "heavy"
  show Very = "very"
  show Ragged = "ragged"
  show Shower = "shower"
  show Moderate = "moderate"
  show Freezing = "freezing"

instance Show WeatherCondition where
  show (Rain intencities) = "(" ++ "Rain " ++ show intencities ++ ")"
  show (Drizzle intencities weather) = "(" ++ "Drizzle " ++ show intencities ++ " (" ++ show weather ++ "))"
  show (Snow intencities) = "(" ++ "Snow " ++ show intencities ++ ")"
  show (Sleet intencities) = "(" ++ "Sleet " ++ show intencities ++ ")"
  show (Thunderstorm intencities weather) = "(" ++ "Thunderstorm " ++ show intencities ++ " " ++ show weather ++ ")"
  show (Atmosphere atmosphereType) = "(" ++ "Atmosphere " ++ show atmosphereType ++ ")"
  show (Clear clearType) = "(" ++ "Clear " ++ show clearType ++ ")"
  show (Clouds cloudsType) = "(" ++ "Clouds " ++ show cloudsType ++ ")"
