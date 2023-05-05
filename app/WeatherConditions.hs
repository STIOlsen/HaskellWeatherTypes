{-  This module abtracts the weather conditions from openweather API
    into: WeatherCondition,Intencity, AtmosphereType and ClearType.
-}

module WeatherConditions where

data WeatherCondition
  = Rain [Intencity]
  | Drizzle [Intencity] (Maybe WeatherCondition)
  | Snow [Intencity] (Maybe WeatherCondition)
  | Thunderstorm [Intencity] (Maybe WeatherCondition)
  | Atmosphere AtmosphereType
  | Clear ClearType
  | Clouds [Intencity]
  deriving (Eq)

data Intencity
  = Light
  | Extreme
  | Heavy
  | Very
  | Ragged
  | Shower
  | Moderate
  | Freezing
  | Few
  | Scattered
  | Broken
  | Overcast
  deriving (Show, Eq)

data AtmosphereType
  = Fog
  | Mist
  | Smoke
  | Haze
  | Dust
  | Sand
  | VolcanicAsh
  | Squall
  | Tornado
  | SandDustWhirls
  deriving (Show, Eq)

data ClearType = ClearSky
  deriving (Show, Eq)

instance Show WeatherCondition where
  show (Rain intencities) = "(" ++ "Rain " ++ show intencities ++ ")"
  show (Drizzle intencities weather) = "(" ++ "Drizzle " ++ show intencities ++ " (" ++ show weather ++ "))"
  show (Snow intencities weather) = "(" ++ "Snow " ++ show intencities ++ " (" ++ show weather ++ "))"
  show (Thunderstorm intencities weather) = "(" ++ "Thunderstorm " ++ show intencities ++ " " ++ show weather ++ ")"
  show (Atmosphere atmosphereType) = "(" ++ "Atmosphere " ++ show atmosphereType ++ ")"
  show (Clear clearType) = "(" ++ "Clear " ++ show clearType ++ ")"
  show (Clouds intencity) = "(" ++ "Clouds " ++ show intencity ++ ")"