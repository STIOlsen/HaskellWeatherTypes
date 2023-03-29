module WeatherConditions where

data Intencity = Light | Extreme | Heavy | Very | Ragged | Shower | Moderate | Freezing

type Rain = [Intencity]

data Drizzle = Drizzle (Maybe Rain) [Intencity]

data Snow = Snow [Intencity] | Sleet [Intencity]

data Thunderstorm = ThunderstormRain [Intencity] Rain | ThunderstormDrizzle [Intencity] Drizzle

data Atmosphere = Fog | Mist | Smoke | Haze | Dust | Sand | VolcanicAsh | Squall | Tornado | SandDustWhirls
  deriving (Show, Eq)

data Clear = ClearSky
  deriving (Show, Eq)

data Clouds = Few | Scatterd | Broken | Overcast
  deriving (Show, Eq)

data WeatherConditions
  = Rain Rain
  | DrizzleCondition Drizzle
  | SnowCondition Snow
  | Thunderstorm Thunderstorm
  | Atmosphere Atmosphere
  | Clear Clear
  | Clouds Clouds
