module Parser where

import Control.Monad (void)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char (letterChar, string)
import WeatherConditions (AtmosphereType (..), ClearType (..), Intencity (..), WeatherCondition (..))

type Parser a = Parsec Void String a

word :: Parser String
word = some letterChar

pIntencity :: Parser Intencity
pIntencity =
  do
    choice
      [ string "light" >> return Light,
        string "extreme" >> return Extreme,
        string "heavy" >> return Heavy,
        string "very" >> return Very,
        string "ragged" >> return Ragged,
        string "shower" >> return Shower,
        string "moderate" >> return Moderate,
        string "freezing" >> return Freezing,
        string "few" >> return Few,
        string "scattered" >> return Scattered,
        string "broken" >> return Broken,
        string "overcast" >> return Overcast
      ]

pAtmosphereType :: Parser AtmosphereType
pAtmosphereType =
  do
    choice
      [ string "mist" >> return Mist,
        string "smoke" >> return Smoke,
        string "haze" >> return Haze,
        string "sand" >> (SandDustWhirls <$ string "/") <|> return Sand,
        string "dust" >> return Dust,
        string "fog" >> return Fog,
        string "volcanic" >> return VolcanicAsh,
        string "squalls" >> return Squall,
        string "tornado" >> return Tornado
      ]

pClearType :: Parser ClearType
pClearType =
  do
    string "clear sky" >> return ClearSky

pIntencities :: Parser [Intencity]
pIntencities =
  do
    intencities <- many (try (optional space >> pIntencity))
    optional space
    optional $ string "intensity"
    optional space
    intensities_1 <- many (try (optional space >> pIntencity))
    return $ intencities ++ intensities_1

pAtmosphere :: Parser WeatherCondition
pAtmosphere = Atmosphere <$> (optional space *> pAtmosphereType)

pClear :: Parser WeatherCondition
pClear = Clear <$> (optional space *> pClearType)

pCondition :: Parser WeatherCondition
pCondition =
  do
    choice
      [ string "drizzle" >> return (Drizzle [] Nothing),
        string "sleet" >> return (Snow [] Nothing),
        string "snow" >> return (Snow [] Nothing),
        string "rain" >> return (Rain []),
        string "clouds" >> return (Clouds []),
        string "thunderstorm" >> return (Thunderstorm [] Nothing)
      ]

pWeatherCondition :: Parser WeatherCondition
pWeatherCondition = do
  intencities <- pIntencities
  optional space
  optional $ string "and"
  optional space
  condition <- pCondition
  case condition of
    (Drizzle _ _) -> return (Drizzle intencities Nothing)
    (Snow _ _) -> return (Snow intencities Nothing)
    (Rain _) -> return (Rain intencities)
    (Clouds _) -> return (Clouds intencities)
    (Thunderstorm _ _) -> return (Thunderstorm intencities Nothing)
    _ -> fail $ "Not a weather condition"

pPrimaryCondition :: Parser WeatherCondition
pPrimaryCondition =
  do
    choice [pAtmosphere, pWeatherCondition, pClear]
    <?> "Unknown primary weather condition"

pWeatherConditions :: Parser WeatherCondition
pWeatherConditions = do
  primary <- pPrimaryCondition
  optional space
  optional $ string "with"
  optional space
  secondary <- optional pWeatherCondition
  case (primary, secondary) of
    (Rain intencities, Just (Drizzle intencities2 _)) -> return $ Drizzle intencities2 (Just (Rain intencities))
    (Drizzle intencities _, Just (Rain intencities2)) -> return $ Drizzle intencities (Just (Rain intencities2))
    (Rain intencities, Just (Rain intencities2)) -> return $ Rain (intencities ++ intencities2)
    (Drizzle intencities _, Just (Drizzle intencities2 _)) -> return $ Drizzle (intencities ++ intencities2) Nothing
    (Thunderstorm intencities _, Just (Thunderstorm intencities2 _)) -> return $ Thunderstorm (intencities ++ intencities2) Nothing
    (Thunderstorm intencities _, Just (Rain intencities2)) -> return $ Thunderstorm intencities (Just (Rain intencities2))
    (Thunderstorm intencities _, Just (Drizzle intencities2 _)) -> return $ Thunderstorm intencities (Just (Drizzle intencities2 Nothing))
    (Rain intencities, Just (Thunderstorm intencities2 _)) -> return $ Thunderstorm (intencities ++ intencities2) (Just (Rain intencities))
    (Snow intencities _, Just (Snow intencities2 _)) -> return $ Snow (intencities ++ intencities2) Nothing
    (Snow intencities _, Just (Rain intencities2)) -> return $ Snow intencities (Just (Rain intencities2))
    (Rain intencities, Just (Snow intencities2 _)) -> return $ Snow (intencities ++ intencities2) (Just (Rain intencities))
    (condition, Nothing) -> return condition
    (x, y) -> fail $ "Unknown weather conditions: " ++ show x ++ ", " ++ show y

parseWeatherCondition :: String -> WeatherCondition
parseWeatherCondition input = case parse pWeatherConditions "" input of
  Left err -> error $ errorBundlePretty err
  Right result -> result

parseWeatherConditions :: [String] -> [WeatherCondition]
parseWeatherConditions = map parseWeatherCondition
