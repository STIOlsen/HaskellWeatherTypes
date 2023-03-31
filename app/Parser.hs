module Parser where

import Control.Monad (void)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import WeatherConditions (AtmosphereType (..), ClearType (..), CloudsType (..), Intencity (..), WeatherCondition (..))

type Parser a = Parsec Void String a

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
        string "freezing" >> return Freezing
      ]

pRain :: Parser WeatherCondition
pRain =
  do
    optional space
    intencities <- many (try (space >> pIntencity))
    _ <- space >> string "rain" >> optional space
    optional space
    return (Rain intencities)

pDrizzle :: Parser WeatherCondition
pDrizzle =
  do
    optional space
    intencities <- many pIntencity
    optional space
    optional $ string "intensity"
    optional space
    _ <- string "drizzle"
    optional space
    isRain <- optional $ string "rain"
    case isRain of
      Just _ -> return (Drizzle intencities (Just (Rain [])))
      Nothing -> return (Drizzle intencities Nothing)

pRainAndDrizzle :: Parser WeatherCondition
pRainAndDrizzle =
  do
    rain <- try pRain
    optional space
    _ <- try $ string "and"
    optional space
    return $ Drizzle [] (Just $ rain)

pWeatherCondition :: Parser WeatherCondition
pWeatherCondition = choice [try pRainAndDrizzle, try pDrizzle, try pRain]

parseWeatherConditions :: String -> IO ()
parseWeatherConditions input = case parse pWeatherCondition "" input of
  Left err -> putStrLn $ errorBundlePretty err
  Right result -> print result
