module Parser where

import Control.Monad (void)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import WeatherConditions (WeatherCondition (..), atmosphereTypes, conditionTable)

type Parser a = Parsec Void String a

pAtmosphere :: Parser WeatherCondition
pAtmosphere = do
  str <- try (manyTill anySingle (lookAhead (choice (string <$> atmosphereTypes))))
  rest <- manyTill anySingle eof
  return $ Atmosphere (str ++ rest)

pCondition :: String -> Parser WeatherCondition
pCondition condition = do
  str <- try (manyTill anySingle (lookAhead (string condition)))
  rest <- manyTill anySingle eof
  case lookup condition conditionTable of
    Just constructor -> return $ constructor (str ++ rest)
    Nothing -> fail $ "Unknown weather condition: " ++ condition

pWeatherCondition :: Parser WeatherCondition
pWeatherCondition =
  foldr (<|>) pAtmosphere $
    map
      pCondition
      ["rain", "drizzle", "snow", "sleet", "thunderstorm", "clear", "clouds"]

parseWeatherCondition :: String -> WeatherCondition
parseWeatherCondition input = case parse pWeatherCondition "" input of
  Left err -> error $ errorBundlePretty err
  Right result -> result

parseWeatherConditions :: [String] -> [WeatherCondition]
parseWeatherConditions conditions = map parseWeatherCondition conditions
