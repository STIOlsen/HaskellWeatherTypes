module Temperature where

type Celsius = Float

type Kelvin = Float

type Fahrenheit = Float

data Temperature
  = Celsius Celsius
  | Kelvin Kelvin
  | Fahrenheit Fahrenheit
  deriving (Eq, Ord)

instance Show Temperature where
  show (Celsius t) = show t ++ " °C"
  show (Kelvin t) = show t ++ " K"
  show (Fahrenheit t) = show t ++ " °F"

fromCelsius :: Celsius -> Temperature
fromCelsius = Celsius

fromKelvin :: Kelvin -> Temperature
fromKelvin = Kelvin

fromFahrenheit :: Fahrenheit -> Temperature
fromFahrenheit = Fahrenheit

getCelsius :: Temperature -> Celsius
getCelsius (Celsius t) = t
getCelsius (Kelvin t) = t - 273.15
getCelsius (Fahrenheit t) = (t - 32) * 5 / 9

getKelvin :: Temperature -> Kelvin
getKelvin (Celsius t) = t + 273.15
getKelvin (Kelvin t) = t
getKelvin (Fahrenheit t) = (t + 459.67) * 5 / 9

getFahrenheit :: Temperature -> Fahrenheit
getFahrenheit (Celsius t) = t * 9 / 5 + 32
getFahrenheit (Kelvin t) = t * 9 / 5 - 459.67
getFahrenheit (Fahrenheit t) = t

toCelsius :: Temperature -> Temperature
toCelsius (Celsius t) = Celsius t
toCelsius (Kelvin t) = Celsius (t - 273.15)
toCelsius (Fahrenheit t) = Celsius ((t - 32) * 5 / 9)

toKelvin :: Temperature -> Temperature
toKelvin (Celsius t) = Kelvin (t + 273.15)
toKelvin (Kelvin t) = Kelvin t
toKelvin (Fahrenheit t) = Kelvin ((t + 459.67) * 5 / 9)

toFahrenheit :: Temperature -> Temperature
toFahrenheit (Celsius t) = Fahrenheit (t * 9 / 5 + 32)
toFahrenheit (Kelvin t) = Fahrenheit (t * 9 / 5 - 459.67)
toFahrenheit (Fahrenheit t) = Fahrenheit t

convertTemperature :: Temperature -> Temperature -> Temperature
convertTemperature (Celsius _) t = t
convertTemperature (Kelvin _) t = toKelvin t
convertTemperature (Fahrenheit _) t = toFahrenheit t

subtractTemperature :: Temperature -> Temperature -> Temperature
subtractTemperature t1 t2 = convertTemperature t1 (Celsius (getCelsius t1 - getCelsius t2))

addTemperature :: Temperature -> Temperature -> Temperature
addTemperature t1 t2 = convertTemperature t1 (Celsius (getCelsius t1 + getCelsius t2))