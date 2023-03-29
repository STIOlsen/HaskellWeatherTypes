module Temperature
  ( Temperature (..),
    toCelsius,
    toKelvin,
    toFahrenheit,
  )
where

data Temperature
  = Celsius Float
  | Kelvin Float
  | Fahrenheit Float
  deriving (Eq)

instance Show Temperature where
  show (Celsius t) = show t ++ " °C"
  show (Kelvin t) = show t ++ " K"
  show (Fahrenheit t) = show t ++ " °F"

instance Num Temperature where
  (Celsius t1) + (Celsius t2) = Celsius (t1 + t2)
  (Kelvin t1) + (Kelvin t2) = Kelvin (t1 + t2)
  (Fahrenheit t1) + (Fahrenheit t2) = Fahrenheit (t1 + t2)
  (Celsius t1) - (Celsius t2) = Celsius (t1 - t2)
  (Kelvin t1) - (Kelvin t2) = Kelvin (t1 - t2)
  (Fahrenheit t1) - (Fahrenheit t2) = Fahrenheit (t1 - t2)
  (Celsius t1) * (Celsius t2) = Celsius (t1 * t2)
  (Kelvin t1) * (Kelvin t2) = Kelvin (t1 * t2)
  (Fahrenheit t1) * (Fahrenheit t2) = Fahrenheit (t1 * t2)

  fromInteger t = Celsius (fromInteger t)

instance Fractional Temperature where
  (Celsius t1) / (Celsius t2) = Celsius (t1 / t2)
  (Kelvin t1) / (Kelvin t2) = Kelvin (t1 / t2)
  (Fahrenheit t1) / (Fahrenheit t2) = Fahrenheit (t1 / t2)

toCelsius :: Temperature -> Temperature
toCelsius (Celsius t) = Celsius t
toCelsius (Kelvin t) = Celsius (t - 273.15)
toCelsius (Fahrenheit t) = Celsius ((t - 32) * 5 / 9)

toKelvin :: Temperature -> Temperature
toKelvin (Celsius t) = Kelvin (t + 273.15)
toKelvin (Kelvin t) = Kelvin t
toKelvin (Fahrenheit t) = Kelvin ((t + 459.67) * 5 / 9)

toFahrenheit :: Temperature -> Temperature
toFahrenheit (Celsius t) = Fahrenheit ((t * 9 / 5) + 32)
toFahrenheit (Kelvin t) = Fahrenheit ((t * 9 / 5) - 459.67)
toFahrenheit (Fahrenheit t) = Fahrenheit t
