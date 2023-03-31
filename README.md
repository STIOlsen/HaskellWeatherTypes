# HaskellWeatherTypes (In Progress)

This program is a command-line application that allows users to fetch current weather and forecast data based on their location. Users can search for weather data by city name or geographical coordinates (latitude and longitude).This project emphasizes the creation of data types for weather and the use of functional programming concepts to accomplish this task.

## Features

- Retrieve current weather data for a specific location
- Fetch forecast data for a specific location
- Display weather data in a user-friendly format
- Support for multiple temperature units (Celsius, Kelvin, and Fahrenheit)

## Prerequisites

To build and run this project, you will need:

- [Haskell Platform](https://www.haskell.org/platform/) installed on your machine
- A valid API key for the [OpenWeatherMap API](https://openweathermap.org/api)

## Installation

1. Clone the repository:

   `https://github.com/STIOlsen/HaskellWeatherTypes `

2. Navigate to the project folder:

   `cd HaskellWeatherTypes-app`

3. Open the `ApiKey.hs` file located in the `app` folder and add your own OpenWeatherMap API key :

   ```haskell
   module ApiKey where

   apiKey :: String
   apiKey = "" -- Your API key goes here
   ```

4. Build and run the project
   `cabal build`
   `cabal run`

## Usage

Upon starting the application, you will be presented with a menu to choose your desired method for looking up the weather:

```
How would you like to look up the weather?
1. By city name
2. By coordinates
Enter the number corresponding to your choice:
```

Enter the number corresponding to your preferred method (1 for city name or 2 for coordinates) and follow the prompts to enter the required information.

The application will then display the current weather

##### Example

```haksell
How would you like to look up the weather?
1. By city name
3. By coordinates
Enter the number corresponding to your choice: 1
Enter the city name:
bergen
"--------- Current weather -----------"
Date: 2023-03-31 15:57:57 UTC
Forcast Time: N/A
Location: (Just 5.328,Just 60.392)
Country: NO
Temperature: 283.08 K
Humidity: 50.0%
Pressure: 1002.0hPa
Wind Speed: 4.63m/s
Wind Direction: 310.0°
Weather Descriptions: ["few clouds"]
Weather Icons: ["02d"]

"---------- Forcast (only the first one ) ----------"
Date: 2023-03-31 18:00:00 UTC
Forcast Time: 2023-03-31 18:00:00 UTC
Location: (Just 5.328,Just 60.392)
Country: N/A
Temperature: 281.62 K
Humidity: 61.0%
Pressure: 1002.0hPa
Wind Speed: 1.43m/s
Wind Direction: 15.0°
Weather Descriptions: ["few clouds"]
Weather Icons: ["02d"]

"--------------------"
"Average Temperature of Forcasts:"
4.990007 °C
(Rain [])
(Rain [])
(Rain [light])
(Drizzle [heavy] (Just (Rain [])))
(Drizzle [] (Just (Rain [shower])))
(Drizzle [] (Just (Rain [heavy,shower])))
(Drizzle [] (Just (Rain [])))
```

## Progress

### Completed Tasks:

1. Created custom data type for weather data: `WeatherData`.
   Retrieved data from OpenWeather Api using the Haskell OpenWeather library.

2. Converted OpenWeather data,both currentweather and forcast, to the `WeatherData` data type.
3. Implemented a `Temperature` representation for Celsius, Kelvin, and Fahrenheit (subject to improvements).
4. Created a method to calculate the average temperature from a list of WeatherData.
5. Developed a custom data type to represent weather conditions, e.g., "light rain" is represented as Rain [Light] (based on OpenWeather conditions).
   Started working on a parser for weatherDescription, currently parsing "rain" and "drizzle."

### Upcoming Tasks:

##### (Tentative)

1. Parse all types of weather descriptions.
2. Update `weatherDescriptions` (a field of `WeatherData`) to use the `WeatherCondition` data type using the parser.
3. Develop more functions for weather calculations.
4. Improve how `WeatherData` and future forecasts are displayed by possibly using the functions mentioned in the previous point.
5. Explore the possibility of using a state or TVar to store and update current weather and forecasts, and update those at time intervals or when necessary.
6. Implement methods for choosing the desired temperature representation, with Kelvin as the default.
