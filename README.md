# HaskellWeatherTypes (In Progress)

This program is a command-line application that allows users to fetch current weather and forecast data based on their location. Users can search for weather data by city name or geographical coordinates (latitude and longitude).This project emphasizes the creation of data types for weather,parsing weather conditions, and the use of functional programming concepts to accomplish this task.

## Features

- Retrieve current weather data for a specific location
- Fetch forecast data for a specific location
- Umbrella recommendation for the current weather condition.
- Average temperature of the forecasted weather.
- Wind speed range of the forecasted weather.
- Umbrella recommendation for each day in the forecast.

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

```haksellHow would you like to look up the weather?
1. By city name
2. By coordinates
Enter the number corresponding to your choice: 1
Enter the city name:
bergen

Umberella recomendation for today:
No umbrella needed.

Average Temperature of forcasts:
9.48 °C

Wind Speed Range of forcast:
(0.5,7.15)

Umbrella recomendation forward :
("2023-05-05",[])
("2023-05-06",[])
("2023-05-07",[])
("2023-05-08",[])
("2023-05-09",[((8,8),Bring an umbrella.)])
("2023-05-10",[((2,2),Bring an umbrella.)])
```

## Progress

### Completed Tasks:

1. Created custom data type for weather data: `WeatherData`.
   Retrieved data from OpenWeather Api using the Haskell OpenWeather library.

2. Converted OpenWeather data,both currentweather and forcast, to the `WeatherData` data type.
3. Implemented a `Temperature` representation for Celsius, Kelvin, and Fahrenheit (subject to improvements).
4. Created a method to calculate the average temperature from a list of `WeatherData`.
5. Developed a custom data type to represent weather conditions, e.g., "light rain" is represented as `Rain [Light]` (based on OpenWeather conditions).
6. Made a parser that parser the strings,describing the weather from OpenWeather conditions, into the `weatherData`structure.
7. Made a `UmbrellaCondition` module to determine if a umbrella is needed based on the weatherConditions.
