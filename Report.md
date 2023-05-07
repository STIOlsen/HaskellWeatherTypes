# Report on Project HaskellWeatherTypes

This project uses various abstractions to model and manipulate weather data. This project fetch weather data from the [OpenWeatherMap API](https://openweathermap.org/api) through the haskell library [openweathermap](https://hackage.haskell.org/package/openweathermap-0.3.0) and transform that data into a variety of useful formats.

#### Table of Content

**1.** [Description of the program](#description-of-the-program)

- [Program overview](#program-overview)

**2.** [Description of the Functional Techniques](#description-of-the-functional-techniques)

- [Abstraction and Algebraic Data types](#abstraction-and-algebraic-data-types)
- [Monadic Parser](#monadic-parser)
- [General Functional Techniques](#general-functional-techniques)

**3.** [Evaluation of the Techniques](#evaluation-of-the-techniques)

**4.** [Comparing Finished Project to Original Project Proposal](#comparing-finished-project-to-original-project-proposal)

### <a id="description-of-the-program"></a> Description of the program

The program is organized into several modules, each containing data types and functions that perform specific tasks.

#### <a id="program-overview"></a> Program overview

- `Main`
- `OpenweatherMapClient` - _Functions for getting weather information from the OpenWeatherMap API._
- `WeatherData` - _Defines data structure for waether data_
- `WeatherDataUtils` - _Functions for processing and manipulating weather data._
- `WeatherDataConverter` - _Converting weather data from OpenWeatherMap API format to WeatherData format_
- `WeatherConditions` - _Defines data structure for weather conditions_
- `Parser` - _Parsers for weather conditions_
- `UmbrellaConditions` - _Defines datas trucure for umbrella conditions_
- `Temperature` - _Defines a Temperature data type and functions for converting and manipulating temperature values_

Since this project prioritizes functionality over aesthetics, testing is limited to displaying certain functions in the terminal based on user input. See the README for how to.

### <a id="description-of-the-functional-techniques"></a> Description of the Functional Techniques

#### <a id="abstraction-and-algebraic-data-types"></a> Abstraction and Algebraic Data types

Abstraction and algebraic data types are fundamental to functional programming. This project makes extensive use of both concepts to model and manipulate weather data. Notably in WeatherData, WeatherConditions,UmbrellaCondition. By using algebraic data types, we can express complex data structures and their relationships in a concise and intuitive manner. This allows us to reason about the program more easily and leverage Haskell's strict type system to ensure correctness in our implementation.

One of the main abstraction i have in this project is in the **WeatherData** moduel. It has encasulated weather contidition such as temprature, humidity, pressure and wind speed. It uses algebraic data types to define different types of weather conniditons

In the **WeatherConditions** module, algebraic data types are used to represent different weather conditions from Openweather API. The WeatherCondition data type is defined using the sum-of-products pattern. It has several constructors, each representing a different type of weather condition. For example, `Rain` represents rain and takes a list of Intencity values that describe the intensity of the rain. Similarly, `Snow` represents snow and takes a list of Intencity values and an optional WeatherCondition value that describes any additional weather condition associated with the snow. The WeatherConditions module uses algebraic data types with multiple constructors to represent various weather conditions, providing a flexible and modular foundation for building higher-level functionality in weather-related applications. This structure made it particularly simple to implement the function e.g. `isUmbrellaNecesarry`, which checks whether an umbrella is necessary based on the weather conditions.

**UmbrellaCondition** is a datatype that describes whether an umbrella is recommended based on the weather. It is abstracted into of three records fields: recommendation,wind, and weather. UmbrellaCondition is seperated from `wheaterData` so it becomes easier to modify the recommendation algorithm. Additionally, the UmbrellaRecommendation is used to represent the possible recommendations, further abstracting the details of the recommendation algorithm from the rest of the code.

#### <a id="monadic-parser"></a> Monadic Parser

The Parser module utilizes monadic parsing to parse complex data structures. By using various monadic combinators as bulinding blocks i have achieved concise and expressive syntax. This parser specifically extracts weather conditions from strings and converts them into the WeatherCondition data type.

The OpenWeather API provides weather data in the form of IDs and strings, can be seen [here](https://openweathermap.org/weather-conditions), the module parses the strings into the weatherData structure instead of simply mapping the ID to a constructor.

To accomplish this i have used the library [MegaParsec](https://hackage.haskell.org/package/megaparsec), which offers a set of primitives and combinators for creating parsers that can handle complex grammars. Making it easier to define efficient and modular parsers

The module defines several smaller parsers, such as pIntencities and pAtmosphere, and composes them using the <|> and try combinators allowing to create a more comprehensive parser for weather conditions and backtracing.

Parsing the weather conditions strings has been a challenging task, and different parser variastions have beeing tried. The complexity arises from the varying structures of the strings given by the OpenWeather API, as seen in the examples `"light intensity drizzle rain"` and `"shower rain and drizzle"`. In these cases, the primary constructor is "Drizzle", while "Rain" serves as the secondary constructor. This results in the weather conditions being represented as `Drizzle [light] (Just Rain [])` and `Drizzle [] (Just Rain [shower])`, respectively. To address this issue, the parser has been designed to handle parsing for two weather conditions and combining them appropriately.

#### <a id="general-functional-techniques"></a> General Functional Techniques

**Pattern matching**, to utilize Haskell's strong type system for woring with datastructures.
**Aliasing**, for readability
**Higher-order functions**, allow for abstractions and creation of more complex functions
**IO monad**, sequence and handels effects of input/output.
**Maybe**, way of handling errors and unexpected cases in a safe and composable manner.

### <a id="evaluation-of-the-techniques"></a> Evaluation of the Techniques

Using abstraction and algebraic data types proved to be a powerful techique for this project. By defining WeatherData and WeatherCondition, i was able to manipulate different weather parameters and conditions in a modular and flexible way. Making it easier to add new features or modify existing ones without having to change large sections of code.

The use of monadic parsing was also usefull for the task of parsing weather conditions. By using monads combinators to compose smaller parsers, i was able to build a modular complex parser, in a simple manner, for various weather conditions. This made it easy to add new parsers or modify existing ones as needed. One benefits of using monadic parsing was that it made it relatively simple to prototype and experiment with different approaches to parsing the weather condition strings.

Overall, the use of these functional techniques helped to make the program more modular, flexible, and easier to maintain. By using abstraction, algebraic data types, and monadic parsing, the program was able to model and manipulate weather data in a way that was both structured and functional.

### <a id="comparing-finished-project-to-original-project-proposal"></a> Comparing Finished Project to Original Project Proposal

The over arching goal from the proposal was **"I want to use OpenWeather's API to display weather data for a user-specified city in the terminal"**

with the broken down parts of what that means
wanted to display :

1. Temperature, wind, and rain.
2. Today's sunrise and sunset times.
3. Whether an umbrella or raincoat is needed, based on wind and rain.
4. Compare rain, snow, sunrise/sunset times for two cities (last month, last year, total)."

When I wrote the project proposal, I didn't think too much about advanced functional techniques, mostly because I didn't know about their usage yet. So, I focused mostly on what I personally wanted to achieve. However, after learning more about advanced functional programming concepts in lectures, and with the suggestion from Håkon, I became interested in parsing. As a result, most of my time with this project was spent abstracting weather data into data types, experimenting with parsing, and creating functions for working with weather data to demonstrate some possible applications

I was able to successfully achieve the overarching goal of using OpenWeather's API to display weather data for a user-specified city in the terminal. I also successfully implemented features to display temperature, wind, and rain information, as well as determining whether an umbrella is needed. However, due to time constraints and the limitations of my API subscription, I was not able to attempt the features for today's sunrise and sunset times or comparing weather data or history between two cities. Despite not completing those features, I am satisfied with what I was able to achieve and the knowledge I gained through this project.
