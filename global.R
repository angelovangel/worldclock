# global for the worldclock app

require(magrittr)
require(anytime)
require(ROpenWeatherMap)
source("config")

# input is time zone as in tzdb
# output is a list with data
get_weather <- function(timeZone) {
  #get city name
  mycity <- strsplit(timeZone, "/") %>% unlist() %>% tail(1)
  myweather <- try(ROpenWeatherMap::get_current_weather(api_key, city = mycity)
                                      )
  # return only if data fetched OK
  if(myweather$cod == 200) {
  return(
    list(
      sunrise = anytime(myweather$sys$sunrise, tz = timeZone) %>% format.POSIXct(format = "%H:%M"),
      sunset = anytime(myweather$sys$sunset, tz = timeZone) %>% format.POSIXct(format = "%H:%M"),
      temp = paste0( round(myweather$main$temp - 273.15, 1), "˚C"),
      weather = myweather$weather$main,
      iconcode = myweather$weather$icon
      
    )
  )
  } else {
      
    }
  
}

get_weather_icon <- function(iconcode) {
  baseurl <- "http://openweathermap.org/img/wn/"
  paste0(baseurl, iconcode, "@2x.png")
}



