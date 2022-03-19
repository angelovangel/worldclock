# global for the worldclock app

require(magrittr)
require(anytime)
require(ROpenWeatherMap)

api_key <- Sys.getenv("api_key")

# get city from time zone
get_city <- function(timeZone) {
  strsplit(timeZone, "/") %>% unlist() %>% tail(1) %>% gsub("_", "+", .)
}

# input is time zone as in tzdb
# output is a list with data
get_weather <- function(timeZone) {
  
  #get city name
  mycity <- strsplit(timeZone, "/") %>% unlist() %>% tail(1) %>% gsub("_", "+", .) # get_current_weather uses + instead of _
  myweather <- ROpenWeatherMap::get_current_weather(api_key, city = mycity)
                                      
  # return only if data fetched OK
  if(myweather$cod == 200) {
    
  return(
    list(
      sunrise = anytime(myweather$sys$sunrise, tz = timeZone),
      sunset = anytime(myweather$sys$sunset, tz = timeZone),
      temp = paste0( round(myweather$main$temp - 273.15, 1), "˚"),
      weather = myweather$weather$main,
      iconcode = myweather$weather$icon,
      city = myweather$name
      
    )
  )
  } else {
  return(
    list(
      sunrise = anytime(00:00:00),
      sunset = anytime(00:00:00),
      temp = "",
      weather = "",
      iconcode = "",
      city = ""
      
    )
  )
    }
  
}

get_weather_icon <- function(iconcode) {
  baseurl <- "http://openweathermap.org/img/wn/"
  paste0(baseurl, iconcode, "@2x.png")
}

insertListItem <- function(tz) {
  
  mytime <- renderText({
    invalidateLater(1000)
    zoned_time <- clock::zoned_time_now(tz)
    format.POSIXct(as_date_time(zoned_time), format = "%H:%M:%S")
  })
  
  city <- get_city(tz)
  weather <- get_weather(tz) # make reactive to invalidate?
  iconurl <- get_weather_icon(weather$iconcode)
  
  insertUI(
    selector = "#mylist",
    ui = tags$div( id = paste0("item_", city ),
                  f7ListItem(paste0(city, " | ", weather$temp), right = "bla", 
                             media = apputils::icon(list(src = iconurl, width = "50px"), lib = "local"), 
                             title = tags$p(style = "font-family: Arial", mytime) )
      
    )
  )
}
