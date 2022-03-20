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
      weather = myweather$weather$main[1], #sometimes more than 1 are returned
      iconcode = myweather$weather$icon[1],
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
      iconcode = "", #fallback icon
      city = ""
      
    )
  )
    }
  
}

get_weather_icon <- function(iconcode) {
  # fallback in case nothing was fetched by get_weather
  if(iconcode == "") {
    "https://raw.githubusercontent.com/framework7io/framework7-icons/master/src/sf/clear_fill.svg"
  } else {
    baseurl <- "http://openweathermap.org/img/wn/"
    paste0(baseurl, iconcode, "@2x.png")
  }
}

insertListItem <- function(tz) {
  
  mytime <- renderText({
    invalidateLater(2000)
    zoned_time <- clock::zoned_time_now(tz)
    format.POSIXct(as_date_time(zoned_time), format = "%H:%M")
  })
  
  city <- get_city(tz) %>% str_replace("\\+", "_") # replace + again to _ to use in id
  weather <- get_weather(tz) # make reactive to invalidate?
  iconurl <- get_weather_icon(weather$iconcode)
  
  insertUI(
    selector = "#mylist",
    ui = tags$div( id = paste0("item_", city),
                  f7ListItem(paste0(weather$temp, " ",weather$weather),
                             #paste0(weather$temp, " ",weather$weather, " ↑", format.POSIXct(weather$sunrise, format = "%H:%M"), " ↓", format.POSIXct(weather$sunset, format = "%H:%M")) , 
                             right = city %>% str_replace("_", " "),  
                             media = apputils::icon(list(src = iconurl, width = "50px"), lib = "local"), 
                             title = tags$h3(
                               style = "font-family: Arial;", mytime)
                             )
      
    )
  )
}
