# global for the worldclock app

require(magrittr)
require(anytime)
require(ROpenWeatherMap)
require(stringr)
require(lubridate)


api_key <- Sys.getenv("api_key")

# get city id from smartselect input
# input is smartselect value (Sofia, BG) and data is the cities dataframe
# match has to be more strict, so modify input

get_cityid <- function(input, data) {
  pattern <- paste0("^", input, "$")
  data$id[str_detect(data$value, paste0("^", input, "$"))][1] # take first in case duplicated entries
}

# get city from time zone
get_city <- function(timeZone) {
  strsplit(timeZone, "/") %>% unlist() %>% tail(1) %>% gsub("_", "+", .)
}

# input is time zone as in tzdb or cityID (openweathermap id)
# output is a list with data
get_weather <- function(input) {
  
  # if input is id
  if(is.numeric(input)) {
    myweather <- ROpenWeatherMap::get_current_weather(api_key, cityID = input)
  } else {
    #get city name
    mycity <- strsplit(input, "/") %>% unlist() %>% tail(1) %>% gsub("_", "+", .) # get_current_weather uses + instead of _
    myweather <- ROpenWeatherMap::get_current_weather(api_key, city = mycity)
  }
    
  # return only if data fetched OK
  if(myweather$cod == 200) {
    
    return(
      list(
        utctime = myweather$dt,
        tz_offset = myweather$timezone,
        #sunrise = anytime(myweather$sys$sunrise + myweather$tz_offset),
        #sunset = anytime(myweather$sys$sunset, tz = timeZone),
        # temp = ifelse(degrees=="°C", 
        #               round(myweather$main$temp - 273.15, 1), 
        #               round((myweather$main$temp * 9/5) - 459.67, 0) 
        #               ),
        temp = myweather$main$temp, # K here
        weather = myweather$weather$main[1], #sometimes more than 1 are returned
        iconcode = myweather$weather$icon[1],
        city = myweather$name,
        city_id = myweather$id
      )
    )
  } else {
    return(
      list(
        utctime = "",
        tz_offset = "",
        #sunrise = anytime(00:00:00),
        #sunset = anytime(00:00:00),
        temp = "",
        weather = "",
        iconcode = "", #fallback icon
        city = "",
        cityid = ""
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

insertListItem <- function(selection, data, degrees = c("°C", "°F") ) {
  
  # call once
  cityid <- get_cityid(selection, data) # 
  weather <- get_weather(cityid) # make reactive to invalidate?
  iconurl <- get_weather_icon(weather$iconcode)
  temperature <- ifelse(degrees=="°C", 
                 round(weather$temp - 273.15, 1), 
                 round((weather$temp * 9/5) - 459.67, 0) 
                                )
                 
  forecast <- get_forecast(cityid, api_key)
  
  # this is cheap call to count seconds
  mytime <- renderText({
    invalidateLater(2000)
    
    #time to show is:
    mytime <- lubridate::now(tzone = "UTC") + weather$tz_offset
    #zoned_time <- clock::zoned_time_now(tz)
    format.POSIXct(as_date_time(mytime), format = "%H:%M")
  })
  
  # for reactive cases (to use input$degrees) remove first then insert
  removeUI(
    selector = paste0("#item_", cityid), # careful here id is the city only 
    multiple = FALSE
  )
  
  # and return the UI
  insertUI(
    selector = "#mylist", where = "beforeEnd",
    ui = tags$div( id = paste0("item_", cityid), # use cityid as tag.. should be ok
              f7Swipeout(
                  f7ListItem(paste0(temperature,"°" ," ", weather$weather), 
                             href = "#", # this is used here just to add the class needed to make it look like a clickable link
                             #paste0(weather$temp, " ",weather$weather, " ↑", format.POSIXct(weather$sunrise, format = "%H:%M"), " ↓", format.POSIXct(weather$sunset, format = "%H:%M")) , 
                             right = selection,  
                             media = apputils::icon(list(src = iconurl, width = "40px"), lib = "local"), 
                             title = tags$h3(
                               style = "font-family: Arial;", mytime)
                             )
                  #f7SwipeoutItem(id = paste0("swipe_", cityid), color = "pink", "Alert")
              ),
               f7Popup(id = paste0("popup_", cityid),title = selection, swipeToClose = T, fullsize = T,
                       f7List(
                         lapply(1:8, function(j){ # these are the foreast points
                           iconpath <- get_weather_icon( forecast$icon[j] )
                           
                           f7ListItem(
                             paste0(forecast$temp[j], "° ", forecast$main[j]),
                             title = tags$h4( forecast$time[j] ),
                             media = apputils::icon(list(src = iconpath, width = "40px"), lib = "local"),
                             )
                         }) 
                       )
               )
             
    )
  )
  
  #open popup
  onclick(paste0("item_", cityid), shinyMobile::updateF7Popup(id = paste0("popup_", cityid)))
}


