# global for the worldclock app

library(magrittr)
library(anytime)
library(stringr)
library(lubridate)
library(sparkline)
library(htmltools)


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
  # selection is "Berlin, DE", from there we get the lat lon and make one API call

  pattern <- paste0("^", selection, "$")
  hit <- data[str_detect(data$value, pattern), ][1, ] # in case more than one
  lat <- hit$lat
  lon <- hit$lon
  cityid <- hit$id
  
  weather <- get_forecast_onecall(lat, lon, apikey = api_key) # make reactive to invalidate?
  iconurl <- get_weather_icon(weather$icon)
  
                 
  #forecast <- get_forecast(cityid, timestamps = 18)
  if(degrees == "°C") { 
      temperature <- sprintf("%+3.0f", weather$temp - 273.15)
      weather$daily_tempday <- formatC(weather$daily_tempday - 273.15, format = "f", digits = 1) 
      weather$daily_tempmin <- sprintf("%+03.0f", weather$daily_tempmin - 273.15) 
      weather$daily_tempmax <- sprintf("%+03.0f", weather$daily_tempmax - 273.15) 
      weather$forecast_tempmin <- sprintf("%+3.0f", weather$forecast_tempmin - 273.15)
      weather$forecast_tempmax <- sprintf("%+3.0f", weather$forecast_tempmax - 273.15)
    } else { 
      temperature <- round((weather$temp * 9/5) - 459.67, 0)
      weather$daily_tempday <- round((weather$daily_tempday * 9/5) - 459.67, 0)
      weather$daily_tempmin <- round((weather$daily_tempmin * 9/5) - 459.67, 0)
      weather$daily_tempmax <- round((weather$daily_tempmax * 9/5) - 459.67, 0)
      weather$forecast_tempmin <- round((weather$forecast_tempmin * 9/5) - 459.67, 0)
      weather$forecast_tempmax <- round((weather$forecast_tempmax * 9/5) - 459.67, 0)
    }
  
  # this is cheap call to count seconds
  mytime <- renderText({
    invalidateLater(2000)
    
    #time to show is:
    mytime <- lubridate::now(tzone = "UTC") + weather$tz_offset
    #zoned_time <- clock::zoned_time_now(tz)
    format.POSIXct(lubridate::as_datetime(mytime), format = "%H:%M")
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
                  f7ListItem(paste0(weather$main, " "), tags$b( paste0(temperature,"°") ), 
                             href = "#", # this is used here just to add the class needed to make it look like a clickable link
                             #paste0(weather$temp, " ",weather$weather, " ↑", format.POSIXct(weather$sunrise, format = "%H:%M"), " ↓", format.POSIXct(weather$sunset, format = "%H:%M")) , 
                             #right = selection,  
                             media = apputils::icon(list(src = iconurl, width = "40px"), lib = "local"), 
                             title = tags$b( style = "font-family: Arial;", mytime), 
                             header = selection, 
                             footer = paste0("↑", format.POSIXct(anytime(weather$sunrise + weather$tz_offset, asUTC = T), format = "%H:%M"), 
                                             " ↓", format.POSIXct(anytime(weather$sunset + weather$tz_offset, asUTC = T), format = "%H:%M")) #weather$main
                             )
                  #f7SwipeoutItem(id = paste0("swipe_", cityid), color = "pink", "Alert")
              ),
               f7Popup(id = paste0("popup_", cityid),
                       title = paste0(selection, " 7 days forecast (", weather$forecast_tempmin, "°/",weather$forecast_tempmax,"°)"), 
                       swipeToClose = T, fullsize = T,
                       f7List(
                         lapply(seq(weather$daily_main), function(j){ # these are the forecast points
                           iconpath <- get_weather_icon( weather$daily_icon[j] )
                           
                           f7ListItem(
                             tags$b(style = "font-family: monospace;", paste0( weather$daily_tempmin[j], "°") ), # monospace for temp to avoid shifting boxplot
                             #htmltools::as.tags(sp),
                             # try boxplots -> low_whisker, q1, median, q3, high_whisker, ..showOutliers = FALSE
                             sparkline(c(weather$forecast_tempmin, 
                                              weather$daily_tempmin[j], 
                                              weather$daily_tempday[j], 
                                              weather$daily_tempmax[j], 
                                              weather$forecast_tempmax), 
                                       type = "box", raw = TRUE, showOutliers = FALSE,
                                       lineColor = "Grey", 
                                       lineWidth = 6,
                                       medianColor = "LightGrey",
                                       boxLineColor = "Grey", 
                                       boxFillColor = "LightGrey", 
                                       whiskerColor = "Grey"),
                              tags$b( style = "font-family: monospace;", paste0( weather$daily_tempmax[j], "°" ) ),
                        
                             title = tags$b(style = "font-family: Arial;",
                                            format.POSIXct(anytime(weather$daily_time[j] + weather$tz_offset, asUTC = T), 
                                                           format = "%a")
                                            ),
                             header = format.POSIXct(anytime(weather$daily_time[j] + weather$tz_offset, asUTC = T), 
                                                     format = "%e %b"),
                             footer = weather$daily_main[j],
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


