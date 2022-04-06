# global for the worldclock app

library(magrittr)
library(anytime)
library(stringr)
library(lubridate)
library(sparkline)
library(htmltools)
library(scales)


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

# define style for fonts
mystyle <- function(fontsize, align = "left", color = "LightGrey") {
  paste0("font-family: 'Helvetica Neue Ultra Thin', 'Helvetica Neue Light', 'Helvetica Neue', Helvetica, Arial; font-size:", 
         fontsize, "px; text-align: ", align, "; color: ", color, ";" 
  )
}

# pass temp in K as argument, return hex
my_temp_color <- function(temp) {
  # set scale 
  # color ramp used for temp gradient
  myramp <- scales::colour_ramp(c("blue", "cyan", "green", "yellow", "red"), na.color = "#D5D8DC")
  scaled_temp <- scales::rescale( temp, from = c(223.15, 323.15), to = c(0,1) ) # -50 to +50 C
  myramp(scaled_temp)
}

  
insertListItem <- function(selection, data, degrees = c("°C", "°F"), clientoffset) {
  
  # call once
  # selection is "Berlin, DE", from there we get the lat lon and make one API call

  pattern <- paste0("^", selection, "$")
  hit <- data[str_detect(data$value, pattern), ][1, ] # in case more than one
  lat <- hit$lat
  lon <- hit$lon
  cityid <- hit$id
  
  weather <- get_forecast_onecall(lat, lon, apikey = api_key) # make reactive to invalidate?
  iconurl <- get_weather_icon(weather$icon)
  
                 
  if(degrees == "°C") { 
      temperature <- sprintf("%+3.0f", weather$temp - 273.15)
      daily_tempday <- formatC(weather$daily_tempday - 273.15, format = "f", digits = 1) 
      daily_tempmin <- sprintf("%+03.0f", weather$daily_tempmin - 273.15) 
      daily_tempmax <- sprintf("%+03.0f", weather$daily_tempmax - 273.15) 
      forecast_tempmin <- sprintf("%+3.0f", weather$forecast_tempmin - 273.15)
      forecast_tempmax <- sprintf("%+3.0f", weather$forecast_tempmax - 273.15)
    } else { 
      temperature <- round((weather$temp * 9/5) - 459.67, 0)
      daily_tempday <- round((weather$daily_tempday * 9/5) - 459.67, 0)
      daily_tempmin <- round((weather$daily_tempmin * 9/5) - 459.67, 0)
      daily_tempmax <- round((weather$daily_tempmax * 9/5) - 459.67, 0)
      forecast_tempmin <- round((weather$forecast_tempmin * 9/5) - 459.67, 0)
      forecast_tempmax <- round((weather$forecast_tempmax * 9/5) - 459.67, 0)
    }
  
  mysunrise <- format.POSIXct(anytime(weather$sunrise + weather$tz_offset, asUTC = T), format = "%H:%M")
  mysunset <- format.POSIXct(anytime(weather$sunset + weather$tz_offset, asUTC = T), format = "%H:%M")
  
  # calculate location offset in hours relative to client (client offset to UTC in minutes given as param)
  listItemOffset <- renderText({
    invalidateLater(10000)
    # generate diff
    myoffset <- difftime(time1 = lubridate::now(tzone = "UTC") + weather$tz_offset, 
                         time2 = lubridate::now(tzone = "UTC") + (-clientoffset*60), units = "hours")
    # some calculations to see whether it is today, tomorrow or yesterday 
    day_there <- day( lubridate::now(tzone = "UTC") + weather$tz_offset )
    day_here <- day( lubridate::now(tzone = "UTC") + (-clientoffset*60) )
    myday <- case_when(
      day_here - day_there == 0 ~ "Today",
      day_here - day_there > 0 ~ "Yesterday",
      day_here - day_there < 0 ~ "Tomorrow"
    )
    paste0(myday, ", ", sprintf("%+.0f", myoffset), " hours" ) 
  })
  
  
  # this is cheap call to count seconds
  mytime <- renderText({
    invalidateLater(2000)
    
    #time to show is:
    mytime <- lubridate::now(tzone = "UTC") + weather$tz_offset # in seconds
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
                  f7ListItem(tags$div(style = mystyle(fontsize = 16, align = "right", color = my_temp_color(weather$temp) ), 
                                      paste0(temperature, "°"), 
                                      tags$div(style = mystyle(fontsize = 16), weather$description)
                                      ), 
                             href = "#", # this is used here just to add the class needed to make it look like a clickable link
                             #paste0(weather$temp, " ",weather$weather, " ↑", format.POSIXct(weather$sunrise, format = "%H:%M"), " ↓", format.POSIXct(weather$sunset, format = "%H:%M")) , 
                             #right = selection,  
                             media = apputils::icon(list(src = iconurl, width = "40px"), lib = "local"), 
                             title = tags$div( style = mystyle(fontsize = 30), mytime), 
                             header = tags$div(style = mystyle(fontsize = 16), selection), 
                             footer = tags$div(style = mystyle(fontsize = 13), listItemOffset)
                             )
                  #f7SwipeoutItem(id = paste0("swipe_", cityid), color = "pink", "Alert")
              ),
               f7Popup(id = paste0("popup_", cityid),
                       title = tags$div(style = mystyle(fontsize = 15),
                         paste0(selection, " 7 days forecast (", forecast_tempmin, "°/", forecast_tempmax,"°)")
                         ), 
                       swipeToClose = T, fullsize = T,
                       f7List(
                         lapply(seq(weather$daily_main), function(j){ # these are the forecast points
                           iconpath <- get_weather_icon( weather$daily_icon[j] )
                           
                           f7ListItem(
                             tags$b(style = "font-family: 'Roboto Mono', monospace;", paste0( daily_tempmin[j], "°") ), # monospace for temp to avoid shifting boxplot
                             #htmltools::as.tags(sp),
                             # try boxplots -> low_whisker, q1, median, q3, high_whisker, ..showOutliers = FALSE
                             sparkline(c(forecast_tempmin, 
                                         daily_tempmin[j], 
                                         daily_tempday[j], 
                                         daily_tempmax[j], 
                                         forecast_tempmax), 
                                       type = "box", raw = TRUE, showOutliers = FALSE,
                                       lineColor = "Grey", 
                                       lineWidth = 3,
                                       medianColor = "LightGrey",
                                       boxLineColor = "LightGrey", 
                                       boxFillColor = my_temp_color( weather$daily_tempmax[j] ), 
                                       whiskerColor = "LightGrey"),
                             tags$b( style = "font-family: monospace;", paste0( daily_tempmax[j], "°" ) ),
                        
                             title = tags$div(style = mystyle(fontsize = 22),
                                            format.POSIXct(anytime(weather$daily_time[j] + weather$tz_offset, asUTC = T), 
                                                           format = "%a")
                                            ),
                             header = format.POSIXct(anytime(weather$daily_time[j] + weather$tz_offset, asUTC = T), 
                                                     format = "%e %b"),
                             footer = tags$div(style = mystyle(fontsize = 13), weather$daily_main[j]),
                             media = apputils::icon(list(src = iconpath, width = "40px"), lib = "local"),
                             )
                         }) 
                       )
               )
             
    )
  )
  #open popup #=============================================
  
  onclick(paste0("item_", cityid), 
          shinyMobile::updateF7Popup(id = paste0("popup_", cityid))
  )
  #============================================================================
  
}


