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
mystyle <- function(fontsize, align = "left", color = "LightGrey", fontweight = 300) {
  paste0("font-family: 'Helvetica Neue Ultra Thin', 'Helvetica Neue Light', 'Helvetica Neue', Helvetica, Arial; font-size:", 
         fontsize, "px; text-align: ", align, "; color: ", color, ";", "font-weight: ", fontweight, ";" 
  )
}

my_rainbow <- c("violet", 
                "blue", 
                "sky blue",
                "cyan", 
                "green", 
                "yellow", 
                "orange",
                "red", 
                "dark red")

# pass temp in K as argument, return hex
my_temp_color <- function(temp) {
  # set scale 
  # color ramp used for temp gradient
  myramp <- scales::colour_ramp(my_rainbow, na.color = "#D5D8DC")
  scaled_temp <- scales::rescale( temp, from = c(223.15, 323.15), to = c(0,1) ) # -50 to +50 C
  myramp(scaled_temp)
}

my_uvi_color <- function(uvi) {
  # color ramp used for index, https://en.wikipedia.org/wiki/Ultraviolet_index
  myramp <- scales::colour_ramp(c("green", "yellow", "red", "dark red"), na.color = "#D5D8DC")
  scaled_uvi <- scales::rescale(uvi, from = c(0, 12), to = c(0, 1))
  myramp(scaled_uvi)
}

# generate gradients for a div, input temps, output style with colors
my_temp_gradient <- function(tempmin, tempmax, forecastmin, forecastmax) {
  color1 <- my_temp_color(tempmin)
  color2 <- my_temp_color(tempmax)
  
  # stop1 is the percentage fill on the left side
  stop1 <- scales::rescale(tempmin, to = c(0, 100),from = c(forecastmin, forecastmax))
  stop2 <- scales::rescale(tempmax, to = c(0, 100), from = c(forecastmin, forecastmax))
  #print(stop1)
  
  # build the gradient with stops at tempmin and tempmax
  # https://css-tricks.com/css3-gradients/
  paste0("height: 5px; width: 120px; border-radius: 5px;
    background: linear-gradient(to right, 
        black, black ", stop1, "%, ", 
        color1, " ", stop1, "%, ", 
        
        color2, " ", stop2, "%, ", 
        "black ", stop2, 
        "%); background-size: 100% 100%; background-repeat: no-repeat;"
        )
}

my_uvi_gradient <- function(uvi_current) {
  
  # rescaled uvi current
   uvi_current_percent <- scales::rescale(uvi_current, to = c(0, 100), from = c(0, 12))
   mycolors <- my_uvi_color(seq(0, uvi_current))
  
  # build the gradient with a circle at uvi_current
  # https://css-tricks.com/css3-gradients/
  paste0("height: 5px; width: 100px; border-radius: 5px; background: linear-gradient(to right, ",
         str_flatten(mycolors, ", "), " ", uvi_current_percent, "%", ", black ", uvi_current_percent, "%", ");",
         #"blue, green, yellow, orange, red);",
         "background-size: 100% 100%; background-repeat: no-repeat;"
  )
}

insertListItem <- function(selection, data, degrees = c("°C", "°F"), timeformat = c(12, 24), clientoffset, language = "en") {
  
  # call once
  # selection is "Berlin, DE", from there we get the lat lon and make one API call

  pattern <- paste0("^", selection, "$")
  hit <- data[str_detect(data$value, pattern), ][1, ] # in case more than one
  lat <- hit$lat
  lon <- hit$lon
  cityid <- hit$id
  
  weather <- get_forecast_onecall(lat, lon, apikey = api_key, language = language) # make reactive to invalidate?
  iconurl <- get_weather_icon(weather$icon)
  
  # chanceOfRain is a vector of length 8, this returns pop for days where there is "Rain" in the weather$daily_main
  
  chanceOfRain <- case_when(
    str_detect(weather$daily_main, "(S|s)now") ~ paste0(", ", "\U2744" ," ", weather$daily_pop * 100, "%"),
    weather$daily_pop > 0 ~ paste0(" ", "\U1F327" ," ", weather$daily_pop * 100, "%"),
    weather$daily_pop == 0 ~ "",
    TRUE ~ ""
  )
  
    
  
                 
  if(degrees == "°C") { 
      temperature <- sprintf("%+3.0f", weather$temp - 273.15)
      feels_like <- sprintf("%+3.0f", weather$feels_like - 273.15)
      daily_tempday <- formatC(weather$daily_tempday - 273.15, format = "f", digits = 1) 
      daily_tempmin <- sprintf("%+3.0f", weather$daily_tempmin - 273.15) 
      daily_tempmax <- sprintf("%+3.0f", weather$daily_tempmax - 273.15) 
      forecast_tempmin <- sprintf("%+3.0f", weather$forecast_tempmin - 273.15)
      forecast_tempmax <- sprintf("%+3.0f", weather$forecast_tempmax - 273.15)
      dew_point <- sprintf("%+3.0f", weather$dew_point - 273.15)
    } else { 
      temperature <- round((weather$temp * 9/5) - 459.67, 0)
      feels_like <- round((weather$feels_like * 9/5) - 459.67, 0)
      daily_tempday <- round((weather$daily_tempday * 9/5) - 459.67, 0)
      daily_tempmin <- round((weather$daily_tempmin * 9/5) - 459.67, 0)
      daily_tempmax <- round((weather$daily_tempmax * 9/5) - 459.67, 0)
      forecast_tempmin <- round((weather$forecast_tempmin * 9/5) - 459.67, 0)
      forecast_tempmax <- round((weather$forecast_tempmax * 9/5) - 459.67, 0)
      dew_point <- round((weather$dew_point * 9/5) - 459.67, 0)
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
    time <- lubridate::now(tzone = "UTC") + weather$tz_offset # in seconds
    #print(time);
    #zoned_time <- clock::zoned_time_now(tz)
    ifelse(timeformat == 24,
    format.POSIXct(lubridate::as_datetime(time), format = "%H:%M"),
    format.POSIXct(lubridate::as_datetime(time), format = "%I:%M %p")
    )
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
                  f7ListItem(tags$div(style = mystyle(fontsize = 18, fontweight = 350, align = "right", color = my_temp_color(weather$temp) ), 
                                      paste0(temperature, "°"), 
                                       tags$span(style = mystyle(fontsize = 12, color = "LightGrey"), 
                                                 chanceOfRain[1]),
                                      tags$div(style = mystyle(fontsize = 15), weather$description), # today
                                      ), 
                             href = "#", # this is used here just to add the class needed to make it look like a clickable link
                             #paste0(weather$temp, " ",weather$weather, " ↑", format.POSIXct(weather$sunrise, format = "%H:%M"), " ↓", format.POSIXct(weather$sunset, format = "%H:%M")) , 
                             #right = selection,  
                             media = apputils::icon(list(src = iconurl, width = "40px"), lib = "local"),
                             title = tags$div( style = mystyle(fontsize = 40, color = "white", fontweight = 200), mytime ), 
                             header = tags$div(style = mystyle(fontsize = 16, fontweight = 350), selection), 
                             footer = tags$div(style = mystyle(fontsize = 13), listItemOffset)
                             )
                  #f7SwipeoutItem(id = paste0("swipe_", cityid), color = "pink", "Alert")
              ),
               f7Popup(id = paste0("popup_", cityid),
                       title = tags$div(style = mystyle(fontsize = 15),
                         paste0(selection)
                         ), 
                       swipeToClose = F, fullsize = T,
                       
                       # Detailed weather for selection
                       #--------------
                       f7List(
                         tags$div(style = mystyle(fontsize = 15), weather$description),
                         f7ListItem(
                           # temp
                           tags$div(style = mystyle(fontsize = 13), 
                                    paste0(daily_tempmin[1], "°", "/", daily_tempmax[1], "°")),
                           
                           title = tags$div(style = mystyle(fontsize = 22, color = my_temp_color(weather$temp)),
                                            paste0(temperature, "°")
                                            ),
                           footer = tags$div(style = mystyle(fontsize = 13, color = "LightGrey"),
                                             paste0("feels like ", feels_like, "°")
                                             ),
                           media = f7Icon("thermometer")
                           ),
                         # humidity and dew
                         f7ListItem(
                           tags$div(style = mystyle(fontsize = 13), paste0("dew point ", dew_point, "°")),
                           title = tags$div(style = mystyle(fontsize = 22, color = "LightGrey"), 
                                            paste0(weather$humidity, "%")
                           ),
                           footer = tags$div(style = mystyle(fontsize = 13, color = "LightGrey"), "Humidity"),
                           media = f7Icon("waveform")
                         ),
                         # UV index
                         f7ListItem(
                           tags$div(style = "margin: 0; width: 100px; font-size: 13px",
                                    paste0( "bla" )),
                           tags$div(style = my_uvi_gradient(uvi_current = weather$uvi)),
                           
                           title = tags$div(style = mystyle(fontsize = 22, color = my_uvi_color(weather$uvi)),
                                            paste0(weather$uvi)
                                                     ), 
                           footer = tags$div(style = mystyle(fontsize = 13, color = "LightGrey"), "UV index"),
                           media = f7Icon("rays")
                           ),
                         
                         
                         # clouds
                         f7ListItem(
                           title = tags$div(style = mystyle(fontsize = 22, color = "LightGrey"), 
                                            paste0(weather$clouds, "%")
                                            ),
                           footer = tags$div(style = mystyle(fontsize = 13, color = "LightGrey"), "Clouds"),
                           media = f7Icon("cloud")
                           ),
                         
                         # wind
                         f7ListItem(
                           tags$div(style = mystyle(fontsize = 13), paste0("wind direction ", weather$wind_deg, "°")),
                           title = tags$div(style = mystyle(fontsize = 22, color = "LightGrey"), 
                                            paste0(weather$wind_speed, " m/s")
                                            ),
                           footer = tags$div(style = mystyle(fontsize = 13, color = "LightGrey"), "Wind"),
                           media = f7Icon("wind")
                           ),
                         
                         # pressure
                         f7ListItem(
                           title = tags$div(style = mystyle(fontsize = 22, color = "LightGrey"), 
                                            paste0(weather$pressure, " hPa")
                                            ),
                           footer = tags$div(style = mystyle(fontsize = 13, color = "LightGrey"), "Pressure"),
                           media = f7Icon("arrow_up_down")
                           ),
                         
                         # visibility   
                         f7ListItem(
                           title = tags$div(style = mystyle(fontsize = 22, color = "LightGrey"), 
                                            paste0(weather$visibility, " m")
                                            ),
                           footer = tags$div(style = mystyle(fontsize = 13, color = "LightGrey"), "Visibility"),
                           media = f7Icon("chevron_up")
                           ),
                         
                         # sunrise/sunset
                         f7ListItem(
                           tags$div(style = mystyle(fontsize = 13), mysunset, f7Icon("sunset")),
                           title = tags$div(style = mystyle(fontsize = 22, color = "LightGrey"), 
                                            paste0(mysunrise)
                           ),
                           footer = tags$div(style = mystyle(fontsize = 13, color = "LightGrey"), "Sunrise/sunset"),
                           media = f7Icon("sunrise")
                           
                         ),
                       ),
                       
                       # 7 days forecast
                       #------------
                       #tags$h3(style = mystyle(fontsize = 18), "7 days forecast"),
                       f7List(
                         tags$div(style = mystyle(fontsize = 15), "7 days forecast"),
                         lapply(seq(weather$daily_main), function(j) { # these are the forecast points
                           iconpath <- get_weather_icon( weather$daily_icon[j] )
                           
                           f7ListItem(
                             tags$div(style = "margin: 0; width: 120px; font-size: 13px",
                                      paste0( daily_tempmin[j], "°"), 
                             tags$span(style = "display: block; float: right; color: LightGrey; font-size: 13px", 
                                      paste0(daily_tempmax[j], "°"))
                             ), 
                             #---------------------------
                             tags$div( style = my_temp_gradient(weather$daily_tempmin[j], 
                                                                weather$daily_tempmax[j], 
                                                                weather$forecast_tempmin, 
                                                                weather$forecast_tempmax)
                                       ),
                             #---------------------------
                             title = tags$div(style = mystyle( fontsize = 22, color = "white" ),
                                            format.POSIXct(anytime(weather$daily_time[j] + weather$tz_offset, asUTC = T), 
                                                           format = "%a")
                                            ),
                             header = format.POSIXct(anytime(weather$daily_time[j] + weather$tz_offset, asUTC = T), 
                                                     format = "%e %b"),
                             footer = tags$div(style = mystyle( fontsize = 13), paste0(weather$daily_description[j], chanceOfRain[j]) ),
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


