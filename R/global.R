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

# taken from apputils
localicon <- function (name, class = NULL){
  if(is.null(name$src))
      stop("'name' must be a named list with a 'src' element
           and optionally 'width' (defaults to 100%).")
  if(is.null(name$width)) name$width <- "100%"
  return(shiny::tags$img(class="img img-local", src=name$src, width=name$width))
}
  
    

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
mystyle <- function(fontsize, align = "left", color = "LightGrey", fontweight = 300, fontstyle = "normal") {
  paste0("font-family: 'Helvetica Neue Ultra Thin', 'Helvetica Neue Light', 'Helvetica Neue', Helvetica, Arial; font-size:", 
         fontsize, "px;", "font-style:", fontstyle, "; text-align: ", align, "; color: ", color, "; font-weight: ", fontweight, ";" 
  )
}

my_rainbow <- c("violet", 
                "blue", 
                "cyan", 
                "green", 
                "yellow", 
                "orange",
                "red", 
                "darkred")

# pass temp in K as argument, return hex
my_temp_color <- function(temp, units = units) {
  # units are: standard, metric, imperial
  temprange <- case_when(
    units == 'standard' ~ c(243.15, 318.15),
    units == 'metric' ~ c(-30, 45),
    units == 'imperial' ~ c(-22, 113)
  )
  # set scale 
  # color ramp used for temp gradient
  myramp <- scales::colour_ramp(my_rainbow, na.color = "#D5D8DC")
  scaled_temp <- scales::rescale( temp, from = temprange, to = c(0,1) ) # -50 to +50 C
  myramp(scaled_temp)
}

my_uvi_color <- function(uvi) {
  # color ramp used for index, https://en.wikipedia.org/wiki/Ultraviolet_index
  myramp <- scales::colour_ramp(c("green", "yellow", "red", "dark red"), na.color = "#D5D8DC")
  scaled_uvi <- scales::rescale(uvi, from = c(0, 12), to = c(0, 1))
  myramp(scaled_uvi)
}

# generate gradients for a div, input temps, output style with colors
my_temp_gradient <- function(tempmin, tempmax, forecastmin, forecastmax, units = units) {
  color1 <- my_temp_color(tempmin, units = units)
  color2 <- my_temp_color(tempmax, units = units)
  
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

take_middle <- function(vec) {vec[ceiling(length(vec)/2)]}

insertListItem <- function(selection, data, timeformat = c(12, 24), clientoffset, language = "en", units = units) {
  
  # call once
  # selection is "Berlin, DE", from there we get the lat lon and make one API call

  pattern <- paste0("^", selection, "$")
  hit <- data[str_detect(data$value, pattern), ][1, ] # in case more than one
  lat <- hit$lat
  lon <- hit$lon
  cityid <- hit$id
  
  #weather 
  weather <- get_weather(endpoint = 'weather', lat = lat, lon = lon, apikey = api_key, language = language, units = units)
  iconurl <- get_weather_icon(weather$weather$icon[1])
  
  forecast <- get_weather(endpoint = 'forecast', lat = lat, lon = lon, apikey = api_key, language = language, units = units)
  
  # chanceOfRain is a vector of length 8, this returns pop for days where there is "Rain" in the weather$daily_main
  chanceOfRain <- case_when(
    #str_detect(weather$daily_main, "(S|s)now") ~ paste0(", ", "\U2744" ," ", weather$daily_pop * 100, "%"),
    forecast$list$pop > 0 ~ paste0(" ", "\U1F327" ," ", forecast$list$pop * 100, " %"),
    forecast$list$pop == 0 ~ "",
    TRUE ~ ""
  )
  
  temperature <- sprintf("%+3.0f", weather$main$temp)
  feels_like <- sprintf("%+3.0f", weather$main$feels_like)
  
  # get these from forecast
  forecast_table <- tibble(
      dt = as.POSIXct(forecast$list$dt),
      pod = forecast$list$sys$pod,
      temp = forecast$list$main$temp, 
      pop = forecast$list$pop,
      main = sapply(forecast$list$weather, function(x) x[]$main),
      desc = sapply(forecast$list$weather, function(x) x[]$description),
      icon = sapply(forecast$list$weather, function(x) x[]$icon)
      )
  
  forecast_daily <- forecast_table %>%
    mutate(day = as_date(dt)) %>% 
    group_by(day) %>% 
    summarise(
      daymint = min(temp, na.rm = T),
      daymaxt = max(temp, na.rm = T),
      pop = max(pop, na.rm = T),
      main = take_middle(main),
      desc = take_middle(desc),
      icon = take_middle(icon)
    )
  
    #group_by(day) %>% 
    #summarise(dmint = min(temp), dmaxt = max(temp))
                 
  # if(degrees == "°C") { 
  #     temperature <- sprintf("%+3.0f", weather$main$temp - 273.15)
  #     feels_like <- sprintf("%+3.0f", weather$main$feels_like - 273.15)
  #     tempmin <- sprintf("%+3.0f", weather$main$temp_min - 273.15)
  #     tempmax <- sprintf("%+3.0f", weather$main$temp_max - 273.15)
  #     #dew_point <- sprintf("%+3.0f", weather$dew_point - 273.15)
  #   } else { 
  #     temperature <- round((weather$main$temp * 9/5) - 459.67, 0)
  #     feels_like <- round((weather$main$feels_like * 9/5) - 459.67, 0)
  #     tempmin <- round((weather$main$temp_min * 9/5) - 459.67, 0)
  #     tempmax <- round((weather$main$temp_max * 9/5) - 459.67, 0)
  #     #dew_point <- round((weather$dew_point * 9/5) - 459.67, 0)
  #   }
  
  weather_description <- str_flatten(weather$weather$description, collapse = ", ")
  mysunrise <- format.POSIXct(anytime(weather$sys$sunrise + weather$timezone, asUTC = T), format = "%H:%M")
  mysunset <- format.POSIXct(anytime(weather$sys$sunset + weather$timezone, asUTC = T), format = "%H:%M")
  
  # calculate location offset in hours relative to client (client offset to UTC in minutes given as param)
  listItemOffset <- renderText({
    invalidateLater(10000)
    # generate diff
    myoffset <- difftime(time1 = lubridate::now(tzone = "UTC") + weather$timezone, 
                         time2 = lubridate::now(tzone = "UTC") + (-clientoffset*60), units = "hours")
    # some calculations to see whether it is today, tomorrow or yesterday 
    day_there <- day( lubridate::now(tzone = "UTC") + weather$timezone )
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
    time <- lubridate::now(tzone = "UTC") + weather$timezone # in seconds
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
    ui = tags$div( 
      id = paste0("item_", cityid), # use cityid as tag.. should be ok
      #tags$hr(),
      f7Swipeout(
        f7ListItem(
          routable = T,
          href = "#", # this is used here just to add the class needed to make it look like a clickable link
          media = localicon(list(src = iconurl, width = "40px")),
          header = tags$div(style = mystyle(fontsize = 16, fontweight = 350), selection), 
          title = tags$div( style = mystyle(fontsize = 40, color = "white", fontweight = 200), mytime ), 
          tags$div(
            style = mystyle(fontsize = 18, fontweight = 350, align = "left", color = my_temp_color(weather$main$temp, units = units) ), 
            paste0(temperature, "°"), 
            tags$span(style = mystyle(fontsize = 14, align = 'right'), chanceOfRain[1], weather_description),
            #tags$div(style = mystyle(fontsize = 15, fontstyle = "italic"), weather_description), # today
          ),
          footer = tags$div(style = mystyle(fontsize = 13), listItemOffset)
        )
      )
    )
  )
  
  # open popup
   onclick(paste0("item_", cityid), 
    shinyMobile::f7Popup(
      id = paste0('popup_', cityid), 
      title = tags$div(style = mystyle(fontsize = 15), paste0("Now: ", selection)),
      backdrop = T, fullsize = F, closeByBackdropClick = T, closeOnEscape = T, page = T,
      f7List(
        f7ListItem(
          # temp
          title = tags$div(
            style = mystyle(fontsize = 22, color = my_temp_color(weather$main$temp, units = units)),
            paste0(temperature, "°")
          ),
          # tags$div(
          #   style = mystyle(fontsize = 13), paste0("L: ",tempmin, "°", " H: ", tempmax, "°")
          # ),
          footer = tags$div(
            style = mystyle(fontsize = 13, color = "LightGrey"),
            paste0("feels like ", feels_like, "°")
          ),
          media = f7Icon("thermometer")
        ),
        
        # humidity
        f7ListItem(
          title = tags$div(
            style = mystyle(fontsize = 22, color = "LightGrey"),
            paste0(weather$main$humidity, " %")
          ),
          footer = tags$div(style = mystyle(fontsize = 13, color = "LightGrey"), "Humidity"),
          media = f7Icon("waveform")
        ),
        
        # clouds
        f7ListItem(
          title = tags$div(
            style = mystyle(fontsize = 22, color = "LightGrey"),
            paste0(weather$clouds$all, " %")
          ),
          footer = tags$div(style = mystyle(fontsize = 13, color = "LightGrey"), "Clouds"),
          media = f7Icon("cloud")
        ),
        # wind
        f7ListItem(
          tags$div(
            style = mystyle(fontsize = 13), paste0("direction ", weather$wind$deg, "°")
          ),
          title = tags$div(
            style = mystyle(fontsize = 22, color = "LightGrey"),
            paste0(weather$wind$speed, " m/s")
          ),
          footer = tags$div(style = mystyle(fontsize = 13, color = "LightGrey"), "Wind"),
          media = f7Icon("wind")
        ),
        # pressure
        f7ListItem(
          title = tags$div(
            style = mystyle(fontsize = 22, color = "LightGrey"),
            paste0(weather$main$pressure, " hPa")
          ),
          footer = tags$div(style = mystyle(fontsize = 13, color = "LightGrey"), "Pressure"),
          media = f7Icon("arrow_up_down")
        )
        # visibility   
        # f7ListItem(
        #   title = tags$div(
        #     style = mystyle(fontsize = 22, color = "LightGrey"),
        #     paste0(weather$visibility, " m")
        #   ),
        #   footer = tags$div(style = mystyle(fontsize = 13, color = "LightGrey"), "Visibility"),
        #   media = f7Icon("chevron_up")
        # )
      ),
      # Forecast
      tags$h3(style = mystyle(fontsize = 15), paste0("Forecast: ", selection)),
      f7List(
        #tags$div(style = mystyle(fontsize = 15), "5 days forecast"),
        lapply(head(seq(forecast_daily$main), -1), function(j) { # these are the forecast points
          iconpath <- get_weather_icon(forecast_daily$icon[j])

          f7ListItem(
            tags$div(style = "margin: 0; width: 120px; font-size: 13px",
              paste0( round(forecast_daily$daymint[j], 0), "°"),
              tags$span(style = "display: block; float: right; color: LightGrey; font-size: 13px",
              paste0( round(forecast_daily$daymaxt[j], 0), "°")
              )
            ),
            #---------------------------
            tags$div(
              style = my_temp_gradient(
                forecast_daily$daymint[j],
                forecast_daily$daymaxt[j],
                min(forecast_daily$daymint, na.rm = T),
                max(forecast_daily$daymaxt, na.rm = T), units = units)
            ),
            #---------------------------
            title = tags$div(
              style = mystyle( fontsize = 22, color = "white" ),
              format(forecast_daily$day[j], format = "%a"),
            ),
            header = format(forecast_daily$day[j], format = "%e %b"),
            #header = format.POSIXct(forecast_table$dt[j], format = "%e %b"),
            footer = tags$div(style = mystyle( fontsize = 13), paste0(forecast_table$desc[j]) ),
            media = localicon(list(src = iconpath, width = "40px"))
          )
        })
      )
    )
   )
}

#open popup #=============================================

# onclick(paste0("item_", cityid), 
#         shinyMobile::updateF7Popup(id = paste0("popup_", cityid))
# )
#============================================================================
