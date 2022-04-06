# get forecast from openweathermap directly

get_forecast <- function(id, timestamps = 8) {
  require(httr)
  require(jsonlite)
  require(dplyr)
  
  api_key <- Sys.getenv("api_key")
  call <- paste0("http://api.openweathermap.org/data/2.5/forecast?id=", id, "&cnt=", timestamps, "&appid=", api_key)
  res <- httr::GET(call)
  data <- jsonlite::fromJSON(rawToChar(res$content))
  
  
  return(
    list(
      time = data$list$dt_txt,
      temp = data$list$main$temp,
      mintemp = min(data$list$main$temp, na.rm = T),
      maxtemp = max(data$list$main$temp, na.rm = T),
      main = bind_rows(data$list$weather)[["main"]],
      description = bind_rows(data$list$weather)[["description"]],
      icon = bind_rows(data$list$weather)[["icon"]]
    )
  )
}

# https://openweathermap.org/api/one-call-api
get_forecast_onecall <- function(lat, lon, exclude = "minutely,hourly", apikey) {
  require(httr)
  require(jsonlite)
  require(dplyr)
  
  lat <- round(lat, 2) #required by the api
  lon <- round(lon, 2)
  
  api_key <- Sys.getenv("api_key")
  call <- paste0("https://api.openweathermap.org/data/2.5/onecall?lat=", lat, "&lon=", lon, "&exclude=", exclude, "&appid=", api_key)
  res <- httr::GET(call)
  data <- jsonlite::fromJSON(rawToChar(res$content))
  
  # [1] because more than one hit may be returned by the api
  if(TRUE) {
  return(
    list(
      time = data$current$dt[1],
      tz = data$timezone[1],
      tz_offset = data$timezone_offset[1], 
      sunrise = data$current$sunrise[1], 
      sunset = data$current$sunset[1],
      temp = data$current$temp[1],
      feels_like = data$current$feels_like,
      main = data$current$weather$main[1],
      description = data$current$weather$description[1],
      icon = data$current$weather$icon[1],
      forecast_tempmin = min(data$daily$temp$min, na.rm = T),
      forecast_tempmax = max(data$daily$temp$max, na.rm = T),
      daily_time = data$daily$dt,
      daily_sunrise = data$daily$sunrise,
      daily_sunset = data$daily$sunset,
      daily_tempmin = data$daily$temp$min,
      daily_tempmax = data$daily$temp$max,
      daily_tempday = data$daily$temp$day,
      daily_pop = data$daily$pop,
      daily_main = bind_rows(data$daily$weather)[["main"]],
      daily_icon = bind_rows(data$daily$weather)[["icon"]]
    )
  )
  } else {
    simpleError("No data obtained from openweathermap.org")
  }
}
