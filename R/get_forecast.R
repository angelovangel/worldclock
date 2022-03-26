# get forecast from openweathermap directly

get_forecast <- function(id, apikey, timestamps = 8) {
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
      main = bind_rows(data$list$weather)[["main"]],
      description = bind_rows(data$list$weather)[["description"]],
      icon = bind_rows(data$list$weather)[["icon"]]
    )
  )
}
