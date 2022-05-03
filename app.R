library(shiny)
#library(clock)
library(dplyr)
library(apputils)
library(shinyjs)
#library(shinydisconnect)
library(stringr)
library(shinyMobile)
library(shiny.pwa)
library(sever)

source("R/global.R")
source("R/get_forecast.R")

# to acces on the lan
#options(shiny.host = "0.0.0.0", shiny.port = 8888)

cities <- readRDS("data/cities500k.rds")

secret_text <- Sys.getenv("secret_text")

lang_choices <- c(
  "Arabic" = "ar",
  "Bulgarian" = "bg",
  "Czech" = "cz",
  "Danish" = "da",
  "German" = "de",
  "Greek" = "el",
  "English" = "en",
  "Persian (Farsi)" = "fa",
  "Finnish" = "fi",
  "French" = "fr",
  "Hindi" = "hi",
  "Croatian" = "hr",
  "Hungarian" = "hu",
  "Italian" = "it",
  "Japanese" = "ja",
  "Korean" = "kr",
  "Norwegian" = "no",
  "Dutch" = "nl",
  "Polish" = "pl",
  "Portuguese" = "pt",
  "Romanian" = "ro",
  "Russian" = "ru",
  "Swedish" = "se",
  "Spanish" = "es",
  "Turkish" = "tr",
  "Chinese" = "zh_cn"
)

########################### UI ##########################
ui <- f7Page(
  options = list(dark = TRUE), # can we update server-side?
  
  useShinyjs(),
  useSever(),
  
  pwa(domain = "http://165.22.73.243/worldclock/", 
      output = "www", 
      icon = "www/icons8-clock-500.png", 
      title = "Clock and Weather"),
  
  tags$script(src = "getClientTimezone.js"),
  
  skin = "ios",
  title = "World Clock",
  f7SingleLayout(
    #includeCSS("www/gradient.css"),
    navbar = f7Navbar(#subtitle = "Clock and weather",
                      title = tags$div(style = mystyle(fontsize = 18, fontweight = 400), "Clock and Weather"),
                      leftPanel = F, 
                      rightPanel = T
                      ), 
    panels = f7Panel(title = tags$div(style = mystyle(fontsize = 18, fontweight = 400), "Settings"),
      #f7Radio("timeformat", "", choices = c(12, 24), selected = 24),
      f7Segment(container = "row",
                f7Button("appendItem", label = f7Icon("plus", color = "white") , color = "black", size = "large"),
                f7Button("removeItem", label = f7Icon("minus", color = "white"), color = "black", size = "large")
      ),
      f7Segment(container = "row",
      f7Button("reset", label = tags$div(style = mystyle(fontsize = 14, fontweight = 400), "Reset", f7Icon("arrow_counterclockwise", color = "white")), color = "black", size = "large"),
      f7Button("about", label = tags$div(style = mystyle(fontsize = 14, fontweight = 400), "About", f7Icon("app", color = "white")), color = "black", size = "large")
      ),
      f7Radio("degrees", "", choices = c("°C", "°F"), selected = "°C"),
      f7Select("language", "Language", choices = lang_choices, selected = "en"),
      side = "right", id = "mypanel", effect = "reveal"),
    
    
    # main
    f7SmartSelect(virtualList = TRUE, # because of the many elements in the list
                  inputId = "selectTZ", 
                  label = "Select city to add", 
                  choices = NULL, 
                  selected = NULL,
                  openIn = "popup", 
                  multiple = FALSE, 
                  closeOnSelect = TRUE, searchbarPlaceholder = "search",
                  popupCloseLinkText = "Cancel"),
    
    f7SmartSelect(#virtualList = TRUE, # because of the many elements in the list
                  inputId = "selectTZcurrent", 
                  label = "Select city to remove", 
                  choices = c("Berlin, DE", "New York, US", "Tokyo, JP"), 
                  selected = NULL,
                  openIn = "popup", 
                  multiple = FALSE, 
                  closeOnSelect = TRUE, 
                  popupCloseLinkText = "Cancel"),
    
    
    
    f7List(id = "mylist"),
    
    #f7Popup(id = "mypopup", "to be updated"),
    
   toolbar = NULL 
  )
)
  
# Define server logic to show current time, update every second ----
########################### server ##########################
server <- function(input, output, session) {
  sever()
  #session$allowReconnect(TRUE)
 
   # try to get client timezone
  observeEvent(input$client_timezone, {
    print(input$client_timezone)
    print(input$client_offset)
    f7Toast(HTML(paste0("Your timezone is: <br><b>", 
                        input$client_timezone, "</b>", "<br>", 
                        "(UTC ", sprintf("%+.0f", -input$client_offset/60), " hours)") # the client offset is UTC relative to client!!
                 ), 
            position = "center", 
            closeButton = F, closeTimeout = 3000, icon = f7Icon("timer"))
    #print(input$client_offset)
  })
  
  itemsToHide <- c("selectTZ", "selectTZcurrent")
  lapply(itemsToHide, shinyjs::hide)
  
  updateF7SmartSelect("selectTZ", choices = cities$value)
  
  
  # emulate click, show in popup
  observeEvent(input$appendItem, ignoreInit = TRUE, {
      shinyjs::click(id = "selectTZ")
      #shinyjs::show("selectTZ")
  })
  
  # emulate click on current items list
  observeEvent(input$removeItem, ignoreInit = TRUE, {
      shinyjs::click(id = "selectTZcurrent")
  })
  
  # observer to close panel on degree select
  observeEvent(input$degrees, ignoreInit = TRUE, {
    updateF7Panel(id = "mypanel")
  })
  
  # close panel on language select
  observeEvent(input$language, ignoreInit = TRUE, {
    updateF7Panel(id = "mypanel")
  })
  
  
  # main server
  #  track current list status
  currList <- c("Berlin, DE", "New York, US", "Tokyo, JP") # 
  
  # start with a list of 3 time zones, otherwise strange things happen with the smartselect input
  observe({
    lapply(currList, insertListItem, data = cities, degrees = input$degrees, timeformat = 24, clientoffset = input$client_offset, language = input$language)
  })
  
  # insertUI when selected
  observeEvent(input$selectTZ, ignoreInit = TRUE, {
      myselection <- input$selectTZ
      
      insertListItem(myselection, data = cities, 
                     degrees = input$degrees, 
                     timeformat = 24,
                     clientoffset = input$client_offset)
      
      if(myselection == "Abu Dhabi, AE" && input$client_timezone == "Asia/Dubai") { # show only there...
          f7Toast(text = secret_text, position = "center", closeButton = F, closeTimeout = 3000, 
                  icon = f7Icon("heart", color = "red")
                  )
      }
      # finally, update selectTZcurrent list
      
      currList <<- c(currList, myselection)
      shinyMobile::updateF7SmartSelect(inputId = "selectTZcurrent", choices = currList, selected = NULL)
      #print(paste0("ins-", currList))
      
      # and toggle panel
      updateF7Panel(id = "mypanel")
      
  })
  
  # actually remove items
  observeEvent(input$selectTZcurrent, ignoreInit = TRUE, {
    myselection <- input$selectTZcurrent
    cityid <- get_cityid(input$selectTZcurrent, cities)
    
    removeUI(
      selector = paste0("#item_", cityid), # careful here id is the city only 
      multiple = FALSE
    )
    # and update list
    
    myindex <- which(str_detect(myselection, currList))
    currList <<- currList[-myindex] # strip last
    
    print(paste0("del-", currList))
    
    shinyMobile::updateF7SmartSelect(inputId = "selectTZcurrent", choices = currList, selected = NULL)
    # and toggle panel
    updateF7Panel(id = "mypanel")
    
  })
  
  
  # reload page on reset
  observeEvent(input$reset, {
    shinyjs::refresh()
  })
  
  # about
  observeEvent(input$about, {
    f7Toast("Data source: https://openweathermap.org/ <br> 
            Source code: https://github.com/angelovangel/",
            position = "center", closeTimeout = NULL,
            closeButton = TRUE)
  })
  
  # really?
   session$onRestore(function() {
     shinyjs::refresh()
   })
}


# Create Shiny app ----
shinyApp(ui, server)