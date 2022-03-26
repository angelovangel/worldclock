library(shiny)
library(clock)
library(dplyr)
#library(shinydashboard)
library(ROpenWeatherMap)
library(apputils)
library(shinyjs)
library(stringr)
library(pingr)
library(shinyMobile)

source("R/global.R")
source("R/get_forecast.R")


#tzdbnames <- c(Sys.timezone(), clock::tzdb_names() )
cities <- readRDS("data/cities.rds")

secret_text <- Sys.getenv("secret_text")

########################### UI ##########################
ui <- f7Page(
  options = list(dark = TRUE), # can we update server-side?
  
  useShinyjs(),
  
  tags$script(src = "getClientTimezone.js"),
  
  skin = "ios",
  title = "World Clock",
  f7SingleLayout(
    navbar = f7Navbar(subtitle = f7Button("settings", label = f7Icon("bars", color = "white"), color = "black", size = "small"),
                      #title = "Clock and Weather",
                      leftPanel = F, 
                      rightPanel = F
                      #subNavbar = f7SubNavbar(
                       # f7Button("appendItem", label = f7Icon("plus", color = "blue") , color = "black", size = "medium"),
                        #f7Button("removeItem", label = f7Icon("minus", color = "blue"), color = "black", size = "medium"))
                      ),
    #panels = f7Panel(id = "mypanel", side = "right", effect = "reveal",
                     #f7Segment(container = "segment", HTML("Add/remove cities")),
                     #f7Segment(container = "segment", 
                      #f7Button("appendItem", label = f7Icon("plus", color = "blue") , color = "black", size = "medium"),
                      #f7Button("removeItem", label = f7Icon("minus", color = "blue"), color = "black", size = "medium"))
                     #f7Button("edit", label = f7Icon("ellipsis", color = "blue"), color = "black", size = "small")
                     #f7Radio("units", label = "Units", choices = c("Celsius", "Fahrenheit"))
                     #),
    # main
    f7Segment(container = "row",
              f7Button("appendItem", label = f7Icon("plus", color = "white") , color = "black", size = "small"),
              f7Button("removeItem", label = f7Icon("minus", color = "white"), color = "black", size = "small")
              ),
    
    f7Radio("degrees", "", choices = c("°C", "°F"), selected = "°C"),
    
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
    
    
    #uiOutput("addedItems"),
    f7List(id = "mylist"
           
       #f7Swipeout(id = "myswipeout",
         #tag = f7ListItem("one"), side = "right", f7SwipeoutItem(id = "del1", color = "pink", "Delete")
       #)
    ),
    
    #f7Popup(id = "mypopup", "to be updated"),
    
   toolbar = NULL 
  )
)
  
# Define server logic to show current time, update every second ----
########################### server ##########################
server <- function(input, output, session) {
  
  # try to get client timezone
  observeEvent(input$client_timezone, {
    print(input$client_timezone)
    f7Toast(HTML(paste0("Your timezone is: <br><b>", input$client_timezone, "</b>")), 
            position = "bottom", 
            closeButton = F, closeTimeout = 3000, icon = f7Icon("timer"))
    #print(input$client_offset)
  })
  
  itemsToHide <- c("selectTZ", "selectTZcurrent", "appendItem", "removeItem", "degrees")
  lapply(itemsToHide, shinyjs::hide)
  
  
  observeEvent(input$settings, {
    shinyjs::toggle(id = "appendItem")
    shinyjs::toggle(id = "removeItem")
    shinyjs::toggle(id = "degrees")
  })
  
  # well try to trigger popup, have to add custom shiny input bindings?
  
  
  updateF7SmartSelect("selectTZ", choices = cities$value)
  
  
  # emulate click, show in popup
  observeEvent(input$appendItem, ignoreInit = TRUE, {
      shinyjs::click(id = "selectTZ")
  })
  # emulate click on current items list
  observeEvent(input$removeItem, ignoreInit = TRUE, {
      shinyjs::click(id = "selectTZcurrent")
  })
  
  # main server
  #  track current list status
  currList <- c("Berlin, DE", "New York, US", "Tokyo, JP") # 
  
  # start with a list of 3 time zones, otherwise strange things happen with the smartselect input
  observe({
    # validate(
    #   need(!is.na(pingr::ping("google.com", count = 1)), "No internet connection!")
    # )
    lapply(currList, insertListItem, data = cities, degrees = input$degrees)
  })
  
  # insertUI when selected
  observeEvent(input$selectTZ, ignoreInit = TRUE, {
      myselection <- input$selectTZ
      
      insertListItem(myselection, data = cities, degrees = input$degrees)
      
      if(myselection == "Abu Dhabi, AE") {
          f7Toast(text = secret_text, position = "center", closeButton = F, closeTimeout = 3000, 
                  icon = f7Icon("heart", color = "red")
                  )
      }
      # finally, update selectTZcurrent list
      
      currList <<- c(currList, myselection)
      shinyMobile::updateF7SmartSelect(inputId = "selectTZcurrent", choices = currList, selected = NULL)
      #print(paste0("ins-", currList))
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
    
  })
  
  
}


# Create Shiny app ----
shinyApp(ui, server)