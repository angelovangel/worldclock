library(shiny)
#library(clock)
library(dplyr)
library(apputils)
library(shinyjs)
library(shinydisconnect)
library(stringr)
library(shinyMobile)
library(shiny.pwa)

source("R/global.R")
source("R/get_forecast.R")

# to acces on the lan
options(shiny.host = "0.0.0.0", shiny.port = 8888)

#tzdbnames <- c(Sys.timezone(), clock::tzdb_names() )
cities <- readRDS("data/cities500k.rds")

secret_text <- Sys.getenv("secret_text")

########################### UI ##########################
ui <- f7Page(
  options = list(dark = TRUE), # can we update server-side?
  shinydisconnect::disconnectMessage(
    text = "Session timed out", refresh = "Refresh", size = 30, colour = "grey", 
    background = "rgba(39, 55, 70, 0.9)", width = "full", top = "center", 
    overlayColour = "#999", overlayOpacity = 1, 
    css = "padding: 15px !important; box-shadow: none !important;"
  ),
  
  useShinyjs(),
  pwa(domain = "http://165.22.73.243/worldclock/", output = "www", icon = "www/icons8-clock-500.png"),
  
  tags$script(src = "getClientTimezone.js"),
  
  skin = "ios",
  title = "World Clock",
  f7SingleLayout(
    navbar = f7Navbar(#subtitle = "Clock and weather",
                      title = "Clock and Weather",
                      leftPanel = F, 
                      rightPanel = T
                      ), 
    panels = f7Panel(
      f7Segment(container = "row",
                f7Button("appendItem", label = f7Icon("plus", color = "white") , color = "black", size = "large"),
                f7Button("removeItem", label = f7Icon("minus", color = "white"), color = "black", size = "large")
      ), 
      f7Radio("degrees", "", choices = c("°C", "°F"), selected = "°C"),
      f7Button("reset", label = f7Icon("arrow_counterclockwise", color = "white"), color = "black", size = "large"),
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
  
  session$allowReconnect(TRUE)
 
   # try to get client timezone
  observeEvent(input$client_timezone, {
    print(input$client_timezone)
    print(input$client_offset)
    f7Toast(HTML(paste0("Your timezone is: <br><b>", 
                        input$client_timezone, "</b>", "<br>", 
                        "(UTC ",input$client_offset/60, " hours)")
                 ), 
            position = "center", 
            closeButton = F, closeTimeout = 3000, icon = f7Icon("timer"))
    #print(input$client_offset)
  })
  
  itemsToHide <- c("selectTZ", "selectTZcurrent")
  lapply(itemsToHide, shinyjs::hide)
  
  
  #updateSelectizeInput("selectTZ", choices = cities$value, session = session, server = T)
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
  
  # really?
  session$onSessionEnded(function() {
    shinyjs::refresh()
  })
}


# Create Shiny app ----
shinyApp(ui, server)