library(shiny)
library(clock)
library(dplyr)
#library(shinydashboard)
library(ROpenWeatherMap)
library(apputils)
library(shinyjs)
library(stringr)

library(shinyMobile)
source("global.R")


#tzdbnames <- c(Sys.timezone(), clock::tzdb_names() )
cities <- readRDS("data/cities.rds")

secret_text <- Sys.getenv("secret_text")

########################### UI ##########################
ui <- f7Page(
  useShinyjs(),
  
  skin = "ios",
  title = "World Clock",
  f7SingleLayout(
    navbar = NULL,
    # main
    f7SmartSelect(virtualList = TRUE, # because of the many elements in the list
                  inputId = "selectTZ", 
                  label = "Select time zone", 
                  choices = NULL, 
                  selected = 1,
                  openIn = "popup", 
                  multiple = FALSE, 
                  closeOnSelect = TRUE, searchbarPlaceholder = "search",
                  popupCloseLinkText = "Cancel"),
    
    f7SmartSelect(#virtualList = TRUE, # because of the many elements in the list
                  inputId = "selectTZcurrent", 
                  label = "Select items to remove", 
                  choices = NULL, 
                  selected = 1,
                  openIn = "popup", 
                  multiple = FALSE, 
                  closeOnSelect = TRUE, 
                  popupCloseLinkText = "Cancel"),
    f7Segment(
      f7Button("edit", "Edit", color = "black", size = "small"),
      f7Button("done", "Done", color = "black", size = "small"), 
    container = "segment"),
    
    f7Segment(
      f7Button("appendItem", label = f7Icon("plus") , color = "gray", size = "small"),
      f7Button("removeItem", label = f7Icon("minus"), color = "gray", size = "small"),
    container = "row"),
    
    #uiOutput("addedItems"),
    f7List(id = "mylist",
           
       #f7Swipeout(id = "myswipeout",
         #tag = f7ListItem("one"), side = "right", f7SwipeoutItem(id = "del1", color = "pink", "Delete")
       #)
    ),
    
   toolbar = f7Toolbar(position = "bottom", hairline = TRUE) 
  )
)
  
# Define server logic to show current time, update every second ----
########################### server ##########################
server <- function(input, output, session) {
  
  updateF7SmartSelect("selectTZ", choices = cities$value)
  
  # hide the smart select, use shinyjs to emulate click on it when + is pressed
  shinyjs::hide(id = "selectTZ")
  shinyjs::hide(id = "selectTZcurrent")
  
  # show on edit only
  shinyjs::hide(id = "appendItem")
  shinyjs::hide(id = "removeItem")
  shinyjs::hide("done")
  
  observeEvent(input$edit, {
    shinyjs::show(id = "appendItem")
    shinyjs::show(id = "removeItem")
    shinyjs::show("done")
    
  })
  
  observeEvent(input$done, {
    shinyjs::hide(id = "appendItem")
    shinyjs::hide(id = "removeItem")
    shinyjs::hide("done")
  })
  
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
  lapply(currList, insertListItem)
  
  # insertUI when selected
  observeEvent(input$selectTZ, ignoreInit = TRUE, {
      myselection <- input$selectTZ
      
      insertListItem(myselection)
      if(myselection == "Dubai, AE") {
          f7Toast(text = secret_text, position = "center", closeButton = F, closeTimeout = 3000, icon = f7Icon("heart"))
      }
      # finally, update selectTZcurrent list
      
      currList <<- c(currList, myselection)
      shinyMobile::updateF7SmartSelect(inputId = "selectTZcurrent", choices = currList, selected = NULL)
      print(paste0("ins-", currList))
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
  
  observeEvent(input$swipe_Tokyo, {
    alert("bla")
  })
  
}


# Create Shiny app ----
shinyApp(ui, server)