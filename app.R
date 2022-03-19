library(shiny)
library(clock)
library(dplyr)
library(shinydashboard)
library(ROpenWeatherMap)
library(apputils)
library(shinyjs)

library(shinyMobile)
source("global.R")


tzdbnames <- c(Sys.timezone(), clock::tzdb_names() )

########################### CSS ##########################
css = HTML("
  .colorday .small-box {
    background-color: #7B7D7D !important;
  }
  .colornight .small-box {
    background-color: #17202A !important;
  }
  .content-wrapper {
    background-color: #17202A;
  }
")


########################### UI ##########################
ui <- f7Page(
  useShinyjs(),
  
  title = "World Clock",
  f7SingleLayout(
    navbar = f7Navbar(
      title = "World Clock",
      f7SmartSelect(virtualList = TRUE, # because of the many elements in the list
                    inputId = "selectTZ", 
                    label = "Select time zone", 
                    choices = tzdbnames, 
                    selected = NULL,
                    openIn = "popup", 
                    multiple = FALSE, 
                    closeOnSelect = TRUE, 
                    popupCloseLinkText = "Cancel"), 
      
      # this will track currently active list elements and will be used in removeItem action
      f7SmartSelect(virtualList = TRUE, # because of the many elements in the list
                    inputId = "selectTZcurrent", 
                    label = "Select items to remove", 
                    choices = Sys.timezone(), 
                    selected = NULL,
                    openIn = "popup", 
                    multiple = FALSE, 
                    closeOnSelect = TRUE, 
                    popupCloseLinkText = "Cancel"),
      
      #tags$hr(),tags$br(),
      hairline = TRUE, shadow = TRUE
    ),
    # main
    f7Segment(
      f7Button("edit", "Edit" , color = "black"),
      f7Button("done", "Done", color = "black"), 
    container = "segment"),
    
    f7Segment(
      f7Button("appendItem", "+", color = "gray"),
      f7Button("removeItem", "-", color = "gray"),
    container = "row"),
    
    #uiOutput("addedItems"),
    f7List(id = "mylist"
           #f7ListItem("list item 1", title = "title")
      # f7Swipeout(
      #   tag = f7ListItem("one"), side = "right", f7SwipeoutItem(id = "del1", color = "pink", "Delete")
      # )
    ),
    
   toolbar = f7Toolbar(position = "bottom", hairline = TRUE) 
  )
)
  
# Define server logic to show current time, update every second ----
########################### server ##########################
server <- function(input, output, session) {
  
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
  observeEvent(input$appendItem, {
      shinyjs::click(id = "selectTZ")
  })
  # emulate click on current items list
  observeEvent(input$removeItem, {
      shinyjs::click(id = "selectTZcurrent")
  })
  
  #  track current list status
  currList <- tzdbnames[c(433, 322, 170)] # Berlin, Tokyo, New York
  
  # start with a list of 3 time zones, otherwise strange things happen with the smartselect input
  lapply(currList, function(x) {
    insertListItem(tz = x)
  } )
  
  # insertUI when selected
  observeEvent(input$selectTZ, {
      selection <- input$selectTZ
      
      insertListItem(selection)
      
      # finally, update selectTZcurrent list
      
      currList <<- c(currList, selection)
      shinyMobile::updateF7SmartSelect(inputId = "selectTZcurrent", choices = currList)
      print(paste0("ins-", selection))
  })
  
  # actually remove items
  observeEvent(input$selectTZcurrent, {
    selection <- input$selectTZcurrent
    removeUI(
      selector = paste0("#item", "_", get_city(input$selectTZcurrent)), # careful here id is the city only 
      multiple = TRUE
    )
    # and update reactive and list
    
    currList <<- currList[ -length(currList) ] # strip last
    
    shinyMobile::updateF7SmartSelect(inputId = "selectTZcurrent", choices = currList)
    print(paste0("del-", selection))
  })
  
  
  
  # main server
  
}

  
#   make_valuebox <- function(timeZone) {
#     
#     mytime <- zoned_time_now(timeZone)
#     #mydiff <- time_point_count_between(as_naive_time(Sys.time()), as_naive_time(mytime), precision = "hour")
#     mydiff <- difftime(as_naive_time(mytime), Sys.time(), unit = "hours") %>% 
#       ceiling() %>%
#       as.numeric()
#     
#     myweather <- get_weather(timeZone)
#     
#     iconurl<- get_weather_icon(myweather$iconcode)
#       
#     myvalue <- tags$p(
#       paste0(
#       format.POSIXct(as_date_time(mytime), format = "%H:%M")," ",
#       myweather$city, " ",
#       myweather$temp, " ", myweather$weather
#       ),  
#     style = "font-size: 50%; color: #D7DBDD;" # tags$p() used to change font size
#     )
#     
#     mysubtitle <- tags$p(
#       paste0(
#       timeZone, " ", 
#       format.POSIXct(as_date_time(mytime), format = "%Y-%m-%d"), " ", 
#       formatC(mydiff, flag = "+"), "HRS", " ",
#       " ↑", format.POSIXct(myweather$sunrise, format = "%H:%M"), " ↓", format.POSIXct(myweather$sunset, format = "%H:%M")
#       ),
#     style = "font-size: 90%; color: #D7DBDD;"
#     )
#     
#     
#     return(
#         
#         apputils::valueBox(width = 12, #apputils for custom icons
#                  value = myvalue,
#                  subtitle = mysubtitle, 
#                  
#                  
#       #color = ifelse(between(get_hour(as.POSIXct(mytime)), 7, 20), "light-blue", "black"),
#       icon = apputils::icon(list(src = iconurl, width = "50px"), 
#                             lib = "local")
#       )
#      
#     )
#   }
#   
#   ## keep track of elements inserted and not yet removed
#   inserted_VB <- c()
#   inserted_S <- c()
#   
#   observeEvent(input$insertBtn, {
#     btn <- input$insertBtn
#     id_vbox <- paste0("id_vb_", btn) # do the id is id_vb_1...
#     id_selectize <- paste0("id_sel_", btn)
#     
#     insertUI(
#       selector = '#placeholder_S',
#       ui = tags$div(
#         selectizeInput(
#           inputId = id_selectize,
#           label = paste0("Time zone ", input$insertBtn + 1),
#           choices = tzdbnames,
#           selected = tzdbnames[0]), 
#         id = id_selectize
#       )
#     )
#     
#     insertUI(
#       selector = "#placeholder_VB", 
#       where = "beforeEnd",
#       ui = tags$div(
#             #make_valuebox(""),
#             #valueBoxOutput(id_vbox, width = 4),
#             # renderText({ 
#             #   invalidateLater(10000, session)
#             #   myweather <- get_weather(input[[id_selectize]])
#             #   paste0("Sunrise: ", myweather$sunrise, " Sunset: ", myweather$sunset, " Weather: ", myweather$weather)
#             # }),
#             renderValueBox({
#               removeClass(id_vbox, "colornight")
#               addClass(id_vbox, "colornight")
#               invalidateLater(5000, session)
#               
#               validate(
#                 need(input[[id_selectize]] != "", "Please select a time zone")
#               )
#               make_valuebox(input[[id_selectize]]) # don't know why but this works and input$id_selectize not
#               
#             }),
#           
#           id = id_vbox
#         #tags$p(paste(id))
#       
#     )
#     )
#     
#     inserted_VB <<- c(inserted_VB, id_vbox)
#     inserted_S <<- c(inserted_S, id_selectize)
#   })
#   
#   observeEvent(input$removeBtn, {
#     removeUI(
#       ## pass in appropriate div id
#       selector = paste0('#', inserted_VB[length(inserted_VB)])
#     )
#     removeUI(
#       selector = paste0('#', inserted_S[length(inserted_S)])
#     )
#     inserted_VB <<- inserted_VB[-length(inserted_VB)]
#     inserted_S <<- inserted_S[-length(inserted_S)]
#   })
#   
#   
#   output$ibox1 <- renderValueBox({
#     #lapply( c("clorday", "colornight"), function(x) removeClass("ibox1", x) )
#     removeClass("ibox1", "colornight")
#     addClass("ibox1", "colornight")
#     
#     invalidateLater(5000, session)
#     
#     validate(
#       need(input$timeZone1 != "", "Please select a time zone")
#     )
#     make_valuebox(input$timeZone1)
#   })
#   
#   # output$ibox2 <- renderValueBox({
#   #   invalidateLater(5000, session)
#   #   make_valuebox(input$timeZone2)
#   # })
#   # 
#   # output$ibox3 <- renderValueBox({
#   #   invalidateLater(5000, session)
#   #   make_valuebox(input$timeZone3)
#   # })
#   
# }

# Create Shiny app ----
shinyApp(ui, server)