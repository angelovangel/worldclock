library(shiny)
library(clock)
library(dplyr)
library(shinydashboard)
library(ROpenWeatherMap)
library(apputils)
library(shinyjs)

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

# Define UI for displaying current time ----
########################### UI ##########################
ui <- dashboardPage(skin = "black",
  
  dashboardHeader(title = "Worldclock"),
  dashboardSidebar(
    fluidRow(
      column(width = 6,
             actionButton("insertBtn", "", icon = icon("plus"))),
      column(width = 6,
             actionButton("removeBtn", "", icon = icon("minus")))
    ),
    tags$hr(),
    selectizeInput(
      "timeZone1",
      label = "Your time zone",
      choices = tzdbnames,
      selected = 1
      ), 
    tags$div(id = "placeholder_S"),
    collapsed = TRUE
  ),
  
  dashboardBody(
    useShinyjs(),
    apputils::use_apputils(),
    tags$head(tags$style(css)), # defined above before ui
    
    #h2("World clock"),
    tags$hr(),
    fluidRow(
    valueBoxOutput("ibox1", width = 12)),
    tags$div(id = "placeholder_VB")
    
  )
  )


  
  
# Define server logic to show current time, update every second ----
########################### server ##########################
server <- function(input, output, session) {
  
  make_valuebox <- function(timeZone) {
    
    mytime <- zoned_time_now(timeZone)
    #mydiff <- time_point_count_between(as_naive_time(Sys.time()), as_naive_time(mytime), precision = "hour")
    mydiff <- difftime(as_naive_time(mytime), Sys.time(), unit = "hours") %>% 
      ceiling() %>%
      as.numeric()
    
    myweather <- get_weather(timeZone)
    
    iconurl<- get_weather_icon(myweather$iconcode)
      
    myvalue <- tags$p(
      paste0(
      format.POSIXct(as_date_time(mytime), format = "%H:%M")," ",
      myweather$city, " ",
      myweather$temp, " ", myweather$weather
      ),  
    style = "font-size: 50%; color: #D7DBDD;" # tags$p() used to change font size
    )
    
    mysubtitle <- tags$p(
      paste0(
      timeZone, " ", 
      format.POSIXct(as_date_time(mytime), format = "%Y-%m-%d"), " ", 
      formatC(mydiff, flag = "+"), "HRS", " ",
      " ↑", format.POSIXct(myweather$sunrise, format = "%H:%M"), " ↓", format.POSIXct(myweather$sunset, format = "%H:%M")
      ),
    style = "font-size: 90%; color: #D7DBDD;"
    )
    
    
    return(
        
        apputils::valueBox(width = 12, #apputils for custom icons
                 value = myvalue,
                 subtitle = mysubtitle, 
                 
                 
      #color = ifelse(between(get_hour(as.POSIXct(mytime)), 7, 20), "light-blue", "black"),
      icon = apputils::icon(list(src = iconurl, width = "50px"), 
                            lib = "local")
      )
     
    )
  }
  
  ## keep track of elements inserted and not yet removed
  inserted_VB <- c()
  inserted_S <- c()
  
  observeEvent(input$insertBtn, {
    btn <- input$insertBtn
    id_vbox <- paste0("id_vb_", btn) # do the id is id_vb_1...
    id_selectize <- paste0("id_sel_", btn)
    
    insertUI(
      selector = '#placeholder_S',
      ui = tags$div(
        selectizeInput(
          inputId = id_selectize,
          label = paste0("Time zone ", input$insertBtn + 1),
          choices = tzdbnames,
          selected = tzdbnames[0]), 
        id = id_selectize
      )
    )
    
    insertUI(
      selector = "#placeholder_VB", 
      where = "beforeEnd",
      ui = tags$div(
            #make_valuebox(""),
            #valueBoxOutput(id_vbox, width = 4),
            # renderText({ 
            #   invalidateLater(10000, session)
            #   myweather <- get_weather(input[[id_selectize]])
            #   paste0("Sunrise: ", myweather$sunrise, " Sunset: ", myweather$sunset, " Weather: ", myweather$weather)
            # }),
            renderValueBox({
              removeClass(id_vbox, "colornight")
              addClass(id_vbox, "colornight")
              invalidateLater(5000, session)
              
              validate(
                need(input[[id_selectize]] != "", "Please select a time zone")
              )
              make_valuebox(input[[id_selectize]]) # don't know why but this works and input$id_selectize not
              
            }),
          
          id = id_vbox
        #tags$p(paste(id))
      
    )
    )
    
    inserted_VB <<- c(inserted_VB, id_vbox)
    inserted_S <<- c(inserted_S, id_selectize)
  })
  
  observeEvent(input$removeBtn, {
    removeUI(
      ## pass in appropriate div id
      selector = paste0('#', inserted_VB[length(inserted_VB)])
    )
    removeUI(
      selector = paste0('#', inserted_S[length(inserted_S)])
    )
    inserted_VB <<- inserted_VB[-length(inserted_VB)]
    inserted_S <<- inserted_S[-length(inserted_S)]
  })
  
  
  output$ibox1 <- renderValueBox({
    #lapply( c("clorday", "colornight"), function(x) removeClass("ibox1", x) )
    removeClass("ibox1", "colornight")
    addClass("ibox1", "colornight")
    
    invalidateLater(5000, session)
    
    validate(
      need(input$timeZone1 != "", "Please select a time zone")
    )
    make_valuebox(input$timeZone1)
  })
  
  # output$ibox2 <- renderValueBox({
  #   invalidateLater(5000, session)
  #   make_valuebox(input$timeZone2)
  # })
  # 
  # output$ibox3 <- renderValueBox({
  #   invalidateLater(5000, session)
  #   make_valuebox(input$timeZone3)
  # })
  
}

# Create Shiny app ----
shinyApp(ui, server)