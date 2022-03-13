library(shiny)
library(clock)
library(dplyr)
library(shinydashboard)

tzdbnames <- c(Sys.timezone(), clock::tzdb_names() )

# Define UI for displaying current time ----
ui <- dashboardPage(skin = "black",
  dashboardHeader(),
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
      label = "Time zone 1",
      choices = tzdbnames,
      selected = 1
      ), 
    tags$div(id = "placeholder_S"),
    collapsed = FALSE
  ),
  dashboardBody(
    #h2("World clock"),
    tags$hr(),
    valueBoxOutput("ibox1", width = 4),
    tags$div(id = "placeholder_VB")
    
    # fluidRow(
    #   column(width = 4,
    #          valueBoxOutput("ibox1", width = 12)
    #          ),
    #   column(width = 4, 
    #          valueBoxOutput("ibox2", width = 12)
    #          ),
    #   column(width = 4, 
    #          valueBoxOutput("ibox3", width = 12)
    #   )
    # )
    
  )
  )


  
  
# Define server logic to show current time, update every second ----
server <- function(input, output, session) {
  
  make_valuebox <- function(timeZone) {
    
    mytime <- zoned_time_now(timeZone)
    #mydiff <- time_point_count_between(as_naive_time(Sys.time()), as_naive_time(mytime), precision = "hour")
    mydiff <- difftime(as_naive_time(mytime), Sys.time(), unit = "hours") %>% 
      trunc() %>%
      as.numeric()
    myicon <- ifelse(between(get_hour(as.POSIXct(mytime)), 7, 20), "sun", "moon")
    
    return(
        
        valueBox(width = 4, 
                 value = paste0(
        format.POSIXct(as_date_time(mytime), format = "%H:%M:%S"), " (", formatC(mydiff, flag = "+"), " h)"
        ),
      subtitle = paste0(
        timeZone, " ", format.POSIXct(as_date_time(mytime), format = "%Y-%m-%d")
        ),
      color = ifelse(between(get_hour(as.POSIXct(mytime)), 7, 20), "orange", "blue"),
      icon = icon(myicon)
      
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
          label = paste0("Time zone ", input$insertBtn),
          choices = tzdbnames,
          selected = tzdbnames[0]), 
        id = id_selectize
      )
    )
    
    insertUI(
      selector = "#placeholder_VB",
      ui = tags$div(
            #make_valuebox(""),
            #valueBoxOutput(id_vbox, width = 4),
            renderValueBox({
              invalidateLater(1000, session)
              make_valuebox(input[[id_selectize]])
            }),
          
          id = id_vbox
        #tags$p(paste(id))
      
    )
    )
    
    inserted_VB <<- c(id_vbox, inserted_VB)
    inserted_S <<- c(id_selectize, inserted_S)
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
    invalidateLater(1000, session)
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