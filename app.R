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
    selectizeInput(
      "timeZone1",
      label = "Time zone 1",
      choices = tzdbnames,
      selected = 1
    ),
    selectizeInput(
      "timeZone2",
      label = "Time zone 2",
      choices = tzdbnames,
      selected = 1
    ),
    selectizeInput(
      "timeZone3",
      label = "Time zone 3",
      choices = tzdbnames,
      selected = 1
    ),
    collapsed = FALSE
  ),
  dashboardBody(
    #h2("World clock"),
    tags$hr(),
    tags$div(id = "placeholder"),
    
    fluidRow(
      column(width = 4,
             valueBoxOutput("ibox1", width = 12)
             ),
      column(width = 4, 
             valueBoxOutput("ibox2", width = 12)
             ),
      column(width = 4, 
             valueBoxOutput("ibox3", width = 12)
      )
    )
    
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
    myicon <- ifelse(between(get_hour(as.POSIXct(mytime)), 8, 20), "sun", "moon")
    
    return(valueBox(
      paste0(
        format.POSIXct(as_date_time(mytime), format = "%H:%M"), " (", formatC(mydiff, flag = "+"), " h)"
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
  inserted <- c()
  
  observeEvent(input$insertBtn, {
    btn <- input$insertBtn
    id = paste0("txt", btn)
    insertUI(
      selector = "#placeholder",
      ui = tags$div(
        valueBox("test", subtitle = id), 
        id = id
        #tags$p(paste(id))
      )
    )
    inserted <<- c(id, inserted)
  })
  
  observeEvent(input$removeBtn, {
    removeUI(
      ## pass in appropriate div id
      selector = paste0('#', inserted[length(inserted)])
    )
    inserted <<- inserted[-length(inserted)]
  })
  

  
  output$ibox1 <- renderValueBox({
    invalidateLater(5000, session)
    make_valuebox(input$timeZone1)
  })
  
  output$ibox2 <- renderValueBox({
    invalidateLater(5000, session)
    make_valuebox(input$timeZone2)
  })
  
  output$ibox3 <- renderValueBox({
    invalidateLater(5000, session)
    make_valuebox(input$timeZone3)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)