library(shiny)

cal_opts <- c("2019")
fis_opts <- c("FY 2020" = "FY2020","FY 2019 (Jan-June only)" = "FY2019")

# Module UI function
yearToggleInput <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    # fileInput(ns("file"), label),
    radioButtons(ns("fy_cy"), label = "Toggle...",
                # ,choices = c("Calendar Year" = "calendar", "Fiscal Year" = "fiscal"), selected = "calendar")
    choiceNames = list(
      tags$span(style = "color:white", "Calendar Year"),
      tags$span(style = "color:white", "Fiscal Year")),
    choiceValues = c("calendar", "fiscal"), selected = "calendar")
    ,conditionalPanel(condition = "input['yt-fy_cy'] == 'calendar'",id = "conditP", 
                      selectInput(ns("cal"), label = "Select...", 
                                  choices = cal_opts, selected = "2019", 
                                  selectize = FALSE, width = "50%")) #,"2020"
    ,conditionalPanel(condition = "input['yt-fy_cy'] == 'fiscal'",id = "conditP", 
                      selectInput(ns("fis"), label = "Select...", 
                                  choices = fis_opts, selected = "FY 2020", 
                                  selectize = FALSE, width = "75%"))
    
  )
}


# Module server function
yearToggle <- function(id) {
  callModule(id = id, function(input, output, session) {
    opt <- reactive({switch(input$fy_cy, "calendar" = input$cal, "fiscal" = input$fis)})
    return(opt)
  })
}
