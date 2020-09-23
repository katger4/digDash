library(shiny)

cal_opts <- c("2020","2019")
fis_opts <- c("FY 2021" = "FY2021","FY 2020" = "FY2020","FY 2019 (Jan-June only)" = "FY2019")

# Module UI function
yearToggleInput <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    # fileInput(ns("file"), label),
    div(style = "padding: 0px 0px; margin-top:-2em"
      ,radioButtons(ns("fy_cy"), label = "",
                # ,choices = c("Calendar Year" = "calendar", "Fiscal Year" = "fiscal"), selected = "calendar")
    choiceNames = list(
      tags$span(style = "color:white", "Calendar Year"),
      tags$span(style = "color:white", "Fiscal Year")),
    choiceValues = c("calendar", "fiscal"), selected = "fiscal"))
    ,conditionalPanel(condition = "input['yt-fy_cy'] == 'calendar'",id = "conditP", 
                      div(style = "padding: 0px 0px 10px; margin-top:-2em"
                           ,selectInput(ns("cal"), label = "", 
                                  choices = cal_opts, selected = "2020", 
                                  selectize = FALSE, width = "75%")) #,"2020")
    )
    ,conditionalPanel(condition = "input['yt-fy_cy'] == 'fiscal'",id = "conditP",
                      div(style = "padding: 0px 0px 10px; margin-top:-2em"
                          ,selectInput(ns("fis"), label = "",
                                  choices = fis_opts, selected = "FY 2021",
                                  selectize = FALSE, width = "80%"))
    )
    
  )
}


# Module server function
yearToggle <- function(id) {
  callModule(id = id, function(input, output, session) {
    opt <- reactive({switch(input$fy_cy, "calendar" = input$cal, "fiscal" = input$fis)})
    return(opt)
  })
}
