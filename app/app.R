library(shiny)
library(tidyverse)
library(scales)
library(ggthemes)
library(shinydashboard)
library(dashboardthemes)
library(Cairo)
options(shiny.usecairo=T)
library(magrittr)
library(lubridate)
require(rCharts)

df <- readRDS(file = "./data/jun_19.rds")

var_to_label <- function(var_name) {
  var_name %<>%
    gsub('_', ' ', .) %>%
    gsub('sierra', 'Sierra', .) %>%
    gsub('overdrive', 'Overdrive', .) %>%
    gsub('cloudlibrary', 'CloudLibrary', .)
  return(var_name)
}

var_choices <- c("Sierra transactions"="sierra_trans",
                 "Overdrive transactions" = "overdrive_trans",
                 "CloudLibrary transactions" = "cloud_trans")

logo_blue_gradient <- shinyDashboardLogoDIY(
  boldText = "Digital"
  ,mainText = "dashboard"
  ,textSize = 16
  ,badgeText = "BETA"
  ,badgeTextColor = "white"
  ,badgeTextSize = 2
  ,badgeBackColor = "#bcbddc"
  ,badgeBorderRadius = 3)

ui <- dashboardPage(title="Digital dashboard",
                    dashboardHeader(title = logo_blue_gradient,titleWidth = 250),
                    dashboardSidebar(width = 250, sidebarMenu(id = "sidebar_menu",
                                                              menuItem("Overview", tabName = "overview", icon = icon("globe"))
                                                              ,menuItem("Transactions", tabName = "transactions", icon = icon("arrows-alt-h"))
                                                              )),
                    dashboardBody(# hide errors
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }",
                                 "-webkit-font-smoothing: antialiased;",
                                 "-webkit-filter: blur(0.000001px);"
                      ),
                      shinyDashboardThemes(theme = "poor_mans_flatly"),
                      tabItems(tabItem(tabName = "overview",
                                       fluidRow(box(width=12
                                                     ,title = span(HTML("<strong>Daily Digital Data Drop dashboard</strong>")) 
                                                     ,solidHeader = TRUE #,includeHTML("about.html")
                                                     ,status = "primary"
                                                    ))
                                       ,fluidRow(column(width=4),column(width=4,valueBoxOutput("card_tot", width = NULL)),column(width=4))
                                       ,fluidRow(column(width = 4, box(width = NULL, chartOutput(outputId = "user_plot_sum", "nvd3"),plotOutput("plot_for_size1")))
                                                 ,column(width = 4,box(width = NULL, chartOutput(outputId = "views_plot_sum", "nvd3"),plotOutput("plot_for_size2")))
                                                 ,column(width = 4,box(width = NULL, chartOutput(outputId = "trans_plot_sum", "nvd3"),plotOutput("plot_for_size3"))))
                                       ,fluidRow(column(width=3),column(width=3,valueBoxOutput("kan_visits", width = NULL)),column(width=3,valueBoxOutput("kan_plays", width = NULL)),column(width=3))
                                       ),
                               tabItem(tabName = "transactions"
                                       ,fluidRow(column(width=4),column(width=4,valueBoxOutput("tot", width = NULL)),column(width=4))
                                       ,valueBoxOutput("t1",width = 3)
                                       ,valueBoxOutput("t2",width = 3)
                                       ,valueBoxOutput("t3",width = 3)
                                       ,valueBoxOutput("t4",width = 3)
                                       ,fluidRow(
                                         column(width = 9
                                                ,fluidRow(box(width = 12, chartOutput(outputId = "trans_plot", "nvd3"),plotOutput("plot_for_size")))
                                                )
                                         ,column(width = 3
                                                 ,fluidRow(box(width=NULL
                                                               ,radioButtons(inputId = "vars", label = "Variable", choices = var_choices,
                                                                             selected = "sierra_trans")
                                                             ,radioButtons(inputId = "time_var", label = "Time", choices = c("monthly","daily"),
                                                                           selected = "monthly")))
                         )
                       )
                      ))
                                       
                               
                      )
                    )

server <- function(input, output, session) {
  ### VARIABLE ###
  var <- reactive({
    if (input$vars == "sierra_trans"){
      df %>% 
        select(starts_with("sierra"), date_dash) %>%
        gather(transaction_type, count, sierra_checkouts:sierra_renewals) %>%
        mutate(s_month = month(date_dash, label = TRUE),
               s_date = as.double(as.POSIXct(as.Date(date_dash),origin="1970-01-01")) * 1000) %>%
        arrange(s_date)
    }
    else if (input$vars == "overdrive_trans"){
      df %>% 
        select(starts_with("overdrive"), date_dash) %>%
        gather(transaction_type, count, overdrive_ebook_checkouts:overdrive_audiobook_holds) %>%
        mutate(s_month = month(date_dash, label = TRUE),
               s_date = as.double(as.POSIXct(as.Date(date_dash),origin="1970-01-01")) * 1000) %>%
        arrange(s_date)
    }
    else if (input$vars == "cloud_trans"){
      df %>% 
        select(starts_with("cloudlibrary"), date_dash) %>%
        gather(transaction_type, count, cloudlibrary_ebook_checkouts:cloudlibrary_audiobook_holds) %>%
        mutate(s_month = month(date_dash, label = TRUE),
               s_date = as.double(as.POSIXct(as.Date(date_dash),origin="1970-01-01")) * 1000) %>%
        arrange(s_date)
    }
    
  })
  
  ### VALUE BOXES ###
  
  output$card_tot <- renderValueBox({
    text <- HTML(paste("New card sign ups to date",br(),"<span style='font-size:12px'>Jan 2019 - Jun 2019</span>"))
    valueBox(
      comma_format()(sum(df %>% select(new_card_sign_ups))), text, icon = icon("id-card"),
      color = "blue"
    )
  })
  
  output$kan_visits <- renderValueBox({
      text <- HTML(paste("Kanopy visits to date",br(),"<span style='font-size:12px'>Jan 2019 - Jun 2019</span>"))
    valueBox(
      comma_format()(sum(df %>% select(kanopy_visits))), text, icon = icon("eye"),
      color = "blue"
    )
  })
  
  output$kan_plays <- renderValueBox({
      text <- HTML(paste("Kanopy plays to date",br(),"<span style='font-size:12px'>Jan 2019 - Jun 2019</span>"))
    valueBox(
      comma_format()(sum(df %>% select(kanopy_plays))), text, icon = icon("youtube"),
      color = "blue"
    )
  })
  
  output$tot <- renderValueBox({
    v <- if (input$vars == "sierra_trans") {'Sierra'}
            else if (input$vars == "overdrive_trans") {'Overdrive'}
            else if (input$vars == "cloud_trans") {'CloudLibrary'}
    text <- HTML(paste(v, "transactions to date",br(),"<span style='font-size:12px'>Jan 2019 - Jun 2019</span>"))
    valueBox(
      comma_format()(sum(var()$count)), text, icon = icon("stopwatch"),
      color = "blue"
    )
  })
  
  output$t1 <- renderValueBox({
    v_type <- if (input$vars == "sierra_trans") {'sierra_checkins'}
    else if (input$vars == "overdrive_trans") {'overdrive_ebook_checkouts'}
    else if (input$vars == "cloud_trans") {'cloudlibrary_ebook_checkouts'}
    text <- HTML(paste(var_to_label(v_type), "to date",br(),"<span style='font-size:12px'>Jan 2019 - Jun 2019</span>"))
    valueBox(
      comma_format()(sum(var() %>% filter(transaction_type == v_type) %$% count)), text, icon = icon("book"),
      color = "blue"
    )
  })

  output$t2 <- renderValueBox({
    v_type <- if (input$vars == "sierra_trans") {'sierra_checkouts'}
    else if (input$vars == "overdrive_trans") {'overdrive_ebook_holds'}
    else if (input$vars == "cloud_trans") {'cloudlibrary_ebook_holds'}
    text <- HTML(paste(var_to_label(v_type), "to date",br(),"<span style='font-size:12px'>Jan 2019 - Jun 2019</span>"))
    valueBox(
      comma_format()(sum(var() %>% filter(transaction_type == v_type) %$% count)), text, icon = icon("book-open"),
      color = "blue")
    # else {valueBox(NULL,NULL,color = "blue")}
  })

  output$t3 <- renderValueBox({
    v_type <- if (input$vars == "sierra_trans") {'sierra_renewals'}
    else if (input$vars == "overdrive_trans") {'overdrive_audiobook_checkouts'}
    else if (input$vars == "cloud_trans") {'cloudlibrary_audiobook_checkouts'}
    text <- HTML(paste(var_to_label(v_type), "to date",br(),"<span style='font-size:12px'>Jan 2019 - Jun 2019</span>"))
    valueBox(
      comma_format()(sum(var() %>% filter(transaction_type == v_type) %$% count)), text, icon = icon("bookmark"),
      color = "blue"
    )
  })
  
  output$t4 <- renderValueBox({
    v_type <- if (input$vars == "sierra_trans") {''}
    else if (input$vars == "overdrive_trans") {'overdrive_audiobook_holds'}
    else if (input$vars == "cloud_trans") {'cloudlibrary_audiobook_holds'}
    text <- HTML(paste(var_to_label(v_type), "to date",br(),"<span style='font-size:12px'>Jan 2019 - Jun 2019</span>"))
    if (!input$vars == "sierra_trans"){
      valueBox(
      comma_format()(sum(var() %>% filter(transaction_type == v_type) %$% count)), text, icon = icon("book-reader"),
      color = "blue"
    )}
  })
  
  ### PLOTS ###
  output$trans_plot <- renderChart({
    if (input$time_var == "monthly"){
      monthly <- var() %>%
        group_by(transaction_type, s_month) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(transaction_type = var_to_label(transaction_type))
      
      n1 <- nPlot(count ~ s_month, group = "transaction_type", data = monthly, type = "multiBarChart", width = session$clientData[["output_plot_for_size_width"]])
      n1$chart(margin = list(left = 100))
      n1$yAxis( tickFormat = "#!d3.format(',.0')!#" )
      n1$addParams(dom = 'trans_plot')
      n1$chart(tooltipContent = "#! function(key, x, y, e){ return '<p><strong>' + key + '</strong></p><p>' + d3.format(',.0')(e.value) + ' in ' + x + ' 2019 </p>'} !#")
      return(n1) 
      
    } else if (input$time_var == "daily") {
      daily <- var() %>%
        mutate(transaction_type = var_to_label(transaction_type))
      
      n1 <- nPlot(count ~ s_date, group = "transaction_type", data = daily, type = "lineChart", width = session$clientData[["output_plot_for_size_width"]])
      n1$chart(margin = list(left = 100, right = 100))
      n1$yAxis( tickFormat = "#!d3.format(',.0')!#" )
      n1$xAxis( tickFormat = "#!function(d) {return d3.time.format('%Y-%m-%d')(new Date(d));} !#" )
      n1$chart(forceX = c("2019-01-01","2019-06-30"))
      n1$addParams(dom = 'trans_plot')
      # n1$chart(tooltipContent = "#! function(key, x, y, e){ return '<p><strong>' + key + '</strong></p><p>' + e.value + ' in ' + x + ' 2019 </p>'} !#")
      return(n1) 
    }

  })
}

shinyApp(ui = ui, server = server)
