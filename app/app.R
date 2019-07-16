library(shiny)
library(tidyverse)
library(scales)
library(plotly)
library(ggthemes)
library(shinydashboard)
library(dashboardthemes)
library(Cairo)
options(shiny.usecairo=T)
library(magrittr)
library(lubridate)
# devtools::install_github("jcheng5/bubbles")
library(bubbles)
# install.packages("packcircles")
library(packcircles)
library(rCharts)

df <- readRDS(file = "./data/jun_19.rds") 

cat_color <- function(var_name) {
  var_name <- tolower(var_name)
  col<- case_when(grepl("website",var_name) ~ "#35978f",
            grepl("encore",var_name) ~ "#74add1",
            grepl("overdrive",var_name) ~ "#9970ab",
            grepl("cloud",var_name) ~ "#5aae61",
            grepl("shared",var_name) ~ "#c2a5cf",
            grepl("classic",var_name) ~ "#a6dba0",
            grepl("sierra",var_name) ~ "#80cdc1",
            grepl("card",var_name) ~ "#35978f",
            grepl("kanopy",var_name) ~ "#01665e")
  return(col)
}

var_to_label <- function(var_name) {
  var_name %<>%
    gsub('_', ' ', .) %>%
    gsub("(^[[:alpha:]])", "\\U\\1", ., perl=TRUE) %>%
    gsub('Cloudlibrary', 'CloudLibrary', .)
}

overview_tooltip <- function(var_name, value, var_type) {
  paste('<b>',var_name,'</b>:<br>',comma_format()(value),var_type)
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
                                                              ,menuItem("New Cards", tabName = "cards", icon = icon("id-card"))
                                                              ,menuItem("Digital Transactions", tabName = "transactions", icon = icon("arrows-alt-h"))
                                                              ,menuItem("Page Views", tabName = "views", icon = icon("eye"))
                                                              ,menuItem("Users", tabName = "users", icon = icon("users"))
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
                                                    ,"January - June 2019"
                                                    ))
                                       ,fluidRow(column(width=4),column(width=4,valueBoxOutput("card_tot", width = NULL)),column(width=4))
                                       ,fluidRow(column(width = 4
                                                        ,fluidRow(box(HTML(paste("<b>",comma_format()(sum(df %>% select(contains("views"), -contains("catalog")))),"</b>page views"))
                                                                      ,width = NULL, plotlyOutput(outputId = "views_plot_sum", height = "200")))
                                                        ,fluidRow(box(HTML(paste("<b>",comma_format()(sum(df %>% select(intersect(contains("views"), contains("catalog"))))),"</b>catalog page views"))
                                                                      ,width = NULL, plotlyOutput(outputId = "cat_views_plot_sum", height = "150px")))
                                                        )
                                                 ,column(width = 4
                                                         ,box(HTML(paste("<b>",comma_format()(sum(df %>% select(starts_with("sierra"), starts_with("overdrive"), starts_with("cloudlibrary")))),"</b>digital transactions*"))
                                                              ,width = NULL
                                                              ,plotlyOutput(outputId = "trans_plot_sum", height = "350px"
                                                                          )
                                                              )
                                                         )
                                                 ,column(width = 4
                                                         ,fluidRow(box(HTML(paste("<b>",comma_format()(sum(df %>% select(ends_with("users"), -contains("catalog")))),"</b>unique users"))
                                                                       ,width = NULL, plotlyOutput(outputId = "user_plot_sum", height = "200px")))
                                                         ,fluidRow(box(HTML(paste("<b>",comma_format()(sum(df %>% select(intersect(ends_with("users"), contains("catalog"))))),"</b>unique catalog users"))
                                                                       ,width = NULL, plotlyOutput(outputId = "cat_user_plot_sum", height = "150px")))
                                                         )
                                       )
                                       # ,fluidRow(column(width=3),column(width=3,valueBoxOutput("kan_visits", width = NULL)),column(width=3,valueBoxOutput("kan_plays", width = NULL)),column(width=3))
                                       ,fluidRow(box(width = 11,"*Digital transactions include checkins, checkouts, holds, and renewals.", style = "color: gray; font-size: 10px; font-family: Monospace;"))
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
    text <- HTML(paste("New card sign ups",br(),"<span style='font-size:12px'>Jan 2019 - Jun 2019</span>"))
    valueBox(
      comma_format()(sum(df %>% select(new_card_sign_ups))), text, icon = icon("id-card"),
      color = "blue"
    )
  })
  
  output$kan_visits <- renderValueBox({
      text <- HTML(paste("Kanopy visits",br(),"<span style='font-size:12px'>Jan 2019 - Jun 2019</span>"))
    valueBox(
      comma_format()(sum(df %>% select(kanopy_visits))), text, icon = icon("eye"),
      color = "blue"
    )
  })
  
  output$kan_plays <- renderValueBox({
      text <- HTML(paste("Kanopy plays",br(),"<span style='font-size:12px'>Jan 2019 - Jun 2019</span>"))
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
  output$views_plot_sum <- renderPlotly({
    view_sum <- df %>% 
      select(contains("views"), -contains("catalog"), date_dash) %>%
      gather(user_type, count, website_visits_page_views:search_requestsencore__page_views) %>%
      mutate(s_year = year(date_dash)) %>%
      group_by(user_type, s_year) %>%
      summarise(tot = sum(count)) %>%
      ungroup() %>%
      mutate(user_type = var_to_label(user_type),
             user_lab = case_when(grepl("Website",user_type) ~ "Website",
                                  grepl("encore",user_type) ~ "Encore"),
             hex = cat_color(user_lab))

    p <- plot_ly(view_sum, x= ~user_lab, y = ~tot
                 ,type = "bar"
                 ,marker = list(color=view_sum$hex)
                 ,text = comma_format()(view_sum$tot)
                 ,textposition="outside"
                 ,cliponaxis = FALSE
                 ,hoverinfo = 'text'
                 ,hovertext = overview_tooltip(view_sum$user_lab,view_sum$tot,'page views')
                 ) %>%
      layout(showlegend = F
             ,bargap = 0
             ,xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE)
             ,yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
             ,margin = list(b=10,l=10,r=10,t=20,pad=4)
             )
    ggplotly(p) %>% 
      config(displayModeBar = F)
  })
  
  output$cat_views_plot_sum <- renderPlotly({
    view_sum <- df %>% 
      select(intersect(contains("views"), contains("catalog")), date_dash) %>%
      gather(user_type, count, search_requests_classic_catalog_page_views:search_requests_shared_catalog_page_views) %>%
      mutate(s_year = year(date_dash)) %>%
      group_by(user_type, s_year) %>%
      summarise(tot = sum(count)) %>%
      ungroup() %>%
      mutate(user_type = var_to_label(user_type),
             user_lab = case_when(grepl("classic",user_type) ~ "Classic",
                                  grepl("shared",user_type) ~ "Shared"),
             hex = cat_color(user_lab))
    p <- plot_ly(view_sum, x= ~tot, y = ~user_lab
                 ,type = "bar"
                 ,orientation = 'h'
                 ,marker = list(color=view_sum$hex)
                 ,text = comma_format()(view_sum$tot)
                 ,textposition="outside"
                 ,cliponaxis = FALSE
                 ,hoverinfo = 'text'
                 ,hovertext = overview_tooltip(view_sum$user_lab,view_sum$tot,'page views')
    ) %>%
      layout(showlegend = F
             ,bargap = 0
             ,xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
             ,yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE)
             ,margin = list(b=10,l=10,r=60,t=10,pad=4)
      )
    ggplotly(p) %>% 
      config(displayModeBar = F)
  })
  
  output$trans_plot_sum <- renderPlotly({
    trans <- df %>% 
      select(starts_with("sierra"), starts_with("overdrive"), starts_with("cloudlibrary"), date_dash) %>%
      gather(transaction_type, count, sierra_checkouts:cloudlibrary_audiobook_holds) %>%
      mutate(s_year = year(date_dash)) %>%
      mutate(grouper = case_when(grepl("sierra",transaction_type) ~ "Sierra",
                                 grepl("overdrive",transaction_type) ~ "Overdrive",
                                 grepl("cloud",transaction_type) ~ "CloudLibrary")) %>%
      group_by(grouper) %>%
      summarise(tot = sum(count))
    
    packing <- circleProgressiveLayout(trans$tot, sizetype='area')
    cap_bub <- bind_cols(trans, packing)
    dat.gg <- circleLayoutVertices(packing, npoints = 100)
    p <- ggplot(data = dat.gg) +
      # Make the bubbles
      geom_polygon(aes(x, y, group = id, fill=as.factor(id)), alpha = 1, show.legend = FALSE) +
      scale_fill_manual(values = cat_color(cap_bub$grouper)) +
      geom_text(data = cap_bub, aes(x, y, size = tot, label = paste(grouper,comma(tot),sep='\n'))) +
      theme_void() +
      theme(legend.position="none") +
      coord_equal()
    
    gp <- ggplotly(p) %>% 
      config(displayModeBar = F) %>%
      layout(xaxis = list(showgrid = F),
             yaxis = list(showgrid = F))
    
    # needed to remove tooltip from circle (1,2,3)
    for(i in 1:3){
      gp$x$data[[i]]$hoverinfo = 'none'
    }
    # specify hoverinfo manually
    gp$x$data[[4]]$hoverlabel = list(bgcolor = cat_color(cap_bub$grouper))
    gp$x$data[[4]]$hovertext = overview_tooltip(cap_bub$grouper,cap_bub$tot,'digital transactions')
    
    return(gp)  

  })
  
  output$user_plot_sum <- renderPlotly({
    user_sum <- df %>% 
      select(ends_with("users"), -contains("catalog"), date_dash) %>%
      gather(user_type, count, website_visits_users:cloudlibrary_unique_users) %>%
      mutate(s_year = year(date_dash)) %>%
      group_by(user_type, s_year) %>%
      summarise(tot = sum(count)) %>%
      ungroup() %>%
      mutate(user_type = var_to_label(user_type),
             user_lab = case_when(grepl("Website",user_type) ~ "Website",
                                  grepl("Overdrive",user_type) ~ "Overdrive",
                                  grepl("Cloud",user_type) ~ "CloudLibrary",
                                  grepl("encore",user_type) ~ "Encore"),
             hex = cat_color(user_type))
    p <- plot_ly(user_sum, labels = ~user_lab, values = ~tot
                 ,marker = list(colors=user_sum$hex)
                 ,textposition="outside"
                 ,hoverinfo = 'text'
                 ,hovertext = overview_tooltip(user_sum$user_lab,user_sum$tot,'users')) %>%
      add_pie(hole = 0.6) %>%
      layout(showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
             ,margin = list(b=10,l=10,r=10,t=10,pad=4))
    ggplotly(p) %>% 
      config(displayModeBar = F)
  })
  
  output$cat_user_plot_sum <- renderPlotly({
    user_sum <- df %>%
      select(intersect(ends_with("users"), contains("catalog")), date_dash) %>%
      gather(user_type, count, search_requests_classic_catalog_users:search_requests_shared_catalog_users) %>%
      mutate(s_year = year(date_dash)) %>%
      group_by(user_type, s_year) %>%
      summarise(tot = sum(count)) %>%
      ungroup() %>%
      mutate(user_type = var_to_label(user_type),
             user_lab = case_when(grepl("classic",user_type) ~ "Classic",
                                  grepl("shared",user_type) ~ "Shared"),
             hex = cat_color(user_lab))
    p <- plot_ly(user_sum, x= ~tot, y = ~user_lab
                 ,type = "bar"
                 ,orientation = 'h'
                 ,marker = list(color=user_sum$hex)
                 ,text = comma_format()(user_sum$tot)
                 ,textposition="outside"
                 ,cliponaxis = FALSE
                 ,hoverinfo = 'text'
                 ,hovertext = overview_tooltip(user_sum$user_lab,user_sum$tot,'page views')
    ) %>%
      layout(showlegend = F
             ,bargap = 0
             ,xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
             ,yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE)
             ,margin = list(b=10,l=10,r=60,t=10,pad=4)
      )
    ggplotly(p) %>% 
      config(displayModeBar = F)
  })
  
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
