source("helpers.R")
source("year_toggle.R")

#### UI ####
ui <- dashboardPage(title="Digital dashboard",
                    dashboardHeader(title = logo_beta
                                    ,titleWidth = 250
                                    ,tags$li(class="dropdown",tags$a(href="https://github.com/katger4/digDash", icon("github"), "Source Code", target="_blank"))
                    ),
                    dashboardSidebar(width = 250, sidebarMenu(id = "sidebar_menu",
                                                              menuItem("Overview", tabName = "overview", icon = icon("globe"))
                                                              ,menuItem("New Cards", tabName = "cards", icon = icon("id-card"))
                                                              ,menuItem("Circulation Activity", tabName = "transactions", icon = icon("exchange"))
                                                              ,menuItem("NYPL.org", tabName = "web", icon = icon("newspaper"))
                                                              ,menuItem("Catalog Users", tabName = "users", icon = icon("users"))
                                                              ,menuItem("Catalog Page Views", tabName = "views", icon = icon("eye"))
                                                              ,br()
                                                              ,menuItem("Date range selector", tabName = NULL, div(id = "slxn", yearToggleInput("yt")))
                                                              ,br(), br(), br()
                                                              ,downloadButton('downloadData', 'Download csv', class= "dl_btn", style="margin-left: 30px") 
                    )),
                    dashboardBody(
                      useShinyjs(),
                      
                      # custom css
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }",
                                 ".nvd3 .nv-axis.nv-y text { font-size: 14px; }",
                                 "#yt-fy_cy .control-label, #conditP .control-label { color:white;}",
                                 ".dl_btn { background-color: #35978f !important; color: #000000 !important; }",
                                 "#link_to_cards:hover, #link_to_Vweb:hover, #link_to_Uweb:hover, #link_to_views:hover, #link_to_users:hover { opacity:0.5; }",
                                 ".small-box.bg-black { background-color: #35978f !important; color: #000000 !important; }",
                                 ".small-box.bg-red { background-color: #80cdc1 !important; color: #000000 !important; }",
                                 ".small-box.bg-yellow { background-color: #9970ab !important; color: #000000 !important; }",
                                 ".small-box.bg-aqua { background-color: #5aae61 !important; color: #000000 !important; }",
                                 ".small-box.bg-navy { background-color: #9D9D9D !important; color: #000000 !important; }",
                                 "-webkit-font-smoothing: antialiased;",
                                 "-webkit-filter: blur(0.000001px);"
                      ),
                      dashboardthemes::shinyDashboardThemes(theme = "poor_mans_flatly"),
                      tabItems(
                        tabItem(tabName = "overview",
                                fluidRow(box(width=12
                                             ,title = htmlOutput("latest_day_str")
                                             ,solidHeader = TRUE 
                                             ,status = "primary"
                                             ,align = "center"
                                ))
                                ,fluidRow(column(width=4),column(width=4,valueBoxOutput("card_tot", width = NULL)),column(width=4))
                                ,fluidRow(column(width = 4
                                                 ,fluidRow(br())
                                                 ,fluidRow(valueBoxOutput("Vweb_box", width = 12))
                                                 ,fluidRow(valueBoxOutput("views_tot", width = 12))
                                )
                                ,column(width = 4
                                        ,box(
                                          valueBoxOutput("circ_home_vb", width = NULL)
                                          ,width = NULL
                                          ,plotly::plotlyOutput(outputId = "trans_plot_sum", height = "170px") %>% shinycssloaders::withSpinner(color="#0dc5c1")
                                        )
                                )
                                ,column(width = 4
                                        ,fluidRow(br())
                                        ,fluidRow(valueBoxOutput("Uweb_box", width = 12))
                                        ,fluidRow(valueBoxOutput("users_tot", width = 12))
                                )
                                )
                                ,fluidRow(box(width = 12,"*Circulation activity includes checkins, checkouts, holds, and renewals.", style = "color: gray; font-size: 12px; font-family: Monospace;"))
                        ),
                        tabItem(tabName = "views"
                                ,fluidRow(column(width=4),column(width=4,valueBoxOutput("views_tot_tab", width = NULL)),column(width=4))
                                ,fluidRow(column(width=1)
                                          ,valueBoxOutput("v1",width = 3)
                                          ,valueBoxOutput("v2",width = 3)
                                          ,valueBoxOutput("v3",width = 3)
                                          ,column(width = 2))
                                ,fluidRow(
                                  column(width = 9
                                         ,fluidRow(box(width = 12, height = 450, chartOutput(outputId = "views_plot", "nvd3"),plotOutput("Vplot_for_size", height = "1px")))
                                  )
                                  ,column(width = 3
                                          ,fluidRow(box(width=NULL
                                                        ,checkboxGroupInput("view_opts", "Variable", choices = views_choices, selected = views_choices)
                                                        ,radioButtons(inputId = "Vtime_var", label = "Time", choices = time_choices,
                                                                      selected = "Monthly")))
                                          ,fluidRow(box(width = NULL,uiOutput("log_views")))
                                  ) # views controls col
                                ) # views row
                        ), # views tab
                        tabItem(tabName = "users"
                                ,fluidRow(column(width=4),column(width=4,valueBoxOutput("users_tot_tab", width = NULL)),column(width=4))
                                ,fluidRow(column(width=1)
                                          ,column(width=2,valueBoxOutput("u1", width = NULL)),column(width=2,valueBoxOutput("u2", width = NULL))
                                          ,column(width=2,valueBoxOutput("u3", width = NULL)),column(width=2,valueBoxOutput("u4", width = NULL)),column(width=2,valueBoxOutput("u5", width = NULL))
                                          ,column(width=1))
                                ,fluidRow(
                                  column(width = 9
                                         ,fluidRow(box(width = 12, height = 450, chartOutput(outputId = "users_plot", "nvd3"),plotOutput("Uplot_for_size", height = "1px")))
                                  )
                                  ,column(width = 3
                                          ,fluidRow(box(width=NULL
                                                        ,checkboxGroupInput("user_opts", "Variable", choices = user_choices, selected = user_choices)
                                                        ,radioButtons(inputId = "Utime_var", label = "Time", choices = time_choices,
                                                                      selected = "Monthly")))
                                          ,fluidRow(box(width = NULL,uiOutput("log_users")))
                                  ) # users controls col
                                ) # users row
                        ), # users tab
                        tabItem(tabName = "cards"
                                ,fluidRow(column(width=4),column(width=4,valueBoxOutput("card_tot_tab", width = NULL)),column(width=4))
                                ,fluidRow(
                                  column(width = 9
                                         ,fluidRow(box(width = 12, height = 450, chartOutput(outputId = "card_plot", "nvd3"),plotOutput("Cplot_for_size", height = "1px")))
                                  )
                                  ,column(width = 3
                                          ,fluidRow(box(width=NULL
                                                        ,radioButtons(inputId = "Ctime_var", label = "Time", choices = time_choices, selected = "Monthly")))
                                  ) # control col
                                ) # row
                        ),
                        tabItem(tabName = "web"
                                ,fluidRow(column(width=2),column(width=4,valueBoxOutput("Uweb_box_tab", width = NULL)),column(width=4,valueBoxOutput("Vweb_box_tab", width = NULL)),column(width=2))
                                ,fluidRow(
                                  column(width = 9
                                         ,fluidRow(box(width = 12, height = 450, chartOutput(outputId = "web_plot", "nvd3"),plotOutput("Wplot_for_size", height = "1px")))
                                  )
                                  ,column(width = 3
                                          ,fluidRow(box(width=NULL
                                                        ,checkboxGroupInput("web_opts", "Variable", choices = web_choices, selected = web_choices)
                                                        ,radioButtons(inputId = "Wtime_var", label = "Time", choices = time_choices, selected = "Monthly")))
                                  ) # control col
                                ) # row
                        ),
                        tabItem(tabName = "transactions"
                                ,fluidRow(column(width=4),column(width=4,valueBoxOutput("circ_tab_tot", width = NULL)),column(width=4))
                                ,conditionalPanel("input.circ_vars === 'sierra_trans'"
                                                  ,fluidRow(width = 12,column(width=1),valueBoxOutput("s1",width = 3),valueBoxOutput("s2",width = 3),valueBoxOutput("s3",width = 3),column(width=2)))
                                ,conditionalPanel("input.circ_vars === 'overdrive_trans'"
                                                  ,fluidRow(width = 12,column(width=2),valueBoxOutput("o1",width = 4),valueBoxOutput("o2",width = 4),column(width=2)))
                                ,conditionalPanel("input.circ_vars === 'cloud_trans'"
                                                  ,fluidRow(width = 12,valueBoxOutput("c1",width = 3),valueBoxOutput("c2",width = 3),valueBoxOutput("c3",width = 3),valueBoxOutput("c4",width = 3)))
                                ,fluidRow(
                                  column(width = 9
                                         ,fluidRow(box(width = 12, height = 450, chartOutput(outputId = "trans_plot", "nvd3"),plotOutput("Tplot_for_size", height = "1px")))
                                  )
                                  ,column(width = 3
                                          ,fluidRow(box(width=NULL
                                                        ,radioButtons(inputId = "circ_vars", label = "Circulation System", choices = circ_choices, selected = "sierra_trans")
                                                        ,conditionalPanel("input.circ_vars === 'sierra_trans'", checkboxGroupInput("sierra_opts", "Variable", choices = sierra_choices, selected = sierra_choices))
                                                        ,conditionalPanel("input.circ_vars === 'overdrive_trans'", checkboxGroupInput("odrive_opts", "Variable", choices = odrive_choices, selected = odrive_choices))
                                                        ,conditionalPanel("input.circ_vars === 'cloud_trans'", checkboxGroupInput("cloud_opts", "Variable", choices = cloud_choices, selected = cloud_choices))
                                                        ,radioButtons(inputId = "Ttime_var", label = "Time", choices = time_choices, selected = "Monthly")))
                                  ) # trans controls col
                                ) # trans row
                        ) # trans tab
                      ) # all tabs
                                            ) # db body
                                            )

#### server ####
server <- function(input, output, session) {

#### READ DATA ###
  df <- df_ss %>%
    googlesheets::gs_read_csv(ws = 1) %>%
    drop_na() %>%
    filter_at(vars(-starts_with("date"), -starts_with("week")), any_vars(. != 0)) %>%
    # stopped recording these vars
    select(-overdrive_ebook_holds,-overdrive_audiobook_holds,-weekday) %>%
    mutate(s_month = month(date_dash, label = TRUE),
           s_date = create_d3_date(date_dash),
           s_year = year(date_dash),
           f_month = create_fy_month(date_dash),
           f_year = create_fy_year(date_dash),
           f_quarter = create_fy_qtr(date_dash, f_year),
           s_weekday = factor(weekdays(date_dash), levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"), ordered = TRUE)
    )
  
#### FILTER & SORT DATA ####
  which_year <- yearToggle("yt")
  df_YT <- reactive({
    if(str_detect(which_year(), "FY")){
      df %>% filter(f_year == which_year())
    } else { 
      df %>% filter(year(date_dash) == which_year())
    }
  })
  
  # get max and min dates
  max_d <- reactive({
    format(max(df_YT()$date_dash), "%B %d, %Y")
  })
  
  min_d <- reactive({
    format(min(df_YT()$date_dash), "%B %d, %Y")
  })
  
  # month text (for valueboxes)
  latest_month_abbr <- reactive({paste(month(max(df_YT()$date_dash), label = TRUE), year(max(df_YT()$date_dash)))})
  earliest_month_abbr <- reactive({paste(month(min(df_YT()$date_dash), label = TRUE), year(min(df_YT()$date_dash)))})
  
#### OVERVIEW TEXT ####
  # overview title string, updates with calendar/discal year toggle
  output$latest_day_str <- renderUI({
    year_type <- if(str_detect(which_year(), "FY")){"fiscal year"} else {"calendar year"}
    to_date <- if(str_detect(which_year(), "FY") & which_year() == max(fis_opts)){paste0(" ",str_replace(which_year(), "FY", ""),"-to-date")} 
                else if(!str_detect(which_year(), "FY") & which_year() == max(cal_opts)){"-to-date"} 
                else {paste0(" ",str_replace(which_year(), "FY", ""))}
    HTML(paste0("<b>Daily Digital Data Drop dashboard</b>: ",year_type,to_date," (",min_d()," - ", max_d(),")"))
    })
  
  output$log_views <- renderUI({
    if (!length(input$view_opts) == 1) {
      HTML("<span style='color: gray; font-size: 12px; font-family: Monospace;'><b>Note:</b> This plot uses a <a href='https://en.wikipedia.org/wiki/Logarithmic_scale'>log scale</a> due to the wide range in page views across platforms. Each axis tick corresponds to a <i>ten-fold</i> increase in page views.</span>")
    }
  })
  
  output$log_users <- renderUI({
    if (!length(input$user_opts) == 1) {
      HTML("<span style='color: gray; font-size: 12px; font-family: Monospace;'><b>Note:</b> This plot uses a <a href='https://en.wikipedia.org/wiki/Logarithmic_scale'>log scale</a> due to the wide range in users across platforms. Each axis tick corresponds to a <i>ten-fold</i> increase in users.</span>")
    }
  })
  
#### Vars ####
  circ_var <- reactive({
      switch(input$circ_vars, 
             "sierra_trans" = if (!is.null(input$sierra_opts)) {prep_data(df_YT(), choices = input$sierra_opts, dates = TRUE, key = "transaction_type")}, 
             "overdrive_trans" = if (!is.null(input$odrive_opts)) {prep_data(df_YT(), choices = input$odrive_opts, dates = TRUE, key = "transaction_type")}, 
             "cloud_trans" = if (!is.null(input$cloud_opts)) {prep_data(df_YT(), choices = input$cloud_opts, dates = TRUE, key = "transaction_type")})
  })
  
  user_var <- reactive({
    if (!is.null(input$user_opts)) {
      prep_data(df_YT(), choices = input$user_opts, dates = TRUE, key = "user_type") 
    }
  })
  
  views_var <- reactive({
    if (!is.null(input$view_opts)) {
      prep_data(df_YT(), choices = input$view_opts, dates = TRUE, key = "user_type") 
    }
  })
  
  web_var <- reactive({
    if (!is.null(input$web_opts)) {
      prep_data(df_YT(), choices = input$web_opts, dates = TRUE, key = "user_type") 
    }
  })
  
  card_var <- reactive({prep_data(df_YT(), choices = "new_card_sign_ups", dates=TRUE, card = TRUE)})
  
#### VALUE BOXES ####
  output$circ_home_vb <- renderValueBox({
    circ_var_total <- select_data(df_YT(), choices = c(unname(sierra_choices),unname(odrive_choices),unname(cloud_choices)), dates = FALSE)
    text <- actionLink("link_to_circ", HTML("<span style='font-size:20px; color:#dbdbdb;'>Circulation activity*</span>"))
    valueBox(
      comma_format()(sum(circ_var_total)), text, icon = icon("exchange"),
      color = "olive"
    )
  })
  
  observeEvent(input$link_to_circ, {
    updateTabItems(session, "sidebar_menu", "transactions")
  })
  
  output$card_tot <- output$card_tot_tab <- renderValueBox({
    card_sel <- select_data(df_YT(), choices = "new_card_sign_ups", dates = FALSE)
    if (input$sidebar_menu == "overview") {
      text <- actionLink("link_to_cards", HTML("<span style='font-size:32px; color:#dbdbdb;'>New card sign ups</span>"))
    }
    else {
      text <- HTML(paste("New card sign ups",br(),"<span style='font-size:12px'>",earliest_month_abbr()," - ",latest_month_abbr(),"</span>"))
    }

    valueBox(
      comma_format()(sum(card_sel)), text, icon = icon("id-card"),
      color = "blue"
    )
  })
  
  observeEvent(input$link_to_cards, {
    updateTabItems(session, "sidebar_menu", "cards")
  })
  
  output$Vweb_box <- output$Vweb_box_tab <- renderValueBox({
    Vweb_sel <- select_data(df_YT(), choices = "website_visits_page_views", dates = FALSE)
    if (input$sidebar_menu == "overview") {
      text <- actionLink("link_to_Vweb", HTML("<span style='font-size:20px; color:#dbdbdb;'>NYPL.org page views</span>"))
    }
    else if (any(grepl('view', input$web_opts))) {
      text <- HTML(paste("NYPL.org page views",br(),"<span style='font-size:12px'>",earliest_month_abbr()," - ",latest_month_abbr(),"</span>"))
    }
    
    valueBox(
      comma_format()(sum(Vweb_sel)), text, icon = icon("search"),
      color = "black"
    )
  })
  
  observeEvent(input$link_to_Vweb, {
    updateTabItems(session, "sidebar_menu", "web")
  })
  
  output$Uweb_box <- output$Uweb_box_tab <- renderValueBox({
    Uweb_sel <- select_data(df_YT(), choices = "website_visits_users", dates = FALSE)
    if (input$sidebar_menu == "overview") {
      text <- actionLink("link_to_Uweb", HTML("<span style='font-size:20px; color:#dbdbdb;'>NYPL.org users</span>"))
    }
    else if (any(grepl('user', input$web_opts))) {
      text <- HTML(paste("NYPL.org users",br(),"<span style='font-size:12px'>",earliest_month_abbr()," - ",latest_month_abbr(),"</span>"))
    }
    
    valueBox(
      comma_format()(sum(Uweb_sel)), text, icon = icon("user-circle"),
      color = "black"
    )
  })
  
  observeEvent(input$link_to_Uweb, {
    updateTabItems(session, "sidebar_menu", "web")
  })

  output$views_tot <- output$views_tot_tab <- renderValueBox({ 
    cat_views <- prep_data(df_YT(), choices = views_choices, dates = FALSE, key = "user_type")$count
    if (input$sidebar_menu == "overview") {
      text <- actionLink("link_to_views", HTML("<span style='font-size:20px; color:#dbdbdb;'>Catalog page views</span>"))
    }
    else{
      text <- HTML(paste("Total catalog page views",br(),"<span style='font-size:12px'>",earliest_month_abbr()," - ",latest_month_abbr(),"</span>"))}
    valueBox(
      comma_format()(sum(cat_views)), text, icon = icon("eye"),
      color = "light-blue"
    )
  })
  
  observeEvent(input$link_to_views, {
    updateTabItems(session, "sidebar_menu", "views")
  })
  
  output$users_tot <- output$users_tot_tab <- renderValueBox({
    cat_users <- prep_data(df_YT(), choices = user_choices, dates = FALSE, key = "user_type")$count
    if (input$sidebar_menu == "overview") {
      text <- actionLink("link_to_users", HTML("<span style='font-size:20px; color:#dbdbdb;'>Catalog users</span>"))
    }
    else {
      text <- HTML(paste("Total catalog users",br(),"<span style='font-size:12px'>",earliest_month_abbr()," - ",latest_month_abbr(),"</span>"))
      }
    valueBox(
      comma_format()(sum(cat_users)), text, icon = icon("users"),
      color = "light-blue"
    )
  })
  
  observeEvent(input$link_to_users, {
    updateTabItems(session, "sidebar_menu", "users")
  })
  
  output$v1 <- renderValueBox({
    if (any(grepl('shared', input$view_opts))) {
      v_type <- 'Shared catalog'
      text <- HTML(paste(var_to_label(v_type), "page views", br(),"<span style='font-size:12px'>",earliest_month_abbr()," - ",latest_month_abbr(),"</span>"))
      valueBox(
        comma_format()(sum(views_var() %>% filter(tool_label(var_to_label(user_type)) == v_type) %$% count)), text, icon = icon("handshake"),
        color = "navy"
      )
    }
  })
  
  output$v2 <- renderValueBox({
    if (any(grepl('classic', input$view_opts))) {
    v_type <- 'Classic catalog'
      text <- HTML(paste(var_to_label(v_type), "page views", br(),"<span style='font-size:12px'>",earliest_month_abbr()," - ",latest_month_abbr(),"</span>"))
      valueBox(
        comma_format()(sum(views_var() %>% filter(tool_label(var_to_label(user_type)) == v_type) %$% count)), text, icon = icon("glasses"),
        color = "navy"
      )
    }
  })

  output$v3 <- renderValueBox({
    if (any(grepl('encore', input$view_opts))) {
      v_type <- 'Encore'
      text <- HTML(paste(var_to_label(v_type), "page views", br(),"<span style='font-size:12px'>",earliest_month_abbr()," - ",latest_month_abbr(),"</span>"))
      valueBox(
        comma_format()(sum(views_var() %>% filter(tool_label(var_to_label(user_type)) == v_type) %$% count)), text, icon = icon("retweet"),
        color = "navy"
      )
    }
  })

  output$u1 <- renderValueBox({
    if (any(grepl('shared', input$user_opts))) {
      v_type <- 'Shared catalog'
      text <- HTML(paste(var_to_label(v_type), "users"))
      valueBox(
        comma_format()(sum(user_var() %>% filter(tool_label(var_to_label(user_type)) == v_type) %$% count)), text, icon = icon("handshake"),
        color = "navy"
      )}
  })

  output$u2 <- renderValueBox({
    if (any(grepl('classic', input$user_opts))) {
    v_type <- 'Classic catalog'
      text <- HTML(paste(var_to_label(v_type), "users"))
      valueBox(
        comma_format()(sum(user_var() %>% filter(tool_label(var_to_label(user_type)) == v_type) %$% count)), text, icon = icon("glasses"),
        color = "navy"
      )}
  })

  output$u3 <- renderValueBox({
    if (any(grepl('cloud', input$user_opts))) {
      v_type <- 'CloudLibrary'
      text <- HTML(paste(var_to_label(v_type), "users"))
      valueBox(
        comma_format()(sum(user_var() %>% filter(tool_label(var_to_label(user_type)) == v_type) %$% count)), text, icon = icon("cloud"),
        color = "navy"
      )}
  })

  output$u4 <- renderValueBox({
    if (any(grepl('overdrive', input$user_opts))) {
    v_type <- 'Overdrive'
      text <- HTML(paste(var_to_label(v_type), "users"))
      valueBox(
        comma_format()(sum(user_var() %>% filter(tool_label(var_to_label(user_type)) == v_type) %$% count)), text, icon = icon("headphones"),
        color = "navy"
      )}
  })
  
  output$u5 <- renderValueBox({
    if (any(grepl('encore', input$user_opts))) {
      v_type <- 'Encore'
      text <- HTML(paste(var_to_label(v_type), "users"))
      valueBox(
        comma_format()(sum(user_var() %>% filter(tool_label(var_to_label(user_type)) == v_type) %$% count)), text, icon = icon("retweet"),
        color = "navy"
      )}
  })

  output$circ_tab_tot <- renderValueBox({
    v <- switch(input$circ_vars, "sierra_trans" = 'Sierra', "overdrive_trans" = 'Overdrive', "cloud_trans" = 'CloudLibrary')
    color <- switch(input$circ_vars, "sierra_trans" = 'red', "overdrive_trans" = 'yellow', "cloud_trans" = 'aqua')
    text <- HTML(paste("Total", v, "circulation activity",br(),"<span style='font-size:12px'>",earliest_month_abbr()," - ",latest_month_abbr(),"</span>"))
    circ_var_total <- switch(input$circ_vars, "sierra_trans" = select_data(df_YT(), choices = sierra_choices, dates = FALSE), 
                             "overdrive_trans" = select_data(df_YT(), choices = odrive_choices, dates = FALSE), 
                             "cloud_trans" = select_data(df_YT(), choices = cloud_choices, dates = FALSE))
    valueBox(
      comma_format()(sum(circ_var_total)), text, icon = icon("exchange"),
      color = color
    )
  })

  output$c1 <- output$s1 <- output$o1 <- renderValueBox({
    v_type <- switch(input$circ_vars, "sierra_trans" = 'sierra_checkins', "overdrive_trans" = 'overdrive_audiobook_checkouts', "cloud_trans" = 'cloudlibrary_audiobook_checkouts')
    if (any(grepl(v_type, circ_var()$transaction_type))) {
      text <- HTML(paste(var_to_label(v_type), br(),"<span style='font-size:12px'>",earliest_month_abbr()," - ",latest_month_abbr(),"</span>"))
      icon <- switch(input$circ_vars, "sierra_trans" = icon("book"), "overdrive_trans" = icon("headphones"), "cloud_trans" = icon("headphones"))
      valueBox(
        comma_format()(sum(circ_var() %>% filter(transaction_type == v_type) %$% count)), text, icon = icon,
        color = "navy"
      )}
  })

  output$c2 <- output$s2 <- output$o2 <- renderValueBox({
    v_type <- switch(input$circ_vars, "sierra_trans" = 'sierra_checkouts', "overdrive_trans" = 'overdrive_ebook_checkouts', "cloud_trans" = 'cloudlibrary_audiobook_holds')
    if (any(grepl(v_type, circ_var()$transaction_type))) {
      text <- HTML(paste(var_to_label(v_type), br(),"<span style='font-size:12px'>",earliest_month_abbr()," - ",latest_month_abbr(),"</span>"))
      icon <- switch(input$circ_vars, "sierra_trans" = icon("book-open"), "overdrive_trans" = icon("book-open"), "cloud_trans" = icon("pause-circle"))
      valueBox(
        comma_format()(sum(circ_var() %>% filter(transaction_type == v_type) %$% count)), text, icon = icon,
        color = "navy")}
  })

  output$c3 <- output$s3 <- renderValueBox({
    v_type <- switch(input$circ_vars, "sierra_trans" = 'sierra_renewals', "overdrive_trans" = '', "cloud_trans" = 'cloudlibrary_ebook_checkouts')
    if (any(grepl(v_type, circ_var()$transaction_type))) {
      icon <- switch(input$circ_vars, "sierra_trans" = icon("bookmark"), "cloud_trans" = icon("book-open"))
      if (!input$circ_vars %in% c("overdrive_trans")){
        text <- HTML(paste(var_to_label(v_type), br(),"<span style='font-size:12px'>",earliest_month_abbr()," - ",latest_month_abbr(),"</span>"))
        valueBox(
          comma_format()(sum(circ_var() %>% filter(transaction_type == v_type) %$% count)), text, icon = icon,
          color = "navy"
        )}}
  })

  output$c4 <- renderValueBox({
    v_type <- switch(input$circ_vars, "sierra_trans" = '', "overdrive_trans" = '', "cloud_trans" = 'cloudlibrary_ebook_holds')
    if (any(grepl(v_type, circ_var()$transaction_type))) {
      text <- HTML(paste(var_to_label(v_type), br(),"<span style='font-size:12px'>",earliest_month_abbr()," - ",latest_month_abbr(),"</span>"))
      if (!input$circ_vars %in% c("sierra_trans","overdrive_trans")){
        valueBox(
          comma_format()(sum(circ_var() %>% filter(transaction_type == v_type) %$% count)), text, icon = icon("bookmark"),
          color = "navy"
        )}}
  })

#### PLOTS ####
  output$trans_plot_sum <- plotly::renderPlotly({
    trans <- prep_data(df, choices = c(unname(sierra_choices),unname(odrive_choices),unname(cloud_choices)), dates = TRUE, key = "transaction_type") %>%
      mutate(transaction_group = tool_label(transaction_type),
             hex = cat_color(transaction_group)) %>%
      group_by(transaction_group, hex) %>%
      summarise(tot = sum(count))

    p <- plotly::plot_ly(trans, labels = ~transaction_group, values = ~tot
                 ,marker = list(colors=trans$hex)
                 ,textposition="outside"
                 ,hoverinfo = 'text'
                 ,hovertext =  paste('<b>',trans$transaction_group,'</b> circulation:<br>',comma_format()(trans$tot))
                 ,text = trans$transaction_group
                 ,textinfo = "text"
                 ,rotation = -100
    ) %>%
      plotly::add_pie(hole = 0.6) %>%
      plotly::layout(showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
             ,dragmode =  "zoom"
             ,margin = list(b=20,l=20,r=20,t=20,pad=4))
    plotly::ggplotly(p) %>%
      plotly::config(displayModeBar = F)

  })

  output$views_plot <- renderChart({
    if (input$Vtime_var == "Monthly"){
      monthly <- prep_bars(views_var(), catalog = TRUE, group_var = "user_type", time_var = "s_month", count_var = "log_count")

      if (!length(input$view_opts) == 1) {
        n_base <- nPlot(log_count ~ s_month, group = "user_lab", data = monthly, type = "multiBarChart", width = session$clientData[["output_Vplot_for_size_width"]])
        n_base$chart(color = unique(monthly$hex), showControls = FALSE)
        yTicks <- "#!function (d) { return d3.format(',.0')(Math.round(100*Math.pow(10,d))/100);}!#"
        tt <- "#! function(key, x, y, e){ return '<p><b>' + key + '</b></p><p>' + function(d) { if (d !== 0) {return d3.format(',.0')(Math.round(100*Math.pow(10,d))/100)} else {return 0}; }(e.value) + ' page views in ' + x + ' 2019 </p>'} !#"
        n_base$yAxis(tickValues = seq(from = 1, to = max(monthly$log_count)))
      }
      else {
        n_base <- nPlot(count ~ s_month, data = monthly, type = "multiBarChart", width = session$clientData[["output_Vplot_for_size_width"]])
        n_base$chart(showLegend = FALSE, showControls = FALSE)
        n_base$chart(color = paste("#! function(d){ return '",unique(monthly$hex),"'} !#"))
        yTicks <- "#!d3.format(',.0')!#"
        tt <- paste0("#! function(key, x, y, e){ return '<p><b>",unique(monthly$user_lab),"</b></p><p>' + d3.format(',.0')(e.value) + ' page views in ' + x + ' 2019 </p>'} !#")
      }
      n <- format_nPlot(n_base, list(left = 70), yTicks, plotID = "views_plot", tooltip = tt)
      return(n)

    }
    else if (input$Vtime_var == "Daily") {
      daily <- views_var() %>%
        mutate(user_lab = tool_label(var_to_label(user_type)),
               hex = cat_color(user_type),
               log_count = ifelse(!count == 0, log(count, 10), NA)) %>%
        filter(!is.na(log_count))

      if (!length(input$view_opts) == 1) {
        n_base <- nPlot(log_count ~ s_date, group = "user_lab", data = daily, type = "lineChart", width = session$clientData[["output_Vplot_for_size_width"]])
        yTicks <- "#!function (d) { return d3.format(',.0')(Math.round(100*Math.pow(10,d))/100);}!#"
        xFormat <- "#!function(d) {return d3.time.format.utc('%Y-%m-%d')(new Date(d));} !#"
        tt <- "#! function(key, x, y){ return '<p><b>' + key + '</b></p><p>' + y + ' page views on ' + x + '</p>'} !#"
        n_base$chart(color = unique(daily$hex))
        n_base$yAxis(tickValues = seq(from = 1, to = max(daily$log_count)))
      }
      else {
        n_base <- nPlot(count ~ s_date, data = daily, type = "lineChart", width = session$clientData[["output_Vplot_for_size_width"]])
        yTicks <- "#!d3.format(',.0')!#"
        xFormat <- "#!function(d) {return d3.time.format.utc('%Y-%m-%d')(new Date(d));} !#"
        tt <- paste0("#! function(key, x, y){ return '<p><b>",unique(daily$user_lab),"</b></p><p>' + y + ' page views on ' + x + '</p>'} !#")
        n_base$chart(color = paste("#! function(d){ return '",unique(daily$hex),"'} !#"))
        n_base$chart(showLegend = FALSE)
      }
      n <- format_nPlot(n_base, list(left = 100, right = 100), yTicks, xFormat,"views_plot", tt)
      return(n)
    }
    else if (input$Vtime_var == "Quarterly") {
      quarterly <- prep_bars(views_var(), catalog = TRUE, group_var = "user_type", time_var = "f_quarter", count_var = "log_count")

      if (!length(input$view_opts) == 1) {
        n_base <- nPlot(log_count ~ f_quarter, group = "user_lab", data = quarterly, type = "multiBarChart", width = session$clientData[["output_Vplot_for_size_width"]])
        n_base$chart(color = unique(quarterly$hex), showControls = FALSE)
        yTicks <- "#!function (d) { return d3.format(',.0')(Math.round(100*Math.pow(10,d))/100);}!#"
        tt <- "#! function(key, x, y, e){ return '<p><b>' + key + '</b></p><p>' + function(d) { return d3.format(',.0')(Math.round(100*Math.pow(10,d))/100); }(e.value) + ' page views in ' + x + '</p>'} !#"
        n_base$yAxis(tickValues = seq(from = 1, to = max(quarterly$log_count)))
      }
      else {
        n_base <- nPlot(count ~ f_quarter, data = quarterly, type = "multiBarChart", width = session$clientData[["output_Vplot_for_size_width"]])
        n_base$chart(color = paste("#! function(d){ return '",unique(quarterly$hex),"'} !#"), showControls = FALSE, showLegend = FALSE)
        yTicks <- "#!d3.format(',.0')!#"
        tt <- paste0("#! function(key, x, y, e){ return '<p><b>",unique(quarterly$user_lab),"</b></p><p>' + d3.format(',.0')(e.value) + ' page views in ' + x + '</p>'} !#")
      }
      n <- format_nPlot(n_base, list(left = 80), yTicks, plotID = "views_plot", tooltip = tt)
      return(n)
    }
    else if (input$Vtime_var == "Weekday") {
      wk_daily <- prep_bars(views_var(), catalog = TRUE, group_var = "user_type", time_var = "s_weekday", count_var = "log_count")
        # views_var() %>%
        # group_by(user_type, s_weekday) %>%
        # summarise(count = sum(count)) %>%
        # ungroup() %>%
        # mutate(user_lab = tool_label(var_to_label(user_type)),
        #        hex = cat_color(user_type),
        #        log_count = ifelse(!count == 0, log(count, 10), NA)) %>%
        # filter(!is.na(log_count)) %>%
        # arrange(s_weekday, count)
      
      if (!length(input$view_opts) == 1) {
        n_base <- nPlot(log_count ~ s_weekday, data = wk_daily, group = "user_lab", type = "multiBarChart", width = session$clientData[["output_Vplot_for_size_width"]])
        n_base$chart(color = unique(wk_daily$hex))
        yTicks <- "#!function (d) { return d3.format(',.0')(Math.round(100*Math.pow(10,d))/100);}!#"
        tt <- "#! function(key, x, y, e){ return '<p><b>' + key + '</b></p><p>' + function(d) { return d3.format(',.0')(Math.round(100*Math.pow(10,d))/100); }(e.value) + ' page views on ' + x + 's</p>'} !#"
        n_base$yAxis(tickValues = seq(from = 1, to = max(wk_daily$log_count)))
      }
      else {
        n_base <- nPlot(count ~ s_weekday, data = wk_daily, type = "multiBarChart", width = session$clientData[["output_Vplot_for_size_width"]])
        n_base$chart(color = paste("#! function(d){ return '",unique(wk_daily$hex),"'} !#"), showControls = FALSE, showLegend = FALSE)
        yTicks <- "#!d3.format(',.0')!#"
        tt <- paste0("#! function(key, x, y, e){ return '<p><b>",unique(wk_daily$user_lab),"</b></p><p>' + d3.format(',.0')(e.value) + ' page views on ' + x + 's</p>'} !#")
      }
      n <- format_nPlot(n_base, list(left = 80), yTicks, plotID = "views_plot", tooltip = tt)
      return(n)
    }

  })
  
  output$users_plot <- renderChart({
    if (input$Utime_var == "Monthly"){
      monthly <- prep_bars(user_var(), catalog = TRUE, group_var = "user_type", time_var = "s_month", count_var = "log_count")

      if (!length(input$user_opts) == 1) {
        n_base <- nPlot(log_count ~ s_month, group = "user_lab", data = monthly, type = "multiBarChart", width = session$clientData[["output_Uplot_for_size_width"]])
        n_base$chart(color = unique(monthly$hex), showControls = FALSE)
        yTicks <- "#!function (d) { return d3.format(',.0')(Math.round(100*Math.pow(10,d))/100);}!#"
        tt <- "#! function(key, x, y, e){ return '<p><b>' + key + '</b></p><p>' + function(d) { if (d !== 0) {return d3.format(',.0')(Math.round(100*Math.pow(10,d))/100)} else {return 0}; }(e.value) + ' users in ' + x + ' 2019 </p>'} !#"
        n_base$yAxis(tickValues = seq(from = 1, to = max(monthly$log_count)))
      }
      else {
        n_base <- nPlot(count ~ s_month, data = monthly, type = "multiBarChart", width = session$clientData[["output_Uplot_for_size_width"]])
        n_base$chart(color = paste("#! function(d){ return '",unique(monthly$hex),"'} !#"), showLegend = FALSE, showControls = FALSE)
        yTicks <- "#!d3.format(',.0')!#"
        tt <- paste0("#! function(key, x, y, e){ return '<p><b>",unique(monthly$user_lab),"</b></p><p>' + d3.format(',.0')(e.value) + ' users in ' + x + ' 2019 </p>'} !#")
      }
      n <- format_nPlot(n_base, list(left = 70), yTicks, plotID = "users_plot", tooltip = tt)
      return(n)

    }
    else if (input$Utime_var == "Daily") {
      daily <- user_var() %>%
        mutate(user_lab = tool_label(var_to_label(user_type)),
               hex = cat_color(user_type),
               log_count = ifelse(!count == 0, log(count, 10), NA)) %>%
        filter(!is.na(log_count))

      if (!length(input$user_opts) == 1) {
        n_base <- nPlot(log_count ~ s_date, group = "user_lab", data = daily, type = "lineChart", width = session$clientData[["output_Uplot_for_size_width"]])
        yTicks <- "#!function (d) { return d3.format(',.0')(Math.round(100*Math.pow(10,d))/100);}!#"
        xFormat <- "#!function(d) {return d3.time.format.utc('%Y-%m-%d')(new Date(d));} !#"
        tt <- "#! function(key, x, y){ return '<p><b>' + key + '</b></p><p>' + y + ' users on ' + x + '</p>'} !#"
        n_base$chart(color = unique(daily$hex))
        n_base$yAxis(tickValues = seq(from = 1, to = max(daily$log_count)))
      }
      else {
        n_base <- nPlot(count ~ s_date, data = daily, type = "lineChart", width = session$clientData[["output_Uplot_for_size_width"]])
        yTicks <- "#!d3.format(',.0')!#"
        xFormat <- "#!function(d) {return d3.time.format.utc('%Y-%m-%d')(new Date(d));} !#"
        tt <- paste0("#! function(key, x, y){ return '<p><b>",unique(daily$user_lab),"</b></p><p>' + y + ' users on ' + x + '</p>'} !#")
        n_base$chart(color = paste("#! function(d){ return '",unique(daily$hex),"'} !#"))
        n_base$chart(showLegend = FALSE)
      }
      n <- format_nPlot(n_base, list(left = 100, right = 100), yTicks, xFormat,"users_plot", tt)
      return(n)
    }
    else if (input$Utime_var == "Quarterly") {
      quarterly <- prep_bars(user_var(), catalog = TRUE, group_var = "user_type", time_var = "f_quarter", count_var = "log_count")

      if (!length(input$user_opts) == 1) {
        n_base <- nPlot(log_count ~ f_quarter, group = "user_lab", data = quarterly, type = "multiBarChart", width = session$clientData[["output_Uplot_for_size_width"]])
        n_base$chart(color = unique(quarterly$hex), showControls = FALSE)
        yTicks <- "#!function (d) { return d3.format(',.0')(Math.round(100*Math.pow(10,d))/100);}!#"
        tt <- "#! function(key, x, y, e){ return '<p><b>' + key + '</b></p><p>' + function(d) { return d3.format(',.0')(Math.round(100*Math.pow(10,d))/100); }(e.value) + ' users in ' + x + '</p>'} !#"
        n_base$yAxis(tickValues = seq(from = 1, to = max(quarterly$log_count)))
      }
      else {
        n_base <- nPlot(count ~ f_quarter, data = quarterly, type = "multiBarChart", width = session$clientData[["output_Uplot_for_size_width"]])
        n_base$chart(color = paste("#! function(d){ return '",unique(quarterly$hex),"'} !#"), showControls = FALSE, showLegend = FALSE)
        yTicks <- "#!d3.format(',.0')!#"
        tt <- paste0("#! function(key, x, y, e){ return '<p><b>",unique(quarterly$user_lab),"</b></p><p>' + d3.format(',.0')(e.value) + ' users in ' + x + '</p>'} !#")
      }
      n <- format_nPlot(n_base, list(left = 80), yTicks, plotID = "users_plot", tooltip = tt)
      return(n)
    }
    else if (input$Utime_var == "Weekday") {
      wk_daily <- prep_bars(user_var(), catalog = TRUE, group_var = "user_type", time_var = "s_weekday", count_var = "log_count")
      
      if (!length(input$user_opts) == 1) {
        n_base <- nPlot(log_count ~ s_weekday, data = wk_daily, group = "user_lab", type = "multiBarChart", width = session$clientData[["output_Uplot_for_size_width"]])
        n_base$chart(color = unique(wk_daily$hex))
        yTicks <- "#!function (d) { return d3.format(',.0')(Math.round(100*Math.pow(10,d))/100);}!#"
        tt <- "#! function(key, x, y, e){ return '<p><b>' + key + '</b></p><p>' + function(d) { return d3.format(',.0')(Math.round(100*Math.pow(10,d))/100); }(e.value) + ' users on ' + x + 's</p>'} !#"
        n_base$yAxis(tickValues = seq(from = 1, to = max(wk_daily$log_count)))
      }
      else {
        n_base <- nPlot(count ~ s_weekday, data = wk_daily, type = "multiBarChart", width = session$clientData[["output_Uplot_for_size_width"]])
        n_base$chart(color = paste("#! function(d){ return '",unique(wk_daily$hex),"'} !#"), showControls = FALSE, showLegend = FALSE)
        yTicks <- "#!d3.format(',.0')!#"
        tt <- paste0("#! function(key, x, y, e){ return '<p><b>",unique(wk_daily$user_lab),"</b></p><p>' + d3.format(',.0')(e.value) + ' users on ' + x + 's</p>'} !#")
      }
      n <- format_nPlot(n_base, list(left = 80), yTicks, plotID = "users_plot", tooltip = tt)
      return(n)
    }

  })

  output$card_plot <- renderChart({
    if (input$Ctime_var == "Monthly"){
      monthly <- prep_bars(card_var(), card = TRUE, time_var = "s_month")
      n_base <- nPlot(count ~ s_month, data = monthly, type = "multiBarChart", width = session$clientData[["output_Cplot_for_size_width"]])
      tt <- "#! function(key, x, y, e){ return '<p><b>New card sign ups</b></p><p>' + d3.format(',.0')(e.value) + ' in ' + x + ' 2019 </p>'} !#"
      n <- format_nPlot(n_base, list(left = 70), "#!d3.format(',.0')!#",plotID = "card_plot", tooltip = tt)
      n$chart(color = "#! function(d){ return '#4272B8'} !#", showLegend = FALSE, showControls = FALSE)
      return(n)
    }
    else if (input$Ctime_var == "Daily") {
      daily <- card_var() #prep_data(df_YT(), card=TRUE)

      n_base <- nPlot(count ~ s_date, data = daily, type = "lineChart", width = session$clientData[["output_Cplot_for_size_width"]])
      xFormat <- "#!function(d) {return d3.time.format.utc('%Y-%m-%d')(new Date(d));} !#"
      tt <- "#! function(key, x, y){ return '<p><b>New card sign ups</b></p><p>' + y + ' on ' + x + '</p>'} !#"
      n <- format_nPlot(n_base, list(left = 70, right = 100), "#!d3.format(',.0')!#", xFormat, plotID = "card_plot", tt)
      n$chart(color = "#! function(d){ return '#4272B8'} !#", showLegend = FALSE)
      return(n)
    }
    else if (input$Ctime_var == "Quarterly") {
      quarterly <- prep_bars(card_var(), card = TRUE, time_var = "f_quarter")

      n_base <- nPlot(count ~ f_quarter, data = quarterly, type = "multiBarChart", width = session$clientData[["output_Cplot_for_size_width"]])
      tt <- "#! function(key, x, y, e){ return '<p><b>New card sign ups</b></p><p>' + d3.format(',.0')(e.value) + ' in ' + x + '</p>'} !#"
      n <- format_nPlot(n_base, list(left = 70), "#!d3.format(',.0')!#", plotID = "card_plot", tooltip = tt)
      n$chart(color = "#! function(d){ return '#4272B8'} !#", showLegend = FALSE, showControls = FALSE)
      return(n)
    }
    else if (input$Ctime_var == "Weekday") {
      wk_daily <- prep_bars(card_var(), card = TRUE, time_var = "s_weekday")
      
      n_base <- nPlot(count ~ s_weekday, data = wk_daily, type = "multiBarChart", width = session$clientData[["output_Cplot_for_size_width"]])
      tt <- "#! function(key, x, y, e){ return '<p><b>New card sign ups</b></p><p>' + d3.format(',.0')(e.value) + ' on ' + x + 's</p>'} !#"
      n <- format_nPlot(n_base, list(left = 70), "#!d3.format(',.0')!#", plotID = "card_plot", tooltip = tt)
      n$chart(color = "#! function(d){ return '#4272B8'} !#", showLegend = FALSE, showControls = FALSE)
      return(n)
    }

  })
  
  output$web_plot <- renderChart({
    if (input$Wtime_var == "Monthly"){
      monthly <- prep_bars(web_var(), web = TRUE, group_var = "user_type", time_var = "s_month", count_var = "count")

      if (!length(input$web_opts) == 1) {
        n_base <- nPlot(count ~ s_month, data = monthly, group = "user_lab", type = "multiBarChart", width = session$clientData[["output_Wplot_for_size_width"]])
        n_base$chart(color = unique(monthly$hex))
        tt <- "#! function(key, x, y, e){ return '<p><b>' + key + '</b></p><p>' + d3.format(',.0')(e.value) + ' in ' + x + ' 2019 </p>'} !#"
      }
      else {
        n_base <- nPlot(count ~ s_month, data = monthly, type = "multiBarChart", width = session$clientData[["output_Wplot_for_size_width"]])
        n_base$chart(color = paste("#! function(d){ return '",unique(monthly$hex),"'} !#"), showLegend = FALSE, showControls = FALSE)
        tt <- paste0("#! function(key, x, y, e){ return '<p><b>",unique(monthly$user_lab),"</b></p><p>' + d3.format(',.0')(e.value) + ' in ' + x + ' 2019 </p>'} !#")
      }
      n <- format_nPlot(n_base, list(left = 70), "#!d3.format(',.0')!#", plotID = "web_plot", tooltip = tt)
      return(n)
    }
    else if (input$Wtime_var == "Daily") {
      daily <- web_var() %>%
        mutate(user_lab = ifelse(grepl("views",user_type), "Page views", "Users"),
               hex = trans_shade(user_type))

      n_base <- nPlot(count ~ s_date, group = "user_lab", data = daily, type = "lineChart", width = session$clientData[["output_Wplot_for_size_width"]])
      if (!length(input$web_opts) == 1) {
        n_base$chart(color = unique(daily$hex))
        tt <- "#! function(key, x, y){ return '<p><b>' + key + '</b></p><p>' + y + ' on ' + x + '</p>'} !#"
      }
      else {
        n_base$chart(color = paste("#! function(d){ return '",unique(daily$hex),"'} !#"), showLegend = FALSE)
        tt <- paste0("#! function(key, x, y){ return '<p><b>",unique(daily$user_lab),"</b></p><p>' + y + ' on ' + x + '</p>'} !#")
      }
      xFormat <- "#!function(d) {return d3.time.format.utc('%Y-%m-%d')(new Date(d));} !#"
      n <- format_nPlot(n_base, list(left = 70, right = 100), "#!d3.format(',.0')!#", xFormat,"web_plot", tt)
      return(n)
    }
    else if (input$Wtime_var == "Quarterly") {
      quarterly <- prep_bars(web_var(), web = TRUE, group_var = "user_type", time_var = "f_quarter", count_var = "count")

      if (!length(input$web_opts) == 1) {
        n_base <- nPlot(count ~ f_quarter, data = quarterly, group = "user_lab", type = "multiBarChart", width = session$clientData[["output_Wplot_for_size_width"]])
        n_base$chart(color = unique(quarterly$hex))
        tt <- "#! function(key, x, y, e){ return '<p><b>' + key + '</b></p><p>' + d3.format(',.0')(e.value) + ' in ' + x + '</p>'} !#"
      }
      else {
        n_base <- nPlot(count ~ f_quarter, data = quarterly, type = "multiBarChart", width = session$clientData[["output_Wplot_for_size_width"]])
        n_base$chart(color = paste("#! function(d){ return '",unique(quarterly$hex),"'} !#"), showLegend = FALSE, showControls = FALSE)
        tt <- paste0("#! function(key, x, y, e){ return '<p><b>",unique(quarterly$user_lab),"</b></p><p>' + d3.format(',.0')(e.value) + ' in ' + x + '</p>'} !#")
      }
      n <- format_nPlot(n_base, list(left = 100), "#!d3.format(',.0')!#", plotID = "web_plot", tooltip = tt)
      return(n)
    }
    else if (input$Wtime_var == "Weekday") {
      wk_daily <- prep_bars(web_var(), web = TRUE, group_var = "user_type", time_var = "s_weekday", count_var = "count")

      if (!length(input$web_opts) == 1) {
        n_base <- nPlot(count ~ s_weekday, data = wk_daily, group = "user_lab", type = "multiBarChart", width = session$clientData[["output_Wplot_for_size_width"]])
        n_base$chart(color = unique(wk_daily$hex))
        tt <- "#! function(key, x, y, e){ return '<p><b>' + key + '</b></p><p>' + d3.format(',.0')(e.value) + ' on ' + x + 's</p>'} !#"
      }
      else {
        n_base <- nPlot(count ~ s_weekday, data = wk_daily, type = "multiBarChart", width = session$clientData[["output_Wplot_for_size_width"]])
        n_base$chart(color = paste("#! function(d){ return '",unique(wk_daily$hex),"'} !#"), showLegend = FALSE, showControls = FALSE)
        tt <- paste0("#! function(key, x, y, e){ return '<p><b>",unique(wk_daily$user_lab),"</b></p><p>' + d3.format(',.0')(e.value) + ' on ' + x + 's</p>'} !#")
      }
      n <- format_nPlot(n_base, list(left = 100), "#!d3.format(',.0')!#", plotID = "web_plot", tooltip = tt)
      return(n)
    }

  })

  output$trans_plot <- renderChart({
    if (input$Ttime_var == "Monthly"){
      monthly <- prep_bars(circ_var(), circ = TRUE, group_var = "transaction_type", time_var = "s_month", count_var = "count")

      if (!length(unique(circ_var()$transaction_type)) == 1) {
        n_base <- nPlot(count ~ s_month, group = "transaction_type", data = monthly, type = "multiBarChart", width = session$clientData[["output_Tplot_for_size_width"]])
        n_base$chart(color = unique(monthly$hex))
        tt <- "#! function(key, x, y, e){ return '<p><b>' + key + '</b></p><p>' + d3.format(',.0')(e.value) + ' in ' + x + ' 2019 </p>'} !#"
      }
      else {
        n_base <- nPlot(count ~ s_month, data = monthly, type = "multiBarChart", width = session$clientData[["output_Tplot_for_size_width"]])
        n_base$chart(color = paste("#! function(d){ return '",unique(monthly$hex),"'} !#"), showLegend = FALSE, showControls = FALSE)
        tt <- paste0("#! function(key, x, y, e){ return '<p><b>",unique(monthly$transaction_type),"</b></p><p>' + d3.format(',.0')(e.value) + ' in ' + x + ' 2019 </p>'} !#")
      }
      n <- format_nPlot(n_base, list(left = 70), "#!d3.format(',.0')!#", plotID = "trans_plot", tooltip = tt)
      return(n)

    }
    else if (input$Ttime_var == "Daily") {
      daily <- circ_var() %>%
        mutate(transaction_type = var_to_label(transaction_type),
               hex = trans_shade(transaction_type))

      n_base <- nPlot(count ~ s_date, group = "transaction_type", data = daily, type = "lineChart", width = session$clientData[["output_Tplot_for_size_width"]])
      if (!length(unique(circ_var()$transaction_type)) == 1) {
        n_base$chart(color = unique(daily$hex))
        tt <- "#! function(key, x, y){ return '<p><b>' + key + '</b></p><p>' + y + ' on ' + x + '</p>'} !#"
      }
      else {
        n_base$chart(color = paste("#! function(d){ return '",unique(daily$hex),"'} !#"), showLegend = FALSE)
        tt <- paste0("#! function(key, x, y){ return '<p><b>",unique(daily$transaction_type),"</b></p><p>' + y + ' on ' + x + '</p>'} !#")
      }

      xFormat <- "#!function(d) {return d3.time.format.utc('%Y-%m-%d')(new Date(d));} !#"
      n <- format_nPlot(n_base, list(left = 100, right = 100), "#!d3.format(',.0')!#", xFormat,"trans_plot", tt)
      return(n)
    }
    else if (input$Ttime_var == "Quarterly") {
      quarterly <- prep_bars(circ_var(), circ = TRUE, group_var = "transaction_type", time_var = "f_quarter", count_var = "count")

      if (!length(unique(circ_var()$transaction_type)) == 1) {
        n_base <- nPlot(count ~ f_quarter, group = "transaction_type", data = quarterly, type = "multiBarChart", width = session$clientData[["output_Tplot_for_size_width"]])
        n_base$chart(color = unique(quarterly$hex))
        tt <- "#! function(key, x, y, e){ return '<p><b>' + key + '</b></p><p>' + d3.format(',.0')(e.value) + ' in ' + x + '</p>'} !#"
      }
      else {
        n_base <- nPlot(count ~ f_quarter, data = quarterly, type = "multiBarChart", width = session$clientData[["output_Tplot_for_size_width"]])
        n_base$chart(color = paste("#! function(d){ return '",unique(quarterly$hex),"'} !#"), showLegend = FALSE, showControls = FALSE)
        tt <- paste0("#! function(key, x, y, e){ return '<p><b>",unique(quarterly$transaction_type),"</b></p><p>' + d3.format(',.0')(e.value) + ' in ' + x + '</p>'} !#")
      }
      n <- format_nPlot(n_base,margin = list(left = 100),ytickFormat = "#!d3.format(',.0')!#",plotID = "trans_plot",tooltip = tt)
      return(n)
    }
    else if (input$Ttime_var == "Weekday") {
      wk_daily <- prep_bars(circ_var(), circ = TRUE, group_var = "transaction_type", time_var = "s_weekday", count_var = "count")

      if (!length(unique(circ_var()$transaction_type)) == 1) {
        n_base <- nPlot(count ~ s_weekday, data = wk_daily, group = "transaction_type", type = "multiBarChart", width = session$clientData[["output_Tplot_for_size_width"]])
        n_base$chart(color = unique(wk_daily$hex))
        tt <- "#! function(key, x, y, e){ return '<p><b>' + key + '</b></p><p>' + d3.format(',.0')(e.value) + ' on ' + x + 's</p>'} !#"
      }
      else {
        n_base <- nPlot(count ~ s_weekday, data = wk_daily, type = "multiBarChart", width = session$clientData[["output_Tplot_for_size_width"]])
        n_base$chart(color = paste("#! function(d){ return '",unique(wk_daily$hex),"'} !#"), showLegend = FALSE, showControls = FALSE)
        tt <- paste0("#! function(key, x, y, e){ return '<p><b>",unique(wk_daily$transaction_type),"</b></p><p>' + d3.format(',.0')(e.value) + ' on ' + x + 's</p>'} !#")
      }
      n <- format_nPlot(n_base, list(left = 100), "#!d3.format(',.0')!#", plotID = "trans_plot", tooltip = tt)
      return(n)
    }

  })
  
#### DATA DL ####
  # clean up overview data for download
  out_ov <- reactive({
    df_YT() %>%
      rename(date = date_dash, 
             search_requests_encore_page_views = search_requestsencore__page_views,
             search_requests_encore_users = search_requests_encore__users)
    })
  
  # clean up card data for download
  out_card <- reactive({
    prep_data(df_YT(), card=TRUE) %>% rename(new_card_sign_ups = count)
    })
  
  # which tab is active?
  out_var <- reactive({
    switch(input$sidebar_menu, 
           "overview" = out_ov(), 
           "cards" = out_card(), 
           "transactions" = circ_var(), 
           "web" = web_var(),
           "users" = user_var(),
           "views" = views_var())
  })
  
  # clean up data for download
  out_df <- reactive({
    out_var() %>% 
      rename_all(recode, 
                 date_dash = "date", 
                 user_type = "var_type",
                 transaction_type = "var_type",
                 s_month = "month",
                 s_year = "year",
                 f_year = "fiscal_year",
                 f_quarter = "fiscal_quarter",
                 s_weekday = "weekday") %>%
      mutate_if(is.character, str_replace_all, pattern = 'search_requestsencore__page_views', replacement = 'search_requests_encore_page_views') %>%
      mutate_if(is.character, str_replace_all, pattern = 'search_requests_encore__users', replacement = 'search_requests_encore_users') %>%
      select(-matches("s_date|f_month"))
  })
  
  # download handler
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("DailyDigitalData_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write_csv(out_df(), file)
    })

}

shinyApp(ui = ui, server = server)


