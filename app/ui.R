
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
                                                              ,menuItem("Page Views", tabName = "views", icon = icon("eye"))
                                                              ,menuItem("Users", tabName = "users", icon = icon("users"))
                                                              ,menuItem("New Cards", tabName = "cards", icon = icon("id-card"))
                                                              ,menuItem("Digital Transactions", tabName = "transactions", icon = icon("arrows-alt-h"))
                    )),
                    dashboardBody(# hide errors
                      tags$style(type="text/css",
                                 # ".shiny-output-error { visibility: hidden; }",
                                 # ".shiny-output-error:before { visibility: hidden; }",
                                 "-webkit-font-smoothing: antialiased;",
                                 "-webkit-filter: blur(0.000001px);"
                      ),
                      shinyDashboardThemes(theme = "poor_mans_flatly"),
                      tabItems(
                        tabItem(tabName = "overview",
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
                      tabItem(tabName = "cards"
                              ,fluidRow(
                                column(width = 9
                                       ,fluidRow(box(width = 12, chartOutput(outputId = "card_plot", "nvd3"),plotOutput("plot_for_size_card")))
                                )
                                ,column(width = 3
                                        ,fluidRow(box(width=NULL
                                                      ,radioButtons(inputId = "time_var_card", label = "Time", choices = time_choices, selected = "Monthly")))
                                ) # control col
                              ) # row
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
                                                      ,radioButtons(inputId = "time_var", label = "Time", choices = time_choices,
                                                                    selected = "Monthly")))
                                ) # trans controls col
                              ) # trans row
                      ) # trans tab
                      ) # all tabs
                      
                      
                    ) # db body
)