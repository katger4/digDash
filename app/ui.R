
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
                    dashboardHeader(title = logo_blue_gradient
                                    ,titleWidth = 250
                                    ,tags$li(class="dropdown",tags$a(href="https://github.com/katger4/digDash", icon("github"), "Source Code", target="_blank"))
                    ),
                    dashboardSidebar(width = 250, sidebarMenu(id = "sidebar_menu",
                                                              menuItem("Overview", tabName = "overview", icon = icon("globe"))
                                                              ,menuItem("New Cards", tabName = "cards", icon = icon("id-card"))
                                                              ,menuItem("Circulation Activity", tabName = "transactions", icon = icon("arrows-alt-h"))
                                                              ,menuItem("NYPL.org", tabName = "web", icon = icon("newspaper"))
                                                              ,menuItem("Catalog Users", tabName = "users", icon = icon("users"))
                                                              ,menuItem("Catalog Page Views", tabName = "views", icon = icon("eye"))
                    )),
                    dashboardBody(
                      useShinyjs(),
                      
                      # custom css
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }",
                                 ".nvd3 .nv-axis.nv-y text { font-size: 14px; }",
                                 "#link_to_cards:hover, #link_to_Vweb:hover, #link_to_Uweb:hover { opacity:0.5; }",
                                 ".small-box.bg-black { background-color: #35978f !important; color: #000000 !important; }",
                                 "-webkit-font-smoothing: antialiased;",
                                 "-webkit-filter: blur(0.000001px);"
                      ),
                      shinyDashboardThemes(theme = "poor_mans_flatly"),
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
                                                 # ,fluidRow(box(uiOutput("c_views_tot"),width = NULL, plotlyOutput(outputId = "cat_views_plot_sum", height = "150px")%>% withSpinner(color="#0dc5c1")))
                                )
                                ,column(width = 4
                                        ,box(uiOutput("circ_tot")
                                             ,width = NULL
                                             ,plotlyOutput(outputId = "trans_plot_sum", height = "350px") %>% withSpinner(color="#0dc5c1")
                                        )
                                )
                                ,column(width = 4
                                        ,fluidRow(br())
                                        ,fluidRow(valueBoxOutput("Uweb_box", width = 12))
                                        ,fluidRow(valueBoxOutput("users_tot", width = 12))
                                        # ,fluidRow(box(uiOutput("nc_user_tot"), width = NULL, plotlyOutput(outputId = "user_plot_sum", height = "200px")%>% withSpinner(color="#0dc5c1")))
                                )
                                )
                                ,fluidRow(box(width = 11,"*Circulation activity includes checkins, checkouts, holds, and renewals.", style = "color: gray; font-size: 12px; font-family: Monospace;"))
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
                                                        ,radioButtons(inputId = "Vtime_var", label = "Time", choices = time_choices,
                                                                      selected = "Monthly")))
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
                                                        ,radioButtons(inputId = "Utime_var", label = "Time", choices = time_choices,
                                                                      selected = "Monthly")))
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
                                ,fluidRow(column(width=4),column(width=4,valueBoxOutput("tot", width = NULL)),column(width=4))
                                ,fluidRow(width = 12
                                          ,valueBoxOutput("t1",width = 3)
                                          ,valueBoxOutput("t2",width = 3)
                                          ,valueBoxOutput("t3",width = 3)
                                          ,valueBoxOutput("t4",width = 3))
                                ,fluidRow(
                                  column(width = 9
                                         ,fluidRow(box(width = 12, height = 450, chartOutput(outputId = "trans_plot", "nvd3"),plotOutput("Tplot_for_size", height = "1px")))
                                  )
                                  ,column(width = 3
                                          ,fluidRow(box(width=NULL
                                                        ,radioButtons(inputId = "vars", label = "Variable", choices = var_choices,
                                                                      selected = "sierra_trans")
                                                        ,radioButtons(inputId = "Ttime_var", label = "Time", choices = time_choices,
                                                                      selected = "Monthly")))
                                  ) # trans controls col
                                ) # trans row
                                ,fluidRow(box(id = "od_foot",width = 11,"*Overdrive holds no longer tallied as of June 6, 2019.", style = "color: gray; font-size: 10px; font-family: Monospace;"))
                        ) # trans tab
                      ) # all tabs
                      
                      
                    ) # db body
)
