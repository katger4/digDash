server <- function(input, output, session) {

  df <- df_ss %>%
    gs_read_csv(ws = 1) %>%
    drop_na() %>%
    filter_at(vars(-starts_with("date"), -starts_with("week")), any_vars(. != 0))
  
  # saveRDS(df, './app/jul.rds')
  # df <- readRDS('./data/jun31.rds')
  
  latest_month_abbr <- paste(month(max(df$date_dash), label = TRUE), year(max(df$date_dash))) 
  
  ### OVERVIEW TEXT ###
  output$latest_day_str <- renderUI({
    HTML(paste0("<b>Daily Digital Data Drop dashboard</b>: calendar year-to-date (January 1, 2019 - ", format(max(df$date_dash), "%B %d, %Y"),")"))
    })
  
  output$log_views <- renderUI({
    if (!length(input$view_opts) == 1) {
      HTML("<span style='color: gray; font-size: 12px; font-family: Monospace;'><b>Note:</b> This plot uses a <a href='https://en.wikipedia.org/wiki/Logarithmic_scale'>log scale</a>. Each axis tick corresponds to a <i>ten-fold</i> increase in page views.</span>")
    }
  })
  
  output$log_users <- renderUI({
    if (!length(input$user_opts) == 1) {
      HTML("<span style='color: gray; font-size: 12px; font-family: Monospace;'><b>Note:</b> This plot uses a <a href='https://en.wikipedia.org/wiki/Logarithmic_scale'>log scale</a>. Each axis tick corresponds to a <i>ten-fold</i> increase in users.</span>")
    }
  })
  
  
  ### Vars ###
  circ_var <- reactive({
      switch(input$circ_vars, 
             "sierra_trans" = if (!is.null(input$sierra_opts)) {prep_data(df, "transaction_type", trans_name= "sierra") %>% filter(transaction_type %in% input$sierra_opts)}, 
             "overdrive_trans" = if (!is.null(input$odrive_opts)) {prep_data(df, "transaction_type", trans_name= "overdrive") %>% filter(transaction_type %in% input$odrive_opts)}, 
             "cloud_trans" = if (!is.null(input$cloud_opts)) {prep_data(df, "transaction_type", trans_name= "cloudlibrary") %>% filter(transaction_type %in% input$cloud_opts)})
  })
  
  user_var <- reactive({
    if (!is.null(input$user_opts)) {
      prep_data(df, "user_type", users = TRUE) %>% 
        filter(user_type %in% input$user_opts)
    }
  })

  views_var <- reactive({
    if (!is.null(input$view_opts)) {
      prep_data(df, key = "user_type", views = TRUE) %>% 
        filter(user_type %in% input$view_opts)
    }
  })
  
  web_var <- reactive({
    if (!is.null(input$web_opts)) {
    prep_data(df, key = "user_type", web = TRUE) %>% 
      filter(user_type %in% input$web_opts)
    }
  })
  
  ### DATA DL ###
  out_ov <- df %>% rename(date = date_dash, 
                          search_requests_encore_page_views = search_requestsencore__page_views,
                          search_requests_encore_users = search_requests_encore__users)
  out_var <- reactive({
    switch(input$sidebar_menu, 
           "overview" = out_ov, 
           "cards" = prep_data(df, card=TRUE) %>% rename(new_card_sign_ups = count), 
           "transactions" = circ_var(), 
           "web" = web_var(),
           "users" = user_var(),
           "views" = views_var())
  })
  
  out_df <- reactive({
    out_var() %>% 
      rename_all(recode, date_dash = "date", 
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
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("DailyDigitalData_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write_csv(out_df(), file)
    })
  # cards <- prep_bars(df, card = TRUE, time_var = "s_month")
  plot_var <- reactive({
    switch(input$sidebar_menu, 
           "cards" = prep_data(df, card=TRUE), 
           "transactions" = circ_var(), 
           "web" = web_var(),
           "users" = user_var(),
           "views" = views_var())
  })
  m <- reactive({
    prep_bars(plot_var(), users = TRUE, group_var = "user_type", time_var = "s_month")
    # m <- prep_bars(plot_var(), card = TRUE, time_var = "s_month")
    print(head(m))
    return(m)
    })
  
  ### VALUE BOXES ###
  output$circ_home_vb <- renderValueBox({
    text <- actionLink("link_to_circ", HTML("<span style='font-size:20px; color:#dbdbdb;'>Circulation activity*</span>"))
    valueBox(
      comma_format()(sum(df %>% select(matches("sierra|overdrive|cloudlibrary"), -ends_with("users")))), text, icon = icon("exchange"),
      color = "olive"
    )
  })
  
  observeEvent(input$link_to_circ, {
    updateTabItems(session, "sidebar_menu", "transactions")
  })
  
  output$card_tot <- output$card_tot_tab <- renderValueBox({
    if (input$sidebar_menu == "overview") {
      text <- actionLink("link_to_cards", HTML("<span style='font-size:32px; color:#dbdbdb;'>New card sign ups</span>"))
    }
    else {
      text <- HTML(paste("New card sign ups",br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
    }

    valueBox(
      comma_format()(sum(df %>% select(new_card_sign_ups))), text, icon = icon("id-card"),
      color = "blue"
    )
  })
  
  observeEvent(input$link_to_cards, {
    updateTabItems(session, "sidebar_menu", "cards")
  })
  
  output$Vweb_box <- output$Vweb_box_tab <- renderValueBox({
    if (input$sidebar_menu == "overview") {
      text <- actionLink("link_to_Vweb", HTML("<span style='font-size:20px; color:#dbdbdb;'>NYPL.org page views</span>"))
    }
    else if (any(grepl('view', input$web_opts))) {
      text <- HTML(paste("NYPL.org page views",br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
    }
    
    valueBox(
      comma_format()(sum(df %>% select(intersect(ends_with("views"), contains("website"))))), text, icon = icon("search"),
      color = "black"
    )
  })
  
  observeEvent(input$link_to_Vweb, {
    updateTabItems(session, "sidebar_menu", "web")
  })
  
  output$Uweb_box <- output$Uweb_box_tab <- renderValueBox({
    if (input$sidebar_menu == "overview") {
      text <- actionLink("link_to_Uweb", HTML("<span style='font-size:20px; color:#dbdbdb;'>NYPL.org users</span>"))
    }
    else if (any(grepl('user', input$web_opts))) {
      text <- HTML(paste("NYPL.org users",br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
    }
    
    valueBox(
      comma_format()(sum(df %>% select(intersect(ends_with("users"), contains("website"))))), text, icon = icon("user-circle"),
      color = "black"
    )
  })
  
  observeEvent(input$link_to_Uweb, {
    updateTabItems(session, "sidebar_menu", "web")
  })

  output$views_tot <- output$views_tot_tab <- renderValueBox({
    if (input$sidebar_menu == "overview") {
      text <- actionLink("link_to_views", HTML("<span style='font-size:20px; color:#dbdbdb;'>Catalog page views</span>"))
    }
    else{
      text <- HTML(paste("Total catalog page views",br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))}
    valueBox(
      comma_format()(sum(prep_data(df, key = "user_type", views = TRUE)$count)), text, icon = icon("eye"),
      color = "light-blue"
    )
  })
  
  observeEvent(input$link_to_views, {
    updateTabItems(session, "sidebar_menu", "views")
  })
  
  output$users_tot <- output$users_tot_tab <- renderValueBox({
    if (input$sidebar_menu == "overview") {
      text <- actionLink("link_to_users", HTML("<span style='font-size:20px; color:#dbdbdb;'>Catalog users</span>"))
    }
    else {
      text <- HTML(paste("Total catalog users",br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
      }
    valueBox(
      comma_format()(sum(prep_data(df, "user_type", users = TRUE)$count)), text, icon = icon("users"),
      color = "light-blue"
    )
  })
  
  observeEvent(input$link_to_users, {
    updateTabItems(session, "sidebar_menu", "users")
  })
  
  output$v1 <- renderValueBox({
    if (any(grepl('shared', input$view_opts))) {
      v_type <- 'Shared catalog'
      text <- HTML(paste(var_to_label(v_type), "page views", br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
      valueBox(
        comma_format()(sum(views_var() %>% filter(tool_label(var_to_label(user_type)) == v_type) %$% count)), text, icon = icon("handshake"),
        color = "navy"
      )
    }
  })
  
  output$v2 <- renderValueBox({
    if (any(grepl('classic', input$view_opts))) {
    v_type <- 'Classic catalog'
      text <- HTML(paste(var_to_label(v_type), "page views", br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
      valueBox(
        comma_format()(sum(views_var() %>% filter(tool_label(var_to_label(user_type)) == v_type) %$% count)), text, icon = icon("glasses"),
        color = "navy"
      )
    }
  })

  output$v3 <- renderValueBox({
    if (any(grepl('encore', input$view_opts))) {
      v_type <- 'Encore'
      text <- HTML(paste(var_to_label(v_type), "page views", br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
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

  output$tot <- renderValueBox({
    v <- switch(input$circ_vars, "sierra_trans" = 'Sierra', "overdrive_trans" = 'Overdrive', "cloud_trans" = 'CloudLibrary')
    color <- switch(input$circ_vars, "sierra_trans" = 'red', "overdrive_trans" = 'yellow', "cloud_trans" = 'aqua')
    text <- HTML(paste("Total", v, "circulation activity",br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
    circ_var_total <- switch(input$circ_vars, "sierra_trans" = prep_data(df, "transaction_type", trans_name= "sierra"), 
                             "overdrive_trans" = prep_data(df, "transaction_type", trans_name= "overdrive"), 
                             "cloud_trans" = prep_data(df, "transaction_type", trans_name= "cloudlibrary"))
    valueBox(
      comma_format()(sum(circ_var_total$count)), text, icon = icon("exchange"),
      color = color
    )
  })

  output$c1 <- output$s1 <- output$o1 <- renderValueBox({
    v_type <- switch(input$circ_vars, "sierra_trans" = 'sierra_checkins', "overdrive_trans" = 'overdrive_audiobook_checkouts', "cloud_trans" = 'cloudlibrary_audiobook_checkouts')
    if (any(grepl(v_type, circ_var()$transaction_type))) {
      text <- HTML(paste(var_to_label(v_type), br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
      icon <- switch(input$circ_vars, "sierra_trans" = icon("book"), "overdrive_trans" = icon("headphones"), "cloud_trans" = icon("headphones"))
      valueBox(
        comma_format()(sum(circ_var() %>% filter(transaction_type == v_type) %$% count)), text, icon = icon,
        color = "navy"
      )}
  })

  output$c2 <- output$s2 <- output$o2 <- renderValueBox({
    v_type <- switch(input$circ_vars, "sierra_trans" = 'sierra_checkouts', "overdrive_trans" = 'overdrive_ebook_checkouts', "cloud_trans" = 'cloudlibrary_audiobook_holds')
    if (any(grepl(v_type, circ_var()$transaction_type))) {
      text <- HTML(paste(var_to_label(v_type), br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
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
        text <- HTML(paste(var_to_label(v_type), br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
        valueBox(
          comma_format()(sum(circ_var() %>% filter(transaction_type == v_type) %$% count)), text, icon = icon,
          color = "navy"
        )}}
  })

  output$c4 <- renderValueBox({
    v_type <- switch(input$circ_vars, "sierra_trans" = '', "overdrive_trans" = '', "cloud_trans" = 'cloudlibrary_ebook_holds')
    if (any(grepl(v_type, circ_var()$transaction_type))) {
      text <- HTML(paste(var_to_label(v_type), br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
      if (!input$circ_vars %in% c("sierra_trans","overdrive_trans")){
        valueBox(
          comma_format()(sum(circ_var() %>% filter(transaction_type == v_type) %$% count)), text, icon = icon("bookmark"),
          color = "navy"
        )}}
  })

  ### PLOTS ###
  output$trans_plot_sum <- renderPlotly({
    trans <- prep_data(df, key = "transaction_type", trans_sum = TRUE) %>%
      mutate(transaction_group = tool_label(transaction_type),
             hex = cat_color(transaction_group)) %>%
      group_by(transaction_group, hex) %>%
      summarise(tot = sum(count))

    p <- plot_ly(trans, labels = ~transaction_group, values = ~tot
                 ,marker = list(colors=trans$hex)
                 ,textposition="outside"
                 ,hoverinfo = 'text'
                 ,hovertext =  paste('<b>',trans$transaction_group,'</b> circulation:<br>',comma_format()(trans$tot))
                 ,text = trans$transaction_group
                 ,textinfo = "text"
                 ,rotation = -100
    ) %>%
      add_pie(hole = 0.6) %>%
      layout(showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
             ,dragmode =  "zoom"
             ,margin = list(b=20,l=20,r=20,t=20,pad=4))
    ggplotly(p) %>%
      config(displayModeBar = F)

  })

  output$views_plot <- renderChart({
    if (input$Vtime_var == "Monthly"){
      monthly <- views_var() %>%
        group_by(user_type, s_month) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(user_lab = tool_label(var_to_label(user_type)),
               hex = cat_color(user_type),
               log_count = ifelse(!count == 0, log(count, 10), 0)) %>%
        arrange(s_month, count)

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
      quarterly <- views_var() %>%
        group_by(user_type, f_quarter) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(user_lab = tool_label(var_to_label(user_type)),
               hex = cat_color(user_type),
               log_count = ifelse(!count == 0, log(count, 10), NA)) %>%
        filter(!is.na(log_count)) %>%
        arrange(f_quarter, count)

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
      wk_daily <- views_var() %>%
        group_by(user_type, s_weekday) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(user_lab = tool_label(var_to_label(user_type)),
               hex = cat_color(user_type),
               log_count = ifelse(!count == 0, log(count, 10), NA)) %>%
        filter(!is.na(log_count)) %>%
        arrange(s_weekday, count)
      
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
      monthly <- user_var() %>%
        group_by(user_type, s_month) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(user_lab = tool_label(var_to_label(user_type)),
               hex = cat_color(user_type),
               log_count = ifelse(!count == 0, log(count, 10), 0)) %>%
        arrange(s_month, count)

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
      quarterly <- user_var() %>%
        group_by(user_type, f_quarter) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(user_lab = tool_label(var_to_label(user_type)),
               hex = cat_color(user_type),
               log_count = ifelse(!count == 0, log(count, 10), NA)) %>%
        filter(!is.na(log_count)) %>%
        arrange(f_quarter, count)

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
      wk_daily <- user_var() %>%
        group_by(user_type, s_weekday) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(user_lab = tool_label(var_to_label(user_type)),
               hex = cat_color(user_type),
               log_count = ifelse(!count == 0, log(count, 10), NA)) %>%
        filter(!is.na(log_count)) %>%
        arrange(s_weekday, count)
      
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
      monthly <- prep_data(df, card=TRUE) %>%
        group_by(s_month) %>%
        summarise(count = sum(count))
      n_base <- nPlot(count ~ s_month, data = monthly, type = "multiBarChart", width = session$clientData[["output_Cplot_for_size_width"]])
      tt <- "#! function(key, x, y, e){ return '<p><b>New card sign ups</b></p><p>' + d3.format(',.0')(e.value) + ' in ' + x + ' 2019 </p>'} !#"
      n <- format_nPlot(n_base, list(left = 70), "#!d3.format(',.0')!#",plotID = "card_plot", tooltip = tt)
      n$chart(color = "#! function(d){ return '#4272B8'} !#", showLegend = FALSE, showControls = FALSE)
      return(n)
    }
    else if (input$Ctime_var == "Daily") {
      daily <- prep_data(df, card=TRUE)

      n_base <- nPlot(count ~ s_date, data = daily, type = "lineChart", width = session$clientData[["output_Cplot_for_size_width"]])
      xFormat <- "#!function(d) {return d3.time.format.utc('%Y-%m-%d')(new Date(d));} !#"
      tt <- "#! function(key, x, y){ return '<p><b>New card sign ups</b></p><p>' + y + ' on ' + x + '</p>'} !#"
      n <- format_nPlot(n_base, list(left = 70, right = 100), "#!d3.format(',.0')!#", xFormat, plotID = "card_plot", tt)
      n$chart(color = "#! function(d){ return '#4272B8'} !#", showLegend = FALSE)
      return(n)
    }
    else if (input$Ctime_var == "Quarterly") {
      quarterly <- prep_data(df, card=TRUE) %>%
        group_by(f_quarter) %>%
        summarise(count = sum(count))

      n_base <- nPlot(count ~ f_quarter, data = quarterly, type = "multiBarChart", width = session$clientData[["output_Cplot_for_size_width"]])
      tt <- "#! function(key, x, y, e){ return '<p><b>New card sign ups</b></p><p>' + d3.format(',.0')(e.value) + ' in ' + x + '</p>'} !#"
      n <- format_nPlot(n_base, list(left = 70), "#!d3.format(',.0')!#", plotID = "card_plot", tooltip = tt)
      n$chart(color = "#! function(d){ return '#4272B8'} !#", showLegend = FALSE, showControls = FALSE)
      return(n)
    }
    else if (input$Ctime_var == "Weekday") {
      wk_daily <- prep_data(df, card=TRUE) %>%
        group_by(s_weekday) %>%
        summarise(count = sum(count))
      
      n_base <- nPlot(count ~ s_weekday, data = wk_daily, type = "multiBarChart", width = session$clientData[["output_Cplot_for_size_width"]])
      tt <- "#! function(key, x, y, e){ return '<p><b>New card sign ups</b></p><p>' + d3.format(',.0')(e.value) + ' on ' + x + 's</p>'} !#"
      n <- format_nPlot(n_base, list(left = 70), "#!d3.format(',.0')!#", plotID = "card_plot", tooltip = tt)
      n$chart(color = "#! function(d){ return '#4272B8'} !#", showLegend = FALSE, showControls = FALSE)
      return(n)
    }

  })
  
  output$web_plot <- renderChart({
    if (input$Wtime_var == "Monthly"){
      monthly <- web_var() %>%
        group_by(user_type, s_month) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(user_lab = ifelse(grepl("views",user_type), "Page views", "Users"),
               hex = trans_shade(user_type)) %>%
        arrange(s_month, count)

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
      quarterly <- web_var() %>%
        group_by(user_type, f_quarter) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(user_lab = ifelse(grepl("views",user_type), "Page views", "Users"),
               hex = trans_shade(user_type)) %>%
        arrange(f_quarter, count)

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
      wk_daily <- web_var() %>%
        group_by(user_type, s_weekday) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(user_lab = ifelse(grepl("views",user_type), "Page views", "Users"),
               hex = trans_shade(user_type)) %>%
        arrange(s_weekday, count)


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
      monthly <- circ_var() %>%
        group_by(transaction_type, s_month) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(transaction_type = var_to_label(transaction_type),
               hex = trans_shade(transaction_type))

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
      n <- format_nPlot(n_base, list(left = 100), "#!d3.format(',.0')!#", plotID = "trans_plot", tooltip = tt)
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
      quarterly <- circ_var() %>%
        group_by(transaction_type, f_quarter) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(transaction_type = var_to_label(transaction_type),
               hex = trans_shade(transaction_type))

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
      wk_daily <- circ_var() %>%
        group_by(transaction_type, s_weekday) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(transaction_type = var_to_label(transaction_type),
               hex = trans_shade(transaction_type)) %>%
        arrange(s_weekday, count)

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

}
