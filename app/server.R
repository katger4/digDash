server <- function(input, output, session) {

  # df <- df_ss %>%
  #   gs_read_csv(ws = 1) %>%
  #   drop_na() %>%
  #   filter_at(vars(-starts_with("date"), -starts_with("week")), any_vars(. != 0))
  
  # saveRDS(df, './app/jul.rds')
  df <- readRDS('./data/jun31.rds')
  
  latest_month_abbr <- paste(month(max(df$date_dash), label = TRUE), year(max(df$date_dash))) 
  
  ### OVERVIEW TEXT ###
  output$latest_day_str <- renderUI({
    HTML(paste("<b>Daily Digital Data Drop dashboard</b>: January 1, 2019 -", format(max(df$date_dash), "%B %d, %Y")))
    })
  
  # output$web_tot <- renderUI({
  #   HTML(paste("<b>",comma_format()(sum(df %>% select(contains("website"), -contains("catalog")))),"</b>page views"))
  # })
  
  output$c_views_tot <- renderUI({
    actionLink("link_to_views", HTML(paste("<span style='font-size:24px'><b>",comma_format()(sum(df %>% select(intersect(contains("views"), matches("catalog|encore"))))),"</b>page views</span>")))
  })
  
  observeEvent(input$link_to_views, {
    updateTabItems(session, "sidebar_menu", "views")
  })
  
  output$circ_tot <- renderUI({
    actionLink("link_to_circ", HTML(paste("<span style='font-size:26px'><b>",comma_format()(sum(df %>% select(matches("sierra|overdrive|cloudlibrary"), -ends_with("users")))),"</b>circulation activity*</span>")))
  })
  
  observeEvent(input$link_to_circ, {
    updateTabItems(session, "sidebar_menu", "transactions")
  })
  
  output$nc_user_tot <- renderUI({
    actionLink("link_to_users", HTML(paste("<span style='font-size:24px'><b>",comma_format()(sum(df %>% select(ends_with("users"), -contains("website")))),"</b>unique users</span>")))
  })
  
  observeEvent(input$link_to_users, {
    updateTabItems(session, "sidebar_menu", "users")
  })
  
  # output$c_user_tot <- renderUI({
  #   HTML(paste("<b>",comma_format()(sum(df %>% select(intersect(ends_with("users"), matches("catalog|encore"))))),"</b>unique catalog users"))
  # })
  
  ### Vars ###
  var <- reactive({
    switch(input$vars, "sierra_trans" = prep_data(df, "transaction_type", trans_name= "sierra"), "overdrive_trans" = prep_data(df, "transaction_type", trans_name= "overdrive"), "cloud_trans" = prep_data(df, "transaction_type", trans_name= "cloudlibrary"))
  })
  
  user_var <- prep_data(df, "user_type", cat = "catalog", users = TRUE)

  # user_var <- reactive({
  #   switch(input$users_vars, "nc_users" = prep_data(df, "user_type", cat = "not catalog", users = TRUE), "c_users" = prep_data(df, "user_type", cat = "catalog", users = TRUE))
  # })

  views_var <- prep_data(df, "user_type", cat = "catalog", views = TRUE)
  # reactive({
  #   # switch(input$views_vars, "nc_users" = prep_data(df, "user_type", cat = "not catalog", views = TRUE), "c_users" = prep_data(df, "user_type", cat = "catalog", views = TRUE))
  # })
  
  web_var <- prep_data(df, "user_type", web = TRUE)
  
  ### VALUE BOXES ###
  
  output$card_tot <- output$card_tot_tab <- renderValueBox({
    if (input$sidebar_menu == "overview") {
      text <- actionLink("link_to_cards", HTML("<span style='font-size:30px; color:#dbdbdb;'>New card sign ups</span>"))
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
      text <- actionLink("link_to_Vweb", HTML("<span style='font-size:22px; color:#dbdbdb;'>Website page views</span>"))
    }
    else {
      text <- HTML(paste("Website page views",br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
    }
    
    valueBox(
      comma_format()(sum(df %>% select(intersect(ends_with("views"), contains("website"))))), text, icon = icon("eye"),
      color = "black"
    )
  })
  
  observeEvent(input$link_to_Vweb, {
    updateTabItems(session, "sidebar_menu", "web")
  })
  
  output$Uweb_box <- output$Uweb_box_tab <- renderValueBox({
    if (input$sidebar_menu == "overview") {
      text <- actionLink("link_to_Uweb", HTML("<span style='font-size:22px; color:#dbdbdb;'>Website users</span>"))
    }
    else {
      text <- HTML(paste("Website users",br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
    }
    
    valueBox(
      comma_format()(sum(df %>% select(intersect(ends_with("users"), contains("website"))))), text, icon = icon("users"),
      color = "black"
    )
  })
  
  observeEvent(input$link_to_Uweb, {
    updateTabItems(session, "sidebar_menu", "web")
  })

  output$views_tot <- renderValueBox({
    text <- HTML(paste("Page views",br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
    valueBox(
      comma_format()(sum(views_var$count)), text, icon = icon("eye"),
      color = "blue"
    )
  })
  
  output$v1 <- renderValueBox({
    v_type <- 'Classic catalog'
      text <- HTML(paste(var_to_label(v_type), "page views", br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
      valueBox(
        comma_format()(sum(views_var %>% filter(tool_label(var_to_label(user_type)) == v_type) %$% count)), text, icon = icon("glasses"),
        color = "blue"
      )
  })
  
  output$v2 <- renderValueBox({
    v_type <- 'Shared catalog'
    text <- HTML(paste(var_to_label(v_type), "page views", br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
    valueBox(
      comma_format()(sum(views_var %>% filter(tool_label(var_to_label(user_type)) == v_type) %$% count)), text, icon = icon("handshake"),
      color = "blue"
    )
  })

  output$v3 <- renderValueBox({
    v_type <- 'Encore'
      text <- HTML(paste(var_to_label(v_type), "page views", br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
      valueBox(
        comma_format()(sum(views_var %>% filter(tool_label(var_to_label(user_type)) == v_type) %$% count)), text, icon = icon("retweet"),
        color = "blue"
      )
  })

  output$users_tot <- renderValueBox({
    text <- HTML(paste("Unique users",br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
    valueBox(
      comma_format()(sum(user_var$count)), text, icon = icon("users"),
      color = "blue"
    )
  })

  output$u1 <- renderValueBox({
    v_type <- 'Shared catalog'
      text <- HTML(paste(var_to_label(v_type), "users", br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
      valueBox(
        comma_format()(sum(user_var %>% filter(tool_label(var_to_label(user_type)) == v_type) %$% count)), text, icon = icon("handshake"),
        color = "blue"
      )
  })

  output$u2 <- renderValueBox({
    v_type <- 'Classic catalog'
      text <- HTML(paste(var_to_label(v_type), "users", br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
      valueBox(
        comma_format()(sum(user_var %>% filter(tool_label(var_to_label(user_type)) == v_type) %$% count)), text, icon = icon("glasses"),
        color = "blue"
      )
  })

  output$u3 <- renderValueBox({
    v_type <- 'CloudLibrary'

      text <- HTML(paste(var_to_label(v_type), "users", br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
      valueBox(
        comma_format()(sum(user_var %>% filter(tool_label(var_to_label(user_type)) == v_type) %$% count)), text, icon = icon("cloud"),
        color = "blue"
      )
  })

  output$u4 <- renderValueBox({
    v_type <- 'Overdrive'
      text <- HTML(paste(var_to_label(v_type), "users", br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
      valueBox(
        comma_format()(sum(user_var %>% filter(tool_label(var_to_label(user_type)) == v_type) %$% count)), text, icon = icon("headphones"),
        color = "blue"
      )
  })
  
  output$u5 <- renderValueBox({
    v_type <- 'Encore'
    text <- HTML(paste(var_to_label(v_type), "users", br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
    valueBox(
      comma_format()(sum(user_var %>% filter(tool_label(var_to_label(user_type)) == v_type) %$% count)), text, icon = icon("retweet"),
      color = "blue"
    )
  })

  output$tot <- renderValueBox({
    v <- switch(input$vars, "sierra_trans" = 'Sierra', "overdrive_trans" = 'Overdrive', "cloud_trans" = 'CloudLibrary')
    text <- HTML(paste(v, "circulation activity",br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
    valueBox(
      comma_format()(sum(var()$count)), text, icon = icon("exchange"),
      color = "blue"
    )
  })

  output$t1 <- renderValueBox({
    v_type <- switch(input$vars, "sierra_trans" = 'sierra_checkins', "overdrive_trans" = 'overdrive_ebook_checkouts', "cloud_trans" = 'cloudlibrary_ebook_checkouts')
    text <- HTML(paste(var_to_label(v_type), br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
    valueBox(
      comma_format()(sum(var() %>% filter(transaction_type == v_type) %$% count)), text, icon = icon("book"),
      color = "blue"
    )
  })

  output$t2 <- renderValueBox({
    v_type <- switch(input$vars, "sierra_trans" = 'sierra_checkouts', "overdrive_trans" = 'overdrive_ebook_holds', "cloud_trans" = 'cloudlibrary_ebook_holds')
    text <- switch(input$vars, "sierra_trans" = HTML(paste(var_to_label(v_type), br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>")),
                   "overdrive_trans" = HTML(paste0(var_to_label(v_type),"*", br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>")),
                   "cloud_trans" = HTML(paste(var_to_label(v_type), br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>")))
    valueBox(
      comma_format()(sum(var() %>% filter(transaction_type == v_type) %$% count)), text, icon = icon("book-open"),
      color = "blue")
  })

  output$t3 <- renderValueBox({
    v_type <- switch(input$vars, "sierra_trans" = 'sierra_renewals', "overdrive_trans" = 'overdrive_audiobook_checkouts', "cloud_trans" = 'cloudlibrary_audiobook_checkouts')
    text <- HTML(paste(var_to_label(v_type), br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>"))
    valueBox(
      comma_format()(sum(var() %>% filter(transaction_type == v_type) %$% count)), text, icon = icon("bookmark"),
      color = "blue"
    )
  })

  output$t4 <- renderValueBox({
    v_type <- switch(input$vars, "sierra_trans" = '', "overdrive_trans" = 'overdrive_audiobook_holds', "cloud_trans" = 'cloudlibrary_audiobook_holds')
    text <- switch(input$vars,
                   "overdrive_trans" = HTML(paste0(var_to_label(v_type),"*", br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>")),
                   "cloud_trans" = HTML(paste(var_to_label(v_type), br(),"<span style='font-size:12px'>Jan 2019 - ",latest_month_abbr,"</span>")))
    if (!input$vars == "sierra_trans"){
      valueBox(
      comma_format()(sum(var() %>% filter(transaction_type == v_type) %$% count)), text, icon = icon("book-reader"),
      color = "blue"
    )}
  })

  ### PLOTS ###

  output$cat_views_plot_sum <- renderPlotly({
    view_sum <- prep_data(df, key = "user_type", cat = "catalog", views = TRUE) %>%
      group_by(user_type, s_year) %>%
      summarise(tot = sum(count)) %>%
      ungroup() %>%
      mutate(user_type = var_to_label(user_type),
             user_lab = tool_label(user_type),
             hex = cat_color(user_lab)) 
    
    view_sum$user_lab <- factor(view_sum$user_lab, levels = unique(view_sum$user_lab)[order(view_sum$tot, decreasing = TRUE)])

    p <- plot_ly(view_sum, x= ~tot, y = ~user_lab
                 ,type = "bar"
                 ,orientation = 'h'
                 ,marker = list(color=view_sum$hex)
                 # ,text = comma_format()(view_sum$tot)
                 # ,textposition="outside"
                 ,cliponaxis = FALSE
                 ,hoverinfo = 'text'
                 ,hovertext = overview_tooltip(view_sum$user_lab,view_sum$tot,'page views')
    ) %>%
      layout(showlegend = F
             ,bargap = 0
             ,xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
             ,yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE)
             ,dragmode =  "zoom"
             ,margin = list(b=10,l=10,r=10,t=10,pad=4)
      )
    ggplotly(p) %>%
      config(displayModeBar = F)
    
    # p <- plot_ly(view_sum, labels = ~user_lab, values = ~tot
    #              ,marker = list(colors=view_sum$hex)
    #              ,textposition="outside"
    #              ,hoverinfo = 'text'
    #              ,hovertext = overview_tooltip(view_sum$user_lab,view_sum$tot,'users')
    #              ,text = view_sum$user_lab
    #              ,textinfo = "text"
    #              ,rotation = -100
    #              # ,text = comma_format()(user_sum$tot)
    # ) %>%
    #   add_pie(hole = 0.6) %>%
    #   layout(showlegend = F,
    #          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    #          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
    #          ,margin = list(b=20,l=10,r=50,t=20,pad=4))
    # ggplotly(p) %>%
    #   config(displayModeBar = F)
  })
  
  

  output$trans_plot_sum <- renderPlotly({
    trans <- prep_data(df, key = "transaction_type", trans_sum = TRUE) %>%
      mutate(grouper = tool_label(transaction_type)) %>%
      group_by(grouper) %>%
      summarise(tot = sum(count))

    packing <- circleProgressiveLayout(trans$tot, sizetype='area')
    cap_bub <- bind_cols(trans, packing) %>% rowid_to_column("id")
    dat.gg <- circleLayoutVertices(packing, npoints = 100)
    p <- ggplot(data = dat.gg) +
      # Make the bubbles
      geom_polygon(aes(x, y, group = id, fill=as.factor(id)), alpha = 1, show.legend = FALSE) +
      scale_fill_manual(values = cat_color(cap_bub$grouper)) +
      geom_text(data = cap_bub, aes(x, y, size = tot, label = grouper)) +
      theme_void() +
      theme(legend.position="none") +
      coord_equal()

    gp <- ggplotly(p) %>%
      config(displayModeBar = F) %>%
      layout(xaxis = list(showgrid = F),
             yaxis = list(showgrid = F)
             ,dragmode =  "zoom")

    # specify hoverinfo manually on circle
    for(i in 1:3){
      name <- cap_bub %>% filter(id == gp$x$data[[i]]$name) %$% grouper
      num <- cap_bub %>% filter(id == gp$x$data[[i]]$name) %$% tot
      gp$x$data[[i]]$text = paste('<b>',name,'</b> circulation:','\n',comma(num))
    }
    
    # specify hoverinfo manually on text
    gp$x$data[[4]]$hoverlabel = list(bgcolor = cat_color(cap_bub$grouper))
    gp$x$data[[4]]$hovertext = paste('<b>',cap_bub$grouper,'</b> circulation:','\n',comma(cap_bub$tot)) #overview_tooltip(cap_bub$grouper,cap_bub$tot,'circulation')

    return(gp)

  })

  output$user_plot_sum <- renderPlotly({
    user_sum <- prep_data(df, "user_type", cat = "not catalog", users = TRUE) %>%
      group_by(user_type, s_year) %>%
      summarise(tot = sum(count)) %>%
      ungroup() %>%
      mutate(user_type = var_to_label(user_type),
             user_lab = tool_label(user_type),
             hex = cat_color(user_type))
    p <- plot_ly(user_sum, labels = ~user_lab, values = ~tot
                 ,marker = list(colors=user_sum$hex)
                 ,textposition="outside"
                 ,hoverinfo = 'text'
                 ,hovertext = overview_tooltip(user_sum$user_lab,user_sum$tot,'users')
                 ,text = user_sum$user_lab
                 ,textinfo = "text"
                 ,rotation = -100
                 # ,text = comma_format()(user_sum$tot)
                   ) %>%
      add_pie(hole = 0.6) %>%
      layout(showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
             ,dragmode =  "zoom"
             ,margin = list(b=20,l=10,r=50,t=20,pad=4))
    ggplotly(p) %>%
      config(displayModeBar = F)
  })

  output$views_plot <- renderChart({
    if (input$Vtime_var == "Monthly"){
      monthly <- views_var %>%
        group_by(user_type, s_month) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(user_lab = tool_label(var_to_label(user_type)),
               hex = cat_color(user_type),
               log_count = ifelse(!count == 0, log(count, 10), 0)) %>%
        arrange(s_month, count)

      n_base <- nPlot(log_count ~ s_month, group = "user_lab", data = monthly, type = "multiBarChart", width = session$clientData[["output_Vplot_for_size_width"]])
      yTicks <- "#!function (d) { return d3.format(',.0')(Math.round(100*Math.pow(10,d))/100);}!#"
      tt <- "#! function(key, x, y, e){ return '<p><strong>' + key + '</strong></p><p>' + function(d) { if (d !== 0) {return d3.format(',.0')(Math.round(100*Math.pow(10,d))/100)} else {return 0}; }(e.value) + ' page views in ' + x + ' 2019 </p>'} !#"
      n <- format_nPlot(n_base, list(left = 70), yTicks, plotID = "views_plot", tooltip = tt)
      n$chart(color = unique(monthly$hex), showControls = FALSE)
      # n$yAxis(tickValues = c(1,2,3,4,5,6,7,8))
      return(n)

    }
    else if (input$Vtime_var == "Daily") {
      daily <- views_var %>%
        mutate(user_lab = tool_label(var_to_label(user_type)),
               hex = cat_color(user_type),
               log_count = ifelse(!count == 0, log(count, 10), NA)) %>%
        filter(!is.na(log_count))

      n_base <- nPlot(log_count ~ s_date, group = "user_lab", data = daily, type = "lineChart", width = session$clientData[["output_Vplot_for_size_width"]])
      yTicks <- "#!function (d) { return d3.format(',.0')(Math.round(100*Math.pow(10,d))/100);}!#"
      xFormat <- "#!function(d) {return d3.time.format.utc('%Y-%m-%d')(new Date(d));} !#"
      tt <- "#! function(key, x, y){ return '<p><strong>' + key + '</strong></p><p>' + y + ' page views on ' + x + '</p>'} !#"
      n <- format_nPlot(n_base, list(left = 100, right = 100), yTicks, xFormat,"views_plot", tt)
      n$chart(color = unique(daily$hex))
      n$yAxis(tickValues = c(1,2,3,4,5,6))
      return(n)
    }
    else if (input$Vtime_var == "Quarterly") {
      quarterly <- views_var %>%
        group_by(user_type, f_quarter) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(user_lab = tool_label(var_to_label(user_type)),
               hex = cat_color(user_type),
               log_count = ifelse(!count == 0, log(count, 10), NA)) %>%
        filter(!is.na(log_count)) %>%
        arrange(f_quarter, count)

      n_base <- nPlot(log_count ~ f_quarter, group = "user_lab", data = quarterly, type = "multiBarChart", width = session$clientData[["output_Vplot_for_size_width"]])
      yTicks <- "#!function (d) { return d3.format(',.0')(Math.round(100*Math.pow(10,d))/100);}!#"
      tt <- "#! function(key, x, y, e){ return '<p><strong>' + key + '</strong></p><p>' + function(d) { return d3.format(',.0')(Math.round(100*Math.pow(10,d))/100); }(e.value) + ' page views in ' + x + '</p>'} !#"
      n <- format_nPlot(n_base, list(left = 80), yTicks, plotID = "views_plot", tooltip = tt)
      n$chart(color = unique(quarterly$hex), showControls = FALSE)
      # n$yAxis(tickValues = c(1,2,3,4,5,6,7,8,9))
      return(n)
    }

  })

  output$users_plot <- renderChart({
    if (input$Utime_var == "Monthly"){
      monthly <- user_var %>%
        group_by(user_type, s_month) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(user_lab = tool_label(var_to_label(user_type)),
               hex = cat_color(user_type),
               log_count = ifelse(!count == 0, log(count, 10), 0)) %>%
        arrange(s_month, count)
      
      n_base <- nPlot(log_count ~ s_month, group = "user_lab", data = monthly, type = "multiBarChart", width = session$clientData[["output_Uplot_for_size_width"]])
      yTicks <- "#!function (d) { return d3.format(',.0')(Math.round(100*Math.pow(10,d))/100);}!#"
      tt <- "#! function(key, x, y, e){ return '<p><strong>' + key + '</strong></p><p>' + function(d) { if (d !== 0) {return d3.format(',.0')(Math.round(100*Math.pow(10,d))/100)} else {return 0}; }(e.value) + ' users in ' + x + ' 2019 </p>'} !#"
      n <- format_nPlot(n_base, list(left = 70), yTicks, plotID = "users_plot", tooltip = tt)
      n$chart(color = unique(monthly$hex), showControls = FALSE)
      n$yAxis(tickValues = c(1,2,3,4,5))
      return(n)

    }
    else if (input$Utime_var == "Daily") {
      daily <- user_var %>%
        mutate(user_lab = tool_label(var_to_label(user_type)),
               hex = cat_color(user_type),
               log_count = ifelse(!count == 0, log(count, 10), NA)) %>%
        filter(!is.na(log_count))

      n_base <- nPlot(log_count ~ s_date, group = "user_lab", data = daily, type = "lineChart", width = session$clientData[["output_Uplot_for_size_width"]])
      yTicks <- "#!function (d) { return d3.format(',.0')(Math.round(100*Math.pow(10,d))/100);}!#"
      xFormat <- "#!function(d) {return d3.time.format.utc('%Y-%m-%d')(new Date(d));} !#"
      tt <- "#! function(key, x, y){ return '<p><strong>' + key + '</strong></p><p>' + y + ' users on ' + x + '</p>'} !#"
      n <- format_nPlot(n_base, list(left = 70, right = 70), yTicks, xFormat,"users_plot", tt)
      n$chart(color = unique(daily$hex))
      n$yAxis(tickValues = c(1,2,3,4))
      return(n)
    }
    else if (input$Utime_var == "Quarterly") {
      quarterly <- user_var %>%
        group_by(user_type, f_quarter) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(user_lab = tool_label(var_to_label(user_type)),
               hex = cat_color(user_type),
               log_count = ifelse(!count == 0, log(count, 10), NA)) %>%
        filter(!is.na(log_count)) %>%
        arrange(f_quarter, count)
      
      n_base <- nPlot(log_count ~ f_quarter, group = "user_lab", data = quarterly, type = "multiBarChart", width = session$clientData[["output_Uplot_for_size_width"]])
      yTicks <- "#!function (d) { return d3.format(',.0')(Math.round(100*Math.pow(10,d))/100);}!#"
      tt <- "#! function(key, x, y, e){ return '<p><strong>' + key + '</strong></p><p>' + function(d) { return d3.format(',.0')(Math.round(100*Math.pow(10,d))/100); }(e.value) + ' users in ' + x + '</p>'} !#"
      n <- format_nPlot(n_base, list(left = 80), yTicks, plotID = "users_plot", tooltip = tt)
      n$chart(color = unique(quarterly$hex), showControls = FALSE)
      return(n)
    }

  })

  output$card_plot <- renderChart({

    if (input$Ctime_var == "Monthly"){
      monthly <- prep_data(df, card=TRUE) %>%
        group_by(s_month) %>%
        summarise(count = sum(count))
      n_base <- nPlot(count ~ s_month, data = monthly, type = "multiBarChart", width = session$clientData[["output_Cplot_for_size_width"]])
      tt <- "#! function(key, x, y, e){ return '<p><strong>New card sign ups</strong></p><p>' + d3.format(',.0')(e.value) + ' in ' + x + ' 2019 </p>'} !#"
      n <- format_nPlot(n_base, list(left = 70), "#!d3.format(',.0')!#",plotID = "card_plot", tooltip = tt)
      n$chart(showControls = FALSE)
      n$chart(showLegend = FALSE)
      n$chart(color = "#! function(d){ return '#4272B8'} !#")
      return(n)
    }
    else if (input$Ctime_var == "Daily") {
      daily <- prep_data(df, card=TRUE)

      n_base <- nPlot(count ~ s_date, data = daily, type = "lineChart", width = session$clientData[["output_Cplot_for_size_width"]])
      xFormat <- "#!function(d) {return d3.time.format.utc('%Y-%m-%d')(new Date(d));} !#"
      # n_base <- nPlot(count ~ date_dash, data = daily, type = "lineChart")
      # xFormat <- "#!function(d) {return d3.time.format.utc.utc('%Y-%m-%d')(new Date(d * 86400000));} !#"
      tt <- "#! function(key, x, y){ return '<p><strong>New card sign ups</strong></p><p>' + y + ' on ' + x + '</p>'} !#"
      n <- format_nPlot(n_base, list(left = 70, right = 100), "#!d3.format(',.0')!#", xFormat, plotID = "card_plot", tt)
      # n$chart(showControls = FALSE)
      n$chart(showLegend = FALSE)
      n$chart(color = "#! function(d){ return '#4272B8'} !#")
      return(n)
    }
    else if (input$Ctime_var == "Quarterly") {
      quarterly <- prep_data(df, card=TRUE) %>%
        group_by(f_quarter) %>%
        summarise(count = sum(count))

      n_base <- nPlot(count ~ f_quarter, data = quarterly, type = "multiBarChart", width = session$clientData[["output_Cplot_for_size_width"]])
      tt <- "#! function(key, x, y, e){ return '<p><strong>New card sign ups</strong></p><p>' + d3.format(',.0')(e.value) + ' in ' + x + '</p>'} !#"
      n <- format_nPlot(n_base, list(left = 70), "#!d3.format(',.0')!#", plotID = "card_plot", tooltip = tt)
      n$chart(showControls = FALSE)
      n$chart(showLegend = FALSE)
      n$chart(color = "#! function(d){ return '#4272B8'} !#")
      return(n)
    }

  })
  
  output$web_plot <- renderChart({
    
    if (input$Wtime_var == "Monthly"){
      monthly <- web_var %>%
        group_by(user_type, s_month) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(user_lab = ifelse(grepl("views",user_type), "Page views", "Users"),
               hex = trans_shade(user_type)) %>%
        arrange(s_month, count)
      n_base <- nPlot(count ~ s_month, group = "user_lab", data = monthly, type = "multiBarChart", width = session$clientData[["output_Wplot_for_size_width"]])
      tt <- "#! function(key, x, y, e){ return '<p><strong>' + key + '</strong></p><p>' + d3.format(',.0')(e.value) + ' in ' + x + ' 2019 </p>'} !#"
      n <- format_nPlot(n_base, list(left = 70), "#!d3.format(',.0')!#", plotID = "web_plot", tooltip = tt)
      n$chart(color = unique(monthly$hex))
      # n$setTemplate(script = sprintf())
      return(n)
    }
    else if (input$Wtime_var == "Daily") {
      daily <- web_var %>%
        mutate(user_lab = ifelse(grepl("views",user_type), "Page views", "Users"),
               hex = trans_shade(user_type))
      
      n_base <- nPlot(count ~ s_date, group = "user_lab", data = daily, type = "lineChart", width = session$clientData[["output_Wplot_for_size_width"]])
      xFormat <- "#!function(d) {return d3.time.format.utc('%Y-%m-%d')(new Date(d));} !#"
      tt <- "#! function(key, x, y){ return '<p><strong>' + key + '</strong></p><p>' + y + ' on ' + x + '</p>'} !#"
      n <- format_nPlot(n_base, list(left = 70, right = 100), "#!d3.format(',.0')!#", xFormat,"web_plot", tt)
      n$chart(color = unique(daily$hex))
      return(n)
    }
    else if (input$Wtime_var == "Quarterly") {
      quarterly <- web_var %>%
        group_by(user_type, f_quarter) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(user_lab = ifelse(grepl("views",user_type), "Page views", "Users"),
               hex = trans_shade(user_type)) %>%
        arrange(f_quarter, count)
      
      n_base <- nPlot(count ~ f_quarter, group = "user_lab", data = quarterly, type = "multiBarChart", width = session$clientData[["output_Wplot_for_size_width"]])
      tt <- "#! function(key, x, y, e){ return '<p><strong>' + key + '</strong></p><p>' + d3.format(',.0')(e.value) + ' in ' + x + '</p>'} !#"
      n <- format_nPlot(n_base, list(left = 70), "#!d3.format(',.0')!#", plotID = "web_plot", tooltip = tt)
      n$chart(color = unique(quarterly$hex))
      return(n)
    }
    
  })

  output$trans_plot <- renderChart({
    if (input$Ttime_var == "Monthly"){
      monthly <- var() %>%
        group_by(transaction_type, s_month) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(transaction_type = var_to_label(transaction_type),
               hex = trans_shade(transaction_type))

      n_base <- nPlot(count ~ s_month, group = "transaction_type", data = monthly, type = "multiBarChart", width = session$clientData[["output_Tplot_for_size_width"]])
      tt <- "#! function(key, x, y, e){ return '<p><strong>' + key + '</strong></p><p>' + d3.format(',.0')(e.value) + ' in ' + x + ' 2019 </p>'} !#"
      n <- format_nPlot(n_base, list(left = 100), "#!d3.format(',.0')!#", plotID = "trans_plot", tooltip = tt)
      n$chart(color = unique(monthly$hex))
      return(n)

    }
    else if (input$Ttime_var == "Daily") {
      daily <- var() %>% mutate(transaction_type = var_to_label(transaction_type),
                                hex = trans_shade(transaction_type))

      n_base <- nPlot(count ~ s_date, group = "transaction_type", data = daily, type = "lineChart", width = session$clientData[["output_Tplot_for_size_width"]])
      xFormat <- "#!function(d) {return d3.time.format.utc('%Y-%m-%d')(new Date(d));} !#"
      tt <- "#! function(key, x, y){ return '<p><strong>' + key + '</strong></p><p>' + y + ' on ' + x + '</p>'} !#"
      n <- format_nPlot(n_base, list(left = 100, right = 100), "#!d3.format(',.0')!#", xFormat,"trans_plot", tt)
      n$chart(color = unique(daily$hex))
      return(n)
    }
    else if (input$Ttime_var == "Quarterly") {
      quarterly <- var() %>%
        group_by(transaction_type, f_quarter) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(transaction_type = var_to_label(transaction_type),
               hex = trans_shade(transaction_type))

      n_base <- nPlot(count ~ f_quarter, group = "transaction_type", data = quarterly, type = "multiBarChart", width = session$clientData[["output_Tplot_for_size_width"]])
      tt <- "#! function(key, x, y, e){ return '<p><strong>' + key + '</strong></p><p>' + d3.format(',.0')(e.value) + ' in ' + x + '</p>'} !#"
      n <- format_nPlot(n_base,
                        margin = list(left = 100),
                        ytickFormat = "#!d3.format(',.0')!#",
                        plotID = "trans_plot",
                        tooltip = tt)
      n$chart(color = unique(quarterly$hex))
      return(n)
    }

  })

  # conditional hide/show overdrive footnote
  observeEvent(input$vars, {
    toggle(id = "od_foot", condition = input$vars == "overdrive_trans")
  })
# })
}
