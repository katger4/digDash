server <- function(input, output, session) {
  ### Vars ###
  var <- reactive({
    switch(input$vars, "sierra_trans" = prep_data(df, "transaction_type", trans_name= "sierra"), "overdrive_trans" = prep_data(df, "transaction_type", trans_name= "overdrive"), "cloud_trans" = prep_data(df, "transaction_type", trans_name= "cloudlibrary"))
  })
  
  user_var <- reactive({
    switch(input$users_vars, "nc_users" = prep_data(df, "user_type", cat = "not catalog", users = TRUE), "c_users" = prep_data(df, "user_type", cat = "catalog", users = TRUE))
  })
  
  views_var <- reactive({
    switch(input$views_vars, "nc_users" = prep_data(df, "user_type", cat = "not catalog", views = TRUE), "c_users" = prep_data(df, "user_type", cat = "catalog", views = TRUE))
  })
  
  ### VALUE BOXES ###
  
  output$card_tot <- output$card_tot_tab <- renderValueBox({
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
    text <- HTML(paste(var_to_label(v_type), br(),"<span style='font-size:12px'>Jan 2019 - Jun 2019</span>"))
    valueBox(
      comma_format()(sum(var() %>% filter(transaction_type == v_type) %$% count)), text, icon = icon("book"),
      color = "blue"
    )
  })

  output$t2 <- renderValueBox({
    v_type <- if (input$vars == "sierra_trans") {'sierra_checkouts'}
    else if (input$vars == "overdrive_trans") {'overdrive_ebook_holds'}
    else if (input$vars == "cloud_trans") {'cloudlibrary_ebook_holds'}
    text <- HTML(paste(var_to_label(v_type), br(),"<span style='font-size:12px'>Jan 2019 - Jun 2019</span>"))
    valueBox(
      comma_format()(sum(var() %>% filter(transaction_type == v_type) %$% count)), text, icon = icon("book-open"),
      color = "blue")
    # else {valueBox(NULL,NULL,color = "blue")}
  })

  output$t3 <- renderValueBox({
    v_type <- if (input$vars == "sierra_trans") {'sierra_renewals'}
    else if (input$vars == "overdrive_trans") {'overdrive_audiobook_checkouts'}
    else if (input$vars == "cloud_trans") {'cloudlibrary_audiobook_checkouts'}
    text <- HTML(paste(var_to_label(v_type), br(),"<span style='font-size:12px'>Jan 2019 - Jun 2019</span>"))
    valueBox(
      comma_format()(sum(var() %>% filter(transaction_type == v_type) %$% count)), text, icon = icon("bookmark"),
      color = "blue"
    )
  })
  
  output$t4 <- renderValueBox({
    v_type <- if (input$vars == "sierra_trans") {''}
    else if (input$vars == "overdrive_trans") {'overdrive_audiobook_holds'}
    else if (input$vars == "cloud_trans") {'cloudlibrary_audiobook_holds'}
    text <- HTML(paste(var_to_label(v_type), br(),"<span style='font-size:12px'>Jan 2019 - Jun 2019</span>"))
    if (!input$vars == "sierra_trans"){
      valueBox(
      comma_format()(sum(var() %>% filter(transaction_type == v_type) %$% count)), text, icon = icon("book-reader"),
      color = "blue"
    )}
  })
  
  ### PLOTS ###
  output$views_plot_sum <- renderPlotly({
    view_sum <- prep_data(df, key = "user_type", cat = "not catalog", views = TRUE) %>%
      group_by(user_type, s_year) %>%
      summarise(tot = sum(count)) %>%
      ungroup() %>%
      mutate(user_type = var_to_label(user_type),
             user_lab = tool_label(user_type),
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
    view_sum <- prep_data(df, key = "user_type", cat = "catalog", views = TRUE) %>%
      group_by(user_type, s_year) %>%
      summarise(tot = sum(count)) %>%
      ungroup() %>%
      mutate(user_type = var_to_label(user_type),
             user_lab = tool_label(user_type),
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
    trans <- prep_data(df, key = "transaction_type", trans_sum = TRUE) %>% 
      mutate(grouper = tool_label(transaction_type)) %>%
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
    user_sum <- prep_data(df, "user_type", cat = "catalog", users = TRUE) %>%
      group_by(user_type, s_year) %>%
      summarise(tot = sum(count)) %>%
      ungroup() %>%
      mutate(user_type = var_to_label(user_type),
             user_lab = tool_label(user_type),
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
  
  output$views_plot <- renderChart({
    if (input$Vtime_var == "Monthly"){
      monthly <- views_var() %>%
        group_by(user_type, s_month) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(user_type = var_to_label(user_type),
               user_lab = tool_label(user_type),
               hex = cat_color(user_type))
      
      n_base <- nPlot(count ~ s_month, group = "user_lab", data = monthly, type = "multiBarChart", width = session$clientData[["output_Vplot_for_size_width"]])
      tt <- "#! function(key, x, y, e){ return '<p><strong>' + key + '</strong></p><p>' + d3.format(',.0')(e.value) + ' page views in ' + x + ' 2019 </p>'} !#"
      n <- format_nPlot(n_base, list(left = 100), "#!d3.format(',.0')!#", plotID = "views_plot", tooltip = tt)
      n$chart(color = unique(monthly$hex))
      return(n)
      
    }
    else if (input$Vtime_var == "Daily") {
      daily <- views_var() %>%
        mutate(user_type = var_to_label(user_type),
               user_lab = tool_label(user_type),
               hex = cat_color(user_type))
      
      n_base <- nPlot(count ~ s_date, group = "user_lab", data = daily, type = "lineChart", width = session$clientData[["output_Vplot_for_size_width"]])
      xFormat <- "#!function(d) {return d3.time.format('%Y-%m-%d')(new Date(d));} !#"
      tt <- "#! function(key, x, y){ return '<p><strong>' + key + '</strong></p><p>' + y + ' page views on ' + x + '</p>'} !#"
      n <- format_nPlot(n_base, list(left = 100, right = 100), "#!d3.format(',.0')!#", xFormat,"views_plot", tt)
      n$chart(color = unique(daily$hex))
      return(n)
    }
    else if (input$Vtime_var == "Quarterly") {
      quarterly <- views_var() %>%
        group_by(user_type, s_quarter) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(user_type = var_to_label(user_type),
               user_lab = tool_label(user_type),
               hex = cat_color(user_type))
      
      n_base <- nPlot(count ~ s_quarter, group = "user_lab", data = quarterly, type = "multiBarChart", width = session$clientData[["output_Vplot_for_size_width"]])
      tt <- "#! function(key, x, y, e){ return '<p><strong>' + key + '</strong></p><p>' + d3.format(',.0')(e.value) + ' page views in ' + x + '</p>'} !#"
      n <- format_nPlot(n_base, list(left = 100), "#!d3.format(',.0')!#", plotID = "views_plot", tooltip = tt)
      n$chart(color = unique(quarterly$hex))
      return(n)
    }
    
  })
  
  output$users_plot <- renderChart({
    if (input$Utime_var == "Monthly"){
      monthly <- user_var() %>%
        group_by(user_type, s_month) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(user_type = var_to_label(user_type),
               user_lab = tool_label(user_type),
               hex = cat_color(user_type))

      n_base <- nPlot(count ~ s_month, group = "user_lab", data = monthly, type = "multiBarChart", width = session$clientData[["output_Uplot_for_size_width"]])
      tt <- "#! function(key, x, y, e){ return '<p><strong>' + key + '</strong></p><p>' + d3.format(',.0')(e.value) + ' users in ' + x + ' 2019 </p>'} !#"
      n <- format_nPlot(n_base, list(left = 100), "#!d3.format(',.0')!#", plotID = "users_plot", tooltip = tt)
      n$chart(color = unique(monthly$hex))
      return(n)

    }
    else if (input$Utime_var == "Daily") {
      daily <- user_var() %>%
        mutate(user_type = var_to_label(user_type),
               user_lab = tool_label(user_type),
               hex = cat_color(user_type))

      n_base <- nPlot(count ~ s_date, group = "user_lab", data = daily, type = "lineChart", width = session$clientData[["output_Uplot_for_size_width"]])
      xFormat <- "#!function(d) {return d3.time.format('%Y-%m-%d')(new Date(d));} !#"
      tt <- "#! function(key, x, y){ return '<p><strong>' + key + '</strong></p><p>' + y + ' users on ' + x + '</p>'} !#"
      n <- format_nPlot(n_base, list(left = 100, right = 100), "#!d3.format(',.0')!#", xFormat,"users_plot", tt)
      n$chart(color = unique(daily$hex))
      return(n)
    }
    else if (input$Utime_var == "Quarterly") {
      quarterly <- user_var() %>%
        group_by(user_type, s_quarter) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(user_type = var_to_label(user_type),
               user_lab = tool_label(user_type),
               hex = cat_color(user_type))

      n_base <- nPlot(count ~ s_quarter, group = "user_lab", data = quarterly, type = "multiBarChart", width = session$clientData[["output_Uplot_for_size_width"]])
      tt <- "#! function(key, x, y, e){ return '<p><strong>' + key + '</strong></p><p>' + d3.format(',.0')(e.value) + ' users in ' + x + '</p>'} !#"
      n <- format_nPlot(n_base, list(left = 100), "#!d3.format(',.0')!#", plotID = "users_plot", tooltip = tt)
      n$chart(color = unique(quarterly$hex))
      return(n)
    }

  })

  output$card_plot <- renderChart({

    if (input$Ctime_var == "Monthly"){
      monthly <- prep_card(df) %>%
        group_by(s_month) %>%
        summarise(count = sum(count))
      n_base <- nPlot(count ~ s_month, data = monthly, type = "multiBarChart", width = session$clientData[["output_Cplot_for_size_width"]])
      tt <- "#! function(key, x, y, e){ return '<p><strong>New card sign ups</strong></p><p>' + d3.format(',.0')(e.value) + ' in ' + x + ' 2019 </p>'} !#"
      n <- format_nPlot(n_base, list(left = 100), "#!d3.format(',.0')!#",plotID = "card_plot", tooltip = tt)
      n$chart(showControls = FALSE)
      n$chart(showLegend = FALSE)
      n$chart(color = "#! function(d){ return '#6baed6'} !#")
      return(n)
    }
    else if (input$Ctime_var == "Daily") {
      daily <- prep_card(df)

      n_base <- nPlot(count ~ s_date, data = daily, type = "lineChart", width = session$clientData[["output_Cplot_for_size_width"]])
      xFormat <- "#!function(d) {return d3.time.format('%Y-%m-%d')(new Date(d));} !#"
      tt <- "#! function(key, x, y){ return '<p><strong>New card sign ups</strong></p><p>' + y + ' on ' + x + '</p>'} !#"
      n <- format_nPlot(n_base, list(left = 100, right = 100), "#!d3.format(',.0')!#", xFormat, plotID = "card_plot", tt)
      # n$chart(showControls = FALSE)
      n$chart(showLegend = FALSE)
      n$chart(color = "#! function(d){ return '#6baed6'} !#")
      return(n)
    }
    else if (input$Ctime_var == "Quarterly") {
      quarterly <- prep_card(df) %>%
        group_by(s_quarter) %>%
        summarise(count = sum(count))

      n_base <- nPlot(count ~ s_quarter, data = quarterly, type = "multiBarChart", width = session$clientData[["output_Cplot_for_size_width"]])
      tt <- "#! function(key, x, y, e){ return '<p><strong>New card sign ups</strong></p><p>' + d3.format(',.0')(e.value) + ' in ' + x + '</p>'} !#"
      n <- format_nPlot(n_base, list(left = 100), "#!d3.format(',.0')!#", plotID = "card_plot", tooltip = tt)
      n$chart(showControls = FALSE)
      n$chart(showLegend = FALSE)
      n$chart(color = "#! function(d){ return '#6baed6'} !#")
      return(n)
    }

  })
  
  output$trans_plot <- renderChart({
    if (input$Ttime_var == "Monthly"){
      monthly <- var() %>%
        group_by(transaction_type, s_month) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(transaction_type = var_to_label(transaction_type))
      
      n_base <- nPlot(count ~ s_month, group = "transaction_type", data = monthly, type = "multiBarChart", width = session$clientData[["output_Tplot_for_size_width"]])
      tt <- "#! function(key, x, y, e){ return '<p><strong>' + key + '</strong></p><p>' + d3.format(',.0')(e.value) + ' in ' + x + ' 2019 </p>'} !#"
      n <- format_nPlot(n_base, list(left = 100), "#!d3.format(',.0')!#", plotID = "trans_plot", tooltip = tt)
      return(n)
      
    } 
    else if (input$Ttime_var == "Daily") {
      daily <- var() %>% mutate(transaction_type = var_to_label(transaction_type))
      
      n_base <- nPlot(count ~ s_date, group = "transaction_type", data = daily, type = "lineChart", width = session$clientData[["output_Tplot_for_size_width"]])
      xFormat <- "#!function(d) {return d3.time.format('%Y-%m-%d')(new Date(d));} !#"
      tt <- "#! function(key, x, y){ return '<p><strong>' + key + '</strong></p><p>' + y + ' on ' + x + '</p>'} !#"
      n <- format_nPlot(n_base, list(left = 100, right = 100), "#!d3.format(',.0')!#", xFormat,"trans_plot", tt)
      return(n) 
    } 
    else if (input$Ttime_var == "Quarterly") {
      quarterly <- var() %>%
        group_by(transaction_type, s_quarter) %>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        mutate(transaction_type = var_to_label(transaction_type))
      
      n_base <- nPlot(count ~ s_quarter, group = "transaction_type", data = quarterly, type = "multiBarChart", width = session$clientData[["output_Tplot_for_size_width"]])
      tt <- "#! function(key, x, y, e){ return '<p><strong>' + key + '</strong></p><p>' + d3.format(',.0')(e.value) + ' in ' + x + '</p>'} !#"
      n <- format_nPlot(n_base, list(left = 100), "#!d3.format(',.0')!#", plotID = "trans_plot", tooltip = tt)
      return(n)
    }

  })
  
}
