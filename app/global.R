library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(Cairo)
options(shiny.usecairo=T)

library(tidyverse)
library(scales)
library(magrittr)
library(lubridate)
library(packcircles)
library(plotly)
library(rCharts)

# df <- readRDS(file = "./data/jun_19.rds") 

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

time_choices <- c("Monthly","Daily","Quarterly")

prep_trans <- function(df, trans_name) {
  df %>% 
    select(starts_with(trans_name), -ends_with("users"), date_dash) %>%
    gather_(key = "transaction_type", value = "count", setdiff(names(.), 'date_dash')) %>%
    mutate(s_month = month(date_dash, label = TRUE),
           s_date = as.double(as.POSIXct(as.Date(date_dash),origin="1970-01-01")) * 1000,
           s_quarter = case_when(between(month(date_dash),7,9) ~ paste('Q1',as.character(year(date_dash))),
                                 between(month(date_dash),10,12) ~ paste('Q2',as.character(year(date_dash))),
                                 between(month(date_dash),1,3) ~ paste('Q3',as.character(year(date_dash))),
                                 between(month(date_dash),4,6) ~ paste('Q4',as.character(year(date_dash)))
                                 )
           ) %>%
    arrange(s_date) 
}

format_nPlot <- function(n_base, margin, ytickFormat, xtickFormat, tooltip) {
  n_base$chart(margin = margin)
  n_base$yAxis(tickFormat = ytickFormat)
  if (!missing(xtickFormat)) {
    n_base$xAxis(tickFormat = xtickFormat)
  }
  n_base$addParams(dom = 'trans_plot')
  n_base$chart(tooltipContent = tooltip)
  return(n_base) 
}

# s <- prep_trans(df, "sierra")
# 
# monthly <- s %>%
#   group_by(transaction_type, s_month) %>%
#   summarise(count = sum(count)) %>%
#   ungroup() %>%
#   mutate(transaction_type = var_to_label(transaction_type))

