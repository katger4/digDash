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

var_choices <- c("Sierra transactions"="sierra_trans",
                 "Overdrive transactions" = "overdrive_trans",
                 "CloudLibrary transactions" = "cloud_trans")

user_choices <- c("Non catalog users" = "nc_users",
                 "Catalog users" = "c_users")

time_choices <- c("Monthly","Daily","Quarterly")

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

tool_label <- function(col_name) {
  return(case_when(grepl("sierra",col_name) ~ "Sierra",
                   grepl("overdrive",col_name) ~ "Overdrive",
                   grepl("cloud",col_name) ~ "CloudLibrary",
                   grepl("Website",col_name) ~ "Website",
                   grepl("Overdrive",col_name) ~ "Overdrive",
                   grepl("Cloud",col_name) ~ "CloudLibrary",
                   grepl("encore",col_name) ~ "Encore",
                   grepl("classic",col_name) ~ "Classic",
                   grepl("shared",col_name) ~ "Shared")
         )
}

create_d3_date <- function(ymd_date) {
  return(as.double(as.POSIXct(as.Date(ymd_date),origin="1970-01-01")) * 1000)
}

create_quarters <- function(ymd_date) {
  return (case_when(between(month(ymd_date),7,9) ~ paste('Q1',as.character(year(ymd_date))),
                    between(month(ymd_date),10,12) ~ paste('Q2',as.character(year(ymd_date))),
                    between(month(ymd_date),1,3) ~ paste('Q3',as.character(year(ymd_date))),
                    between(month(ymd_date),4,6) ~ paste('Q4',as.character(year(ymd_date)))
                    )
          )
}

prep_card <- function(df) {
  df %>% 
    select(new_card_sign_ups, date_dash) %>%
    rename(count = new_card_sign_ups) %>%
    mutate(s_month = month(date_dash, label = TRUE),
           s_date = create_d3_date(date_dash),
           s_quarter = create_quarters(date_dash)
    ) %>%
    arrange(s_date) 
}

prep_data <- function(df, key, cat, users, views, trans_name, trans_sum) {
  if (!missing(cat)){  
    if (cat == "not catalog" & !missing(users)) {
      selected <- df %>% select(ends_with("users"), -contains("catalog"), date_dash)
      } else if (cat == "catalog" & !missing(users)) {
        selected <- df %>% select(intersect(ends_with("users"), contains("catalog")), date_dash) 
      } else if (cat == "catalog" & !missing(views)) {
        selected <- df %>% select(intersect(contains("views"), contains("catalog")), date_dash) 
      } else if (cat == "not catalog" & !missing(views)) {
        selected <- df %>% select(contains("views"), -contains("catalog"), date_dash)
      } 
  }
  if (!missing(trans_name)) {
    selected <- df %>% select(starts_with(trans_name), -ends_with("users"), date_dash)
  }
  if (!missing(trans_sum)) {
    selected <- df %>% select(starts_with("sierra"), starts_with("overdrive"), starts_with("cloudlibrary"), date_dash)
  }

  selected %>%
    gather_(key = key, value = "count", setdiff(names(.), 'date_dash')) %>%
    mutate(s_month = month(date_dash, label = TRUE),
           s_date = create_d3_date(date_dash),
           s_quarter = create_quarters(date_dash),
           s_year = year(date_dash)
    ) %>%
    arrange(s_date)
}

# group_by_time <- function(prepped_df, category_var, time_var) {
#   # category_var <- as.name(category_var)
#   # time_var <- as.name(time_var)
#   prepped_df %>%
#     group_by_at(vars(category_var,time_var)) %>%
#     summarise(count = sum(count))
# }

format_nPlot <- function(n_base, margin, ytickFormat, xtickFormat, plotID, tooltip) {
  n_base$chart(margin = margin)
  n_base$yAxis(tickFormat = ytickFormat)
  if (!missing(xtickFormat)) {
    n_base$xAxis(tickFormat = xtickFormat)
  }
  n_base$addParams(dom = plotID)
  n_base$chart(tooltipContent = tooltip)
  return(n_base) 
}

