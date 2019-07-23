library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(Cairo)
options(shiny.usecairo=T)
library(shinycssloaders)
library(shinyjs)

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

views_choices <- c("Encore and Website" = "nc_users",
                 "Classic and Shared catalog" = "c_users")

user_choices <- c("Non catalog users" = "nc_users",
                  "Catalog users" = "c_users")

time_choices <- c("Monthly","Daily","Quarterly")

df <- readRDS(file = "./data/today_data.rds")

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

trans_shade <- function(var_name) {
  var_name <- tolower(var_name)
  col<- case_when(grepl("sierra",var_name) & grepl("checkins",var_name) ~ "#addfd7",
                  grepl("sierra",var_name) & grepl("checkouts",var_name) ~ "#66a49a",
                  grepl("sierra",var_name) & grepl("renewals",var_name) ~ "#426962",
                  grepl("overdrive",var_name) & grepl("audio",var_name) & grepl("checkouts",var_name) ~ "#cab6d4",
                  grepl("overdrive",var_name) & grepl("audio",var_name) & grepl("holds",var_name) ~ "#ad8dbc",
                  grepl("overdrive",var_name) & grepl("ebook",var_name) & grepl("checkouts",var_name) ~ "#7a5a89",
                  grepl("overdrive",var_name) & grepl("ebook",var_name) & grepl("holds",var_name) ~ "#4e3a58",
                  grepl("cloud",var_name) & grepl("audio",var_name) & grepl("checkouts",var_name) ~ "#aad5ae",
                  grepl("cloud",var_name) & grepl("audio",var_name) & grepl("holds",var_name) ~ "#7bbe81",
                  grepl("cloud",var_name) & grepl("ebook",var_name) & grepl("checkouts",var_name) ~ "#488b4e",
                  grepl("cloud",var_name) & grepl("ebook",var_name) & grepl("holds",var_name) ~ "#2e5932")
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
  return (case_when(between(month(ymd_date),7,9) ~ paste('Q1',as.character(year(ymd_date)+1)),
                    between(month(ymd_date),10,12) ~ paste('Q2',as.character(year(ymd_date)+1)),
                    between(month(ymd_date),1,3) ~ paste('Q3',as.character(year(ymd_date))),
                    between(month(ymd_date),4,6) ~ paste('Q4',as.character(year(ymd_date)))
                    )
          )
}

create_fy_month <- function(ymd_date) {
  factor(month(ymd_date), levels = c(7:12, 1:6), labels = c(month.abb[7:12], month.abb[1:6]))
}

create_fy_year <- function(ymd_date) {
  ifelse(between(month(ymd_date),7,12), year(ymd_date)+1, year(ymd_date))
}

create_fy_qtr <- function(ymd_date, fy_year) {
  paste0("FY ", fy_year, " Q", quarter(ymd_date, with_year = FALSE, fiscal_start = 7))
}

prep_data <- function(df, key, cat, users, views, trans_name, trans_sum, card) {
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
  
  if (missing(card)) {
    gathered <- selected %>% gather_(key = key, value = "count", setdiff(names(.), 'date_dash'))
  }
  else {
    gathered <- df %>% 
      select(new_card_sign_ups, date_dash) %>%
      rename(count = new_card_sign_ups)
  }

  gathered %>%
    mutate(s_month = month(date_dash, label = TRUE),
           s_date = create_d3_date(date_dash),
           # f_quater = create_fy_qtr(date_dash),
           s_year = year(date_dash),
           f_month = create_fy_month(date_dash),
           f_year = create_fy_year(date_dash),
           s_quarter = create_fy_qtr(date_dash, f_year)
    ) %>%
    arrange(s_date)
}

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

# view_sum <- prep_data(df, key = "user_type", cat = "not catalog", views = TRUE) %>%
#   group_by(user_type, s_year) %>%
#   summarise(tot = sum(count)) %>%
#   ungroup() %>%
#   mutate(user_type = var_to_label(user_type),
#          user_lab = tool_label(user_type),
#          hex = cat_color(user_lab))

# u <- prep_data(df, key = "user_type", cat="catalog", users = TRUE)
# 
# monthly <- u %>%
#   group_by(user_type, f_month, s_month) %>%
#   summarise(count = sum(count)) %>%
#   ungroup() %>%
#   mutate(user_type = var_to_label(user_type),
#          user_lab = tool_label(user_type),
#          hex = cat_color(user_type))
# n_base <- nPlot(count ~ f_month, group = "user_lab", data = monthly, type = "multiBarChart")
# tt <- "#! function(key, x, y, e){ return '<p><strong>' + key + '</strong></p><p>' + d3.format(',.0')(e.value) + ' users in ' + x + ' 2019 </p>'} !#"
# n <- format_nPlot(n_base, list(left = 100), "#!d3.format(',.0')!#", plotID = "users_plot", tooltip = tt)
# n$chart(color = unique(monthly$hex))
# n
