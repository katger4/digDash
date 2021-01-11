library(shiny)
library(shinydashboard)
library(shinyjs)

library(tidyverse)
library(scales)
library(magrittr)
library(lubridate)
library(rCharts)
library(highcharter)

# to setup googlesheet auth (comment out once done)
# options(httr_oob_default=TRUE) 
# shiny_token <- gs_auth(new_user = TRUE, cache=FALSE)
# saveRDS(shiny_token, "shiny_app_token.rds")
# file.info("shiny_app_token.rds")

googlesheets::gs_auth(token = "shiny_app_token.rds")
df_ss <- googlesheets::gs_title("data_drop")

# create logo
logo_beta <- dashboardthemes::shinyDashboardLogoDIY(
  boldText = "Digital"
  ,mainText = "dashboard"
  ,textSize = 16
  ,badgeText = "BETA"
  ,badgeTextColor = "white"
  ,badgeTextSize = 2
  ,badgeBackColor = "#bcbddc"
  ,badgeBorderRadius = 3)

circ_choices <- c("Sierra"="sierra_trans",
                  "Overdrive" = "overdrive_trans",
                  "CloudLibrary" = "cloud_trans")

sierra_choices <- c("Checkins" = "sierra_checkins",
                    "Checkouts" = "sierra_checkouts",
                    "Renewals" = "sierra_renewals")

odrive_choices <- c("Audiobook checkouts" = "overdrive_audiobook_checkouts",
                    "Ebook checkouts" = "overdrive_ebook_checkouts")

cloud_choices <- c("Audiobook checkouts" = "cloudlibrary_audiobook_checkouts",
                   "Audiobook holds" = "cloudlibrary_audiobook_holds",
                   "Ebook checkouts" = "cloudlibrary_ebook_checkouts",
                   "Ebook holds" = "cloudlibrary_ebook_holds")

web_choices <- c("Users" = "website_visits_users", "Page views" = "website_visits_page_views")

views_choices <- c("Shared catalog" = "search_requests_shared_catalog_page_views",
                   "Classic catalog" = "search_requests_classic_catalog_page_views",
                   "Encore" = "search_requestsencore__page_views")

user_choices <- c("Shared catalog" = "search_requests_shared_catalog_users",
                  "Classic catalog" = "search_requests_classic_catalog_users",
                  "CloudLibrary" = "cloudlibrary_unique_users",
                  "Overdrive" = "overdrive_unique_users",
                  "Encore" = "search_requests_encore__users")

time_choices <- c("Monthly","Daily","Quarterly","Weekday")

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
                  grepl("cloud",var_name) & grepl("ebook",var_name) & grepl("holds",var_name) ~ "#2e5932",
                  grepl("website",var_name) & grepl("views",var_name) ~ "#49a19a",
                  grepl("website",var_name) & grepl("users",var_name) ~ "#9acbc7")
  return(col)
}

var_to_label <- function(var_name) {
  var_name %<>%
    gsub('_', ' ', .) %>%
    gsub("(^[[:alpha:]])", "\\U\\1", ., perl=TRUE) %>%
    gsub('Cloudlibrary', 'CloudLibrary', .)
}

tool_label <- function(col_name) {
  return(case_when(grepl("sierra",col_name) ~ "Sierra",
                   grepl("overdrive",col_name) ~ "Overdrive",
                   grepl("cloud",col_name) ~ "CloudLibrary",
                   grepl("Website",col_name) ~ "Website",
                   grepl("Overdrive",col_name) ~ "Overdrive",
                   grepl("Cloud",col_name) ~ "CloudLibrary",
                   grepl("encore",col_name) ~ "Encore",
                   grepl("classic",col_name) ~ "Classic catalog",
                   grepl("shared",col_name) ~ "Shared catalog")
  )
}

create_d3_date <- function(ymd_date) {
  return(as.double(as.POSIXct(as.Date(ymd_date),origin="1970-01-01")) * 1000)
}

create_fy_month <- function(ymd_date) {
  factor(month(ymd_date), levels = c(7:12, 1:6), labels = c(month.abb[7:12], month.abb[1:6]))
}

create_fy_year <- function(ymd_date) {
  paste0("FY",ifelse(between(month(ymd_date),7,12), year(ymd_date)+1, year(ymd_date)))
}

create_fy_qtr <- function(ymd_date, fy_year) {
  paste0(fy_year, " Q", quarter(ymd_date, with_year = FALSE, fiscal_start = 7))
}

select_data <- function(df, choices, dates) {
  if (dates == TRUE) {
    selected <- df %>% select(choices, date_dash,s_month,s_date,s_year,f_month,f_year,f_quarter,s_weekday)
  } else {
    selected <- df %>% select(choices) 
  } 
  return(selected)
}

prep_data <- function(df, choices, dates, key, card) {
  selected <- select_data(df, choices, dates)
  if (!missing(card)) {
    selected %>% rename(count = new_card_sign_ups) %>% arrange(s_date)
  } else if (dates == FALSE){
    selected %>% gather_(key = key, value = "count", setdiff(names(.), names(df %>% select(date_dash,s_month,s_date,s_year,f_month,f_year,f_quarter,s_weekday))))
  }
  else {
    selected %>% 
      gather_(key = key, value = "count", setdiff(names(.), names(df %>% select(date_dash,s_month,s_date,s_year,f_month,f_year,f_quarter,s_weekday)))) %>%
      arrange(s_date)
  }
}

prep_bars <- function(df, catalog, circ, card, web, group_var, time_var, count_var) {
  if (!missing(card)) {
    grouped <- df %>%
      group_by(!!as.name(time_var)) %>%
      summarise(count = sum(count)) %>% 
      ungroup() %>%
      rename(plot_var = !!as.name(time_var))
    return(grouped)
  } 
  else {
    grouped <- df %>% 
      group_by(!!as.name(group_var), !!as.name(time_var)) %>%
      summarise(count = sum(count)) %>%
      ungroup() %>%
      rename(plot_var = !!as.name(time_var))
    if (!missing(catalog)) {
      grouped <- grouped %>%
        mutate(user_lab = tool_label(var_to_label(user_type)),
               hex = cat_color(user_type),
               log_count = ifelse(!count == 0, log(count, 10), NA)) %>%
        filter(!is.na(log_count))
    } else if (!missing(web)) {
      grouped$user_lab <- ifelse(grepl("views",grouped$user_type), "Page views", "Users")
      grouped$hex <- trans_shade(grouped$user_type)
    } else if (!missing(circ)) {
      grouped$transaction_type <- var_to_label(grouped$transaction_type)
      grouped$hex <- trans_shade(grouped$transaction_type)
    } 
    return(arrange(grouped, plot_var, !!as.name(count_var)))
  } 
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
