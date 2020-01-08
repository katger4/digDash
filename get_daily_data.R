library(tidyverse)
library(lubridate)
library(googledrive)
library(googlesheets)

setwd("/Users/katgertz/Desktop/digDash/")

# options(httr_oob_default=TRUE) 
# shiny_token <- gs_auth(new_user = TRUE, cache=FALSE)
# saveRDS(shiny_token, "gdd_token.rds")
googlesheets::gs_auth(token = "gdd_token.rds")

clean_df <- function(df, ws_year) {
  new_names <- df %>%
    replace(., is.na(.), 0) %>%
    mutate(date_dash = dmy(paste(X2,"-",ws_year))) %>%
    select(-starts_with("X")) %>%
    rename(weekday = DATE) %>%
    rename_all(tolower) %>%
    rename_all(str_trim) %>%
    rename_all(~gsub('\n| ', '_', .x)) %>%
    rename_all(~gsub('\\(|\\)', '', .x))
  return(new_names)
}

dddd <- gs_title("Daily Digital DATA DROP ")

active_sheet_name <- gs_ws_ls(dddd)[[1]]
mon <- str_split(active_sheet_name, " ")[[1]][1]

if (match(mon, month.name)) {
  active_month <- match(mon, month.name)
} else if (match(mon, month.abb)) {
  active_month <- match(mon, month.abb)
} else { active_month <- ''}

if (active_month == month(as_date(today()))) {
  today_day <- day(as_date(today()))
  today_range <- today_day+2
} else { 
  max_days <- days_in_month(as.Date(paste0(str_split(active_sheet_name, " ")[[1]][2],'-',active_month,'-01')))
  today_range <- unname(max_days)+2
    }

# baseline <- readRDS("./app/data/today_data.rds")
baseline_bk <- gs_title("data_drop")
baseline <- baseline_bk %>% gs_read(ws = 1)

today_data <- dddd %>%
  gs_read(ws = active_sheet_name, range = paste0("B2:AG",today_range), na = c("", "N/A", "NA")) %>%
  clean_df(.,year(as_date(today()))) %>%
  bind_rows(baseline) %>%
  # in case script is run > 1x per day, need this to delete dup daily rows 
  arrange(desc(date_dash)) %>%
  group_by(date_dash) %>%
  slice(1) %>%
  select(-starts_with("kanopy")) %>%
  ungroup()

# create new sheet
drive_auth(email = readRDS("email.rds"))
write_csv(today_data, "./app/data/today_data.csv")
ss <- drive_get(id = readRDS("key.rds")) 
drive_update(ss, "./app/data/today_data.csv")

