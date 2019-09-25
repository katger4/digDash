library(tidyverse)
library(lubridate)
# library(googledrive)
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

today_day <- day(as_date(today()))
# august data INCOMPLETE as of 9/6/2019
# today_day <- 31

today_range <- today_day+2

# baseline <- readRDS("./app/data/today_data.rds")
baseline_bk <- gs_title("data_drop")
baseline <- baseline_bk %>% gs_read(ws = 1)

today_data <- dddd %>%
  gs_read(ws = active_sheet_name, range = paste0("B2:AG",today_range), na = c("", "N/A", "NA")) %>%
  clean_df(.,"2019") %>%
  bind_rows(baseline) %>%
  # in case script is run > 1x per day, need this to delete dup daily rows 
  arrange(desc(date_dash)) %>%
  group_by(date_dash) %>%
  slice(1) %>%
  select(-starts_with("kanopy")) %>%
  ungroup()

# create new sheet
gs_ws_new(baseline_bk, paste0(as.character(today()),'_',as.character(hour(now()))), input = today_data)
# gs_ws_new(baseline_bk, paste0(as.character(today()),'_',as.character(hour(now())),'_3'), input = today_data)
# delete old sheet
gs_ws_delete(baseline_bk, ws = 1)

# saveRDS(today_data, file = "./app/data/today_data.rds")
# write_csv(today_data, "./app/data/today_data.csv")
