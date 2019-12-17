library(tidyverse)
library(lubridate)
library(googledrive)
library(googlesheets)

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

range_month <- function(ws_name) {
  month_33 <- c('jan','mar','may','jul','aug','oct','dec')
  month_32 <- c('sep','apr','jun','nov')
  month_30 <- c('feb')
  mon <- tolower(substr(ws_name, 0, 3))
  range <- case_when(mon %in% month_33 ~ "B2:AG33",
                     mon %in% month_32 ~ "B2:AG32",
                     mon %in% month_30 ~ "B2:AG30")
  return(range)
}

dddd <- gs_title("Daily Digital DATA DROP ")

active_sheet_name <- gs_ws_ls(dddd)[[1]]

today_day <- 13
today_day <- day(as_date(today()))

today_range <- today_day+2

jan_19 <- dddd %>%
  gs_read(ws = "JAN 2019", range = "B2:AG33") %>%
  clean_df(.,"2019")
feb_19 <- dddd %>%
  gs_read(ws = "February 2019", range = "B2:AG30") %>%
  clean_df(.,"2019") %>%
  bind_rows(jan_19)
mar_19 <- dddd %>%
  gs_read(ws = "March 2019", range = "B2:AG33") %>%
  clean_df(.,"2019") %>%
  bind_rows(feb_19)
apr_19 <- dddd %>%
  gs_read(ws = "April 2019", range = "B2:AG32") %>%
  clean_df(.,"2019")  %>%
  bind_rows(mar_19)
may_19 <- dddd %>%
  gs_read(ws = "May 2019", range = "B2:AG33") %>%
  clean_df(.,"2019") %>%
  bind_rows(apr_19)
jun_19 <- dddd %>%
  gs_read(ws = "June 2019", range = "B2:AG32") %>%
  clean_df(.,"2019") %>%
  bind_rows(may_19) %>%
  replace(., is.na(.), 0)
jul_19 <- dddd %>%
  gs_read(ws = "July 2019", range = "B2:AG33", na = c("", "N/A", "NA")) %>%
  clean_df(.,"2019") %>%
  bind_rows(jun_19) %>%
  replace(., is.na(.), 0)
aug_19 <- dddd %>%
  gs_read(ws = "August 2019", range = "B2:AG33", na = c("", "N/A", "NA")) %>%
  clean_df(.,"2019") %>%
  bind_rows(jul_19) %>%
  replace(., is.na(.), 0)
sep_19 <- dddd %>%
  gs_read(ws = "September 2019", range = "B2:AG32", na = c("", "N/A", "NA")) %>%
  clean_df(.,"2019") %>%
  bind_rows(aug_19) %>%
  replace(., is.na(.), 0)
oct_19 <- dddd %>%
  gs_read(ws = "October 2019", range = "B2:AG33", na = c("", "N/A", "NA")) %>%
  clean_df(.,"2019") %>%
  bind_rows(sep_19) %>%
  replace(., is.na(.), 0)
nov_19 <- dddd %>%
  gs_read(ws = "November 2019", range = "B2:AG32", na = c("", "N/A", "NA")) %>%
  clean_df(.,"2019") %>%
  bind_rows(oct_19) %>%
  replace(., is.na(.), 0)
dec_19 <- dddd %>%
  gs_read(ws = "December 2019", range = "B2:AG13", na = c("", "N/A", "NA")) %>%
  clean_df(.,"2019") %>%
  bind_rows(nov_19) %>%
  replace(., is.na(.), 0)

saveRDS(dec_19, file = "./app/data/dec_19.rds")
write_csv(dec_19, "./app/data/dec_19.csv")
ss <- drive_get(id = "1Jt0FUs2-D2bUdVpavJNY-UMemgld57s_ApdiOlEQDQU") 
drive_update(ss, "./app/data/dec_19.csv")

baseline_bk <- gs_title("data_drop")
baseline <- baseline_bk %>% gs_read(ws = 1)
# saveRDS(baseline, file = "./app/data/nov_baseline_bad.rds")

gs_ws_new(baseline_bk, paste0(as.character(today()),'_',as.character(hour(now())),'_2'), input = nov_19)
gs_ws_delete(baseline_bk, ws = 1)

nov_sum <- nov_19 %>% mutate(row_sum = rowSums(select(., website_visits_page_views:cloudlibrary_unique_users)))

bs_sum <- baseline %>% mutate(row_sum = rowSums(select(., website_visits_page_views:cloudlibrary_unique_users)))

new_rows <- nov_sum %>% 
  left_join(bs_sum, by=c("date_dash" = "date_dash")) %>% 
  filter(row_sum.x != row_sum.y) %>%
  select(ends_with(".x")) %>%
  rename_all(~gsub('.x', '', .x))

gs_ws_new(baseline_bk, paste0(as.character(today()),'_',as.character(hour(now())),'_test'), input = baseline)

write_csv(nov_19, "./app/data/nov_19.csv")

start_time <- Sys.time()
# gs_edit_cells(ss = baseline_bk, ws = "2019-11-19_14_test", input = nov_19, anchor = "A1", trim = TRUE, col_names = TRUE)
ss <- drive_get(id = "1Jt0FUs2-D2bUdVpavJNY-UMemgld57s_ApdiOlEQDQU") 

drive_update(ss, "./app/data/nov_19.csv")

end_time <- Sys.time()
end_time - start_time

# all_equal(baseline, nov_19, convert = TRUE)

# VLookup <- function(this, data, key, value) {
#   m <- match(this, data[[key]])
#   data[[value]][m]
# }

baseline <- readRDS("./app/data/jun_19.rds")

today_data <- dddd %>%
  gs_read(ws = active_sheet_name, range = paste0("B2:AG",today_range), na = c("", "N/A", "NA")) %>%
  clean_df(.,"2019") %>%
  bind_rows(baseline)

saveRDS(today_data, file = "./app/data/today_data.rds")

  


# sierra_month <- sierra_month[with(sierra_month, order(month_name, month_num)), ]

s <- jun_19 %>% 
  select(starts_with("sierra"), date_dash) %>%
  gather(transaction_type, count, sierra_checkouts:sierra_renewals) %>%
  mutate(s_month = month(date_dash, label = TRUE),
         s_date = as.double(as.POSIXct(as.Date(date_dash),origin="1970-01-01")) * 1000,
         t_date = as.numeric(as.POSIXct(date_dash,tz='UTC')) * 1000) %>%
  # filter(transaction_type == 'sierra_checkins') %>%
  arrange(s_date)

n1 <- nPlot(count ~ s_date, group = "transaction_type", data = s, type = "lineChart")
n1$chart(margin = list(left = 100, right = 100))
n1$yAxis( tickFormat = "#!d3.format(',.0')!#" )
n1$xAxis( tickFormat = "#!function(d) {return d3.time.format('%Y-%m-%d')(new Date(d));} !#" )
n1$chart(forceX = c("2019-01-01","2019-06-30"))
n1
# n1$xAxis( tickFormat = "#!d3.format(',r')!#" )
n1$addParams(dom = 'trans_plot')
# n1$chart(tooltipContent = "#! function(key, x, y, e){ return '<p><strong>' + key + '</strong></p><p>' + e.value + ' in ' + x + ' 2019 </p>'} !#")
return(n1) 

# facetted scatter plot
  p1 <- rPlot("date_dash", "count", data = sierra, color = "transaction_type", 
              facet = "transaction_type", type = 'point')
  rPlot("date_dash", "count", data = sierra, color = "transaction_type", 
              facet = "transaction_type", type = 'line')
  # p1$addParams(dom = 'myChart')
  # return(p1)
  n1 <- nPlot(count ~ transaction_type, group = "transaction_type", data = sierra, type = "multiBarChart")
  n1 <- nPlot(m_total ~ s_month, group = "transaction_type", data = sierra_month, type = "multiBarChart")
  n1$chart(margin = list(left = 100))
  n1$yAxis( tickFormat = "#!d3.format(',.0')!#" )
  n1  


