# clean data

library(tidyverse)
library(glue)
library(lubridate)

# get data
water_year <- 2021

# list files and get most recent
files <- fs::dir_ls("data_raw", glob = "*csv")
files <- fs::dir_ls("data_raw", regexp = "hist")

# get most recent file only
files_info <- fs::dir_info("data_raw", regexp = "hist")
(file_recent <- files_info %>%
    select(path, modification_time) %>%
    arrange(modification_time) %>%
    slice_max(order_by = modification_time))

# remove any other files
files_info %>% filter(path!=file_recent$path) %>%
  pull(path) %>%
  fs::file_delete()

# read in most recent
df <- read_csv(glue("{file_recent$path}"))

# clean
df_clean <- df %>%
  rename(date = Date) %>%
  mutate(year = year(date), month = month(date),
         water_year = year(date) + ifelse(month(date) >= 10, 1, 0),
         water_day = (date - as.Date(sprintf('%s-10-01', water_year)))) %>%
  group_by(water_year) %>%
  mutate(water_day = as.numeric(water_day - min(water_day) + 1)) %>%
  ungroup()

data <- df_clean

# function to add stats
calc_yr_stats <-  function(data){

  # find peaks
  peaks <- data %>%
    group_by(site_no, water_year)%>%
    summarize(peak_q = max(Flow, na.rm=TRUE)) %>%
    mutate(peak50_q = peak_q/2)

  peak_dates <- data %>%
    left_join(peaks) %>%
    group_by(site_no, water_year) %>%
    filter(Flow == peak_q) %>%
    filter(water_day == max(water_day, na.rm=TRUE)) %>% # take latest date peak is measured
    mutate(peak_date = date, peak_day = water_day) %>%
    dplyr::select(-date, -water_day, -Flow, -year) %>%
    ungroup() %>%
    mutate(peak_met = case_when(
      as.character(peak_date) == Sys.Date() ~ 'TBD',
      TRUE ~ as.character(peak_date))) %>%
    distinct(peak_q, .keep_all = TRUE)

  m50_dates <- data %>%
    left_join(peak_dates) %>%
    group_by(site_no, water_year)%>%
    filter(water_day >= peak_day) %>%
    filter(Flow <= peak50_q) %>%
    filter(water_day == min(water_day, na.rm=TRUE)) %>% # earliest water day at or below peak/2
    mutate(peak50_day = water_day,
           peak50_date = date) %>%
    dplyr::select(-date, -water_day, -Flow)%>%
    mutate(peak50_met = case_when(
      as.character(peak50_date) == Sys.Date() ~ 'TBD',
      TRUE ~ as.character(peak50_date)))

  m50_current <- m50_dates %>% filter(water_year == 2021)

  # if sm50 has not been met in 2021, add empty row with latest day
  tbd_sites <- setdiff(unique(data$site_no), unique(m50_current$site_no))

  tbd_date <- data %>%
    filter(water_year == 2021)%>%
    filter(site_no %in% tbd_sites) %>%
    left_join(peak_dates) %>%
    group_by(site_no, water_year)%>%
    filter(water_day == max(water_day, na.rm=TRUE)) %>% # set everything to latest date
    mutate(peak50_day = water_day,
           peak50_date = date) %>%
    dplyr::select(-date, -water_day, -Flow) %>%
    mutate(peak50_met = 'TBD')

  data_out <- rbind(m50_dates, tbd_date)

  return(data_out)
  #return(peak_dates)
}

pks <- calc_yr_stats(df_clean)

library(hydrostats)
df2 <- ts.format(df_clean, format = "%Y-%m-%d", cols = c(3, 5))
seasonality(df2)


# calc stats: see here: https://github.com/USGS-VIZLAB/snow-to-flow/blob/main/data_processing_pipeline/2_process/src/prep_sntl.R
# https://github.com/USGS-VIZLAB/snow-to-flow/blob/main/data_processing_pipeline/1_fetch/src/fetch_SNOTEL.R
# use word() to pull year: word(nfa$date, 1,1, sep="-")

# https://cran.r-project.org/web/packages/FlowScreen/FlowScreen.pdf
library(FlowScreen)
data("cania.sub.ts")
head(caniapiscau)
res1 <- pk.cov(cania.sub.ts)
(res2 <- screen.metric(res1[,2], "Day of Year"))
screen.cpts(caniapiscau.res, type="b")
screen.frames(caniapiscau.res, type="b")

df2 <- df_clean %>% select(site_no, date, Flow, Flow_cd, agency_cd) %>%
  as.data.frame() %>%
  mutate(Flow = Flow * 0.0283168,
         PARAM = 1) %>%
  rename(Date=date, ID=site_no, SYM=Flow_cd, Agency=agency_cd) %>%
  select(ID, PARAM, Date, Flow, SYM, Agency)
df2 <- create.ts(df2)
nfares <- pk.cov(df2)

ggplot() + geom_point(data=nfares, aes(x=hYear, y=Q50, fill=Q50), pch=21, size=4) +
  scale_x_continuous(breaks = seq(1940,2020,2)) +
  theme_classic(base_size = 10, base_family = "Roboto Condensed") +
  theme(axis.text.x = element_text(angle=330, hjust = 0.4, vjust = 0.1 ))

(res2 <- screen.metric(nfares[,2], "Day of Year"))
nfares <- metrics.all(df2)
screen.cpts(nfares, type="b")
screen.cpts(nfares, type="l")
screen.cpts(nfares, type="h")
screen.frames(nfares, type="l")
screen.frames(nfares, type="h")
