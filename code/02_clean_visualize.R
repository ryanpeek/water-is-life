# clean data and visualize

library(tidyverse)
library(glue)
library(lubridate)


# Get Data ----------------------------------------------------------------

# list csv files in dir
files <- fs::dir_ls("data_raw", glob = "*csv")

# get file info to identify most recent
files_info <- fs::dir_info("data_raw",
                           regexp = "nfa_updated")

# get most recent file only if multiple exists
(file_recent <- files_info %>%
    select(path, modification_time) %>%
    arrange(modification_time) %>%
    slice_max(order_by = modification_time))

# remove any other files
files_info %>% filter(path!=file_recent$path) %>%
  pull(path) %>%
  fs::file_delete()


# READ IN -----------------------------------------------------------------

# read in most recent
df <- read_csv(glue("{file_recent$path}"))

# clean AND RENAME
df_clean <- df %>%
  rename(date = Date) %>%
  mutate(year = year(date), month = month(date),
         water_year = year(date) + ifelse(month(date) >= 10, 1, 0),
         water_day = (date - as.Date(sprintf('%s-10-01', water_year)))) %>%
  group_by(water_year) %>%
  mutate(water_day = as.numeric(water_day - min(water_day) + 1)) %>%
  ungroup()


# SUMMARIZE  --------------------------------------------------------------

library(FlowScreen)

# select data of interest and make into time series
df_ts <- df_clean %>%
  select(site_no, date, Flow, Flow_cd, agency_cd) %>%
  as.data.frame() %>%
  # convert to cms
  mutate(Flow = Flow * 0.0283168,
         PARAM = 1) %>%
  # rename
  rename(Date=date, ID=site_no, SYM=Flow_cd, Agency=agency_cd) %>%
  select(ID, PARAM, Date, Flow, SYM, Agency) %>%
  create.ts()

# use pk.cov function to calc center of volume
df_cov <- pk.cov(df_ts)


# VISUALIZE ---------------------------------------------------------------

# center of volume plot
plot_cov <-
  ggplot() +
  geom_pointrange(data=df_cov, aes(x=hYear, y=Q50, ymin=Q25, ymax=Q75, fill=Q50),
                  pch=21, size=0.6,  alpha=0.8,
                  color="gray20", show.legend=FALSE) +
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = seq(1940, year(Sys.Date()),3)) +
  theme_bw(base_size = 10) +
  theme(axis.text.x = element_text(angle=330,
                                   hjust = 0.4,
                                   vjust = 0.1 )) +
  labs(x="Water Year", y="Center of Volume 50% \n(Day of year for tot ann streamflow)")

print(plot_cov)

ggsave(plot_cov, filename = "output/figure_center_of_volumne_Q25-Q75.png",
       dpi=200, width=10, height = 6.5)
