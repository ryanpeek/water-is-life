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


# STATS -------------------------------------------------------------------


# use pk.cov function to calc center of volume
df_cov <- pk.cov(df_ts)

# which is basically this:
# tst <- df_clean %>% group_by(water_year) %>% summarize(totvol=cumsum(Flow)/sum(Flow)) %>% bind_cols(., df_clean[,c(10,3,5)])

# get WYT
wyt <- read_csv("data_raw/WYT_1906-2021.csv") %>%
  mutate(Sac_WY_type=factor(Sac_WY_type, levels=c("W","AN","BN","D","C")))

# join w data
df_cov <- left_join(df_cov, wyt %>% select(WY, Sac_WY_type), by=c("hYear"="WY"))

# look at drought severity:
df_drought <- FlowScreen::dr.seas(df_ts)
df_drought2 <- df_ts %>% filter(hyear>2019) %>% FlowScreen::dr.events()
df_drought$hyear <- year(attr(df_drought$StartDay, "times"))

# get mean by water year type for Center of Vol
mean_wy_cov <- df_cov %>% group_by(Sac_WY_type) %>% summarize(mean50=mean(Q50),
                                                              mean25=mean(Q25),
                                                              mean75=mean(Q75))

# get quantiles
(thresh <- df_ts %>% filter(hyear==2017) %>% group_by(hyear) %>% summarize(quants = quantile(Flow, 0.2, na.rm=TRUE)))


# VISUALIZE ---------------------------------------------------------------

# center of volume plot
(plot_cov <-
   ggplot() +
   geom_point(data=df_cov, aes(y=hYear, x=Q50, fill=Sac_WY_type),
              pch=21, size=3.5, alpha=0.9,
              color="gray20", show.legend=TRUE) +
   geom_vline(data=mean_wy_cov, aes(xintercept=mean50, color=Sac_WY_type), lty=2) +
   scale_fill_viridis_d("Water Year\n Type") +
   scale_color_viridis_d("Water Year\n Type") +
   scale_y_continuous(breaks = seq(1940, year(Sys.Date()),5)) +
   theme_bw(base_size = 10) +
   labs(y="Water Year", x="Center of Volume\n(Day of year for 50% of tot. ann streamflow)")
)

ggsave(plot_cov, filename = "output/figure_center_of_volume_Q50.png",
       dpi=200, width=10, height = 6.5)

# drought metrics
ggplot(data=df_drought) +
  geom_linerange(aes(xmax=EndDay, xmin=StartDay, y=hyear, color=Severity)) +
  geom_point(aes(x=StartDay, y=hyear), pch=21, size=2, fill="orange") +
  geom_point(aes(x=EndDay, y=hyear), pch=21, size=3, fill="maroon") +
  scale_color_viridis_c("Severity") +
  scale_y_continuous(breaks = c(seq(1941, 2021, 4))) +
  theme_classic() +
  labs(title="Start and end of significant droughts",
       subtitle="Severity of drought based on 30-day window using a flow duration curve (Beyene et al. 2014)")
# save
ggsave(plot_cov, filename = "output/figure_drought_periods.png",
       dpi=200, width=10, height = 6.5)
