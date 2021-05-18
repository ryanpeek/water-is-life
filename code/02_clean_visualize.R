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

# Get Water Year Types
wyt <- read_csv("data_raw/WYT_1906-2021.csv") %>%
  mutate(Sac_WY_type=factor(Sac_WY_type, levels=c("W","AN","BN","D","C")))


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

# join with water year:
df_ts <- left_join(df_ts, wyt %>% select(WY, Sac_WY_type), by=c("hyear"="WY"))


# STATS -------------------------------------------------------------------

# use pk.cov function to calc center of volume
df_cov <- pk.cov(df_ts)

# join w water year data
df_cov <- left_join(df_cov, wyt %>% select(WY, Sac_WY_type), by=c("hYear"="WY"))

# VISUALIZE ---------------------------------------------------------------

# plot all with current year:
p1<-ggplot(data=df_ts) +
  #geom_line(aes(x=hdoy, y=Flow, color="Sac_WY_type"), alpha=0.8, lwd=0.3) +
  geom_line(aes(x=hdoy, y=Flow, group=hyear), color="gray", alpha=0.8, lwd=0.3) +
  geom_line(data=df_ts %>% filter(hyear>=year(Sys.Date())),
            aes(x=hdoy, y=Flow), color="steelblue", lwd=2) +
  geom_point(data=df_ts %>% filter(hyear>=year(Sys.Date())) %>%
               slice_max(hdoy, n = 1),
             aes(x=hdoy, y=Flow), pch=21, size=4, fill="orange") +
  theme_classic() +
  labs(title=glue("Flow from USGS: {unique(df_clean$site_no)},
                  {min(df_clean$water_year)}-{max(df_clean$water_year)}"),
       caption=glue("Updated {Sys.Date()}"),
       x="Day of Water Year", y="Flow (cms)")
# save
ggsave(p1, filename = "output/figure_flow_spaghetti_plot_all.png",
       dpi=200, width=11, height = 8)


# Facet by wy type:
p2 <- ggplot(data=df_ts) +
  geom_ribbon(data=df_ts,
              aes(x=hdoy, ymax=Flow, ymin=0,
                  fill=Sac_WY_type, group=hyear),
              alpha=0.5, lwd=0.3, show.legend = FALSE) +
  # current year
  geom_line(data=df_ts %>% filter(hyear>=year(Sys.Date())),
            aes(x=hdoy, y=Flow), color="black", lwd=0.8) +
  ggrepel::geom_label_repel(data=df_ts %>% filter(hyear>=year(Sys.Date())) %>%
                          slice_max(hdoy, n = 1),
                        aes(x=hdoy, y=Flow, label="Current date"),
                        box.padding = 2.5, segment.alpha=0.2,
                        min.segment.length = 0.01, nudge_x = 5, nudge_y = 2)+
  geom_point(data=df_ts %>% filter(hyear>=year(Sys.Date())) %>%
               slice_max(hdoy, n = 1),
             aes(x=hdoy, y=Flow), pch=21, size=4, fill="orange") +
  theme_classic() +
  scale_fill_viridis_d("Water \nYear Type")+
  scale_color_viridis_d("Water \nYear Type")+
  labs(title=glue("Flow from USGS: {unique(df_clean$site_no)}"),
       subtitle = glue("{min(df_clean$water_year)}-{max(df_clean$water_year)}"),
       caption=glue("Updated {Sys.Date()}"),
       x="Day of Water Year", y="Flow (cms)") +
  facet_wrap(vars(Sac_WY_type), scales = "free_y")

ggsave(p2, filename = "output/figure_flow_ribbon_wy_facet.png",
       dpi=200, width=11, height = 8)


# Compare within same wy type:
p3 <- df_ts %>% filter(Sac_WY_type=="C") %>%
  ggplot() +
  geom_ribbon(aes(x=hdoy, ymax=Flow, ymin=0,
                  group=hyear), fill="#FDE725FF",
              alpha=0.5, lwd=0.3, show.legend = FALSE) +
  geom_line(aes(x=hdoy, y=Flow, group=hyear), color="gray40",
            alpha=0.4, lwd=0.3, show.legend=FALSE) +
  # current year:
  geom_line(data=df_ts %>% filter(hyear>=year(Sys.Date())),
            aes(x=hdoy, y=Flow), color="black", lwd=0.9) +
  ggrepel::geom_label_repel(data=df_ts %>% filter(hyear>=year(Sys.Date())) %>%
                              slice_max(hdoy, n = 1),
                            aes(x=hdoy, y=Flow, label="Current date"),
                            box.padding = 2.5, segment.alpha=0.5,
                            min.segment.length = 0.01, nudge_x = 5, nudge_y = 2) +
  geom_point(data=df_ts %>% filter(hyear>=year(Sys.Date())) %>%
               slice_max(hdoy, n = 1),
             aes(x=hdoy, y=Flow), pch=21, size=4, fill="orange") +
  theme_classic() +
  scale_color_viridis_d("Water \nYear Type") +
  scale_fill_viridis_d("Water \nYear Type") +
  labs(title=glue("Flow from USGS: {unique(df_clean$site_no)}"),
       subtitle = glue("Only Critically Dry Years: {min(df_clean$water_year)}-{max(df_clean$water_year)}"),
       caption=glue("Updated {Sys.Date()}"),
       x="Day of Water Year", y="Flow (cms)")

ggsave(p3, filename = "output/figure_flow_ribbon_wy_just_current.png",
       dpi=200, width=11, height = 8)
