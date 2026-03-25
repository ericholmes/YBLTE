# Yolo LTE hydrology and water quality conditions

## Load libraries ----
library(tidyverse)
library(lubridate)
## Set variables ----
download_data <- F
saveplots <- F

cdec_stations <- c("YBY", "LIS", "RCS", "FWD", "PTC", "FRE", "CCY", "KNL")
  
sensor_codes <- data.frame(sensor = c("discharge_cfs", "stage_ft"), 
                           sensor_num = c(20, 1))
startdate <- "2015-10-1"
enddate <- Sys.Date()

## Load functions ----
source("Code/YBLTE_useful_functions.R")

##Download gage data ----
### CDEC ----
if(download_data == T){
  cdec <- data.frame()
  for(station in cdec_stations){
    for(param in sensor_codes$sensor_num){
      print(paste("downloading:", station, param))
      try(cdec <- rbind(cdec, downloadCDEC(site_no = station, parameterCd = param, startDT = startdate , endDT = enddate)))
    }
  }
}else(load("Data/cdec_flow_data.Rdata"))

##save(cdec, file = "Data/cdec_flow_data.Rdata")

lischl <- downloadCDEC(site_no = "LIS", parameterCd = 28, startDT = startdate, endDT = enddate)

lisec <- downloadCDEC(site_no = "LIS", parameterCd = 100, startDT = startdate, endDT = enddate)

##save(lischl, lisec, file = "Data/cdec_lis_chl+ec.Rdata")
# load(file = "Data/cdec_lis_chl+ec.Rdata")
lischl$Param_val <- as.numeric(lischl$Param_val)
lisec$Param_val <- as.numeric(lisec$Param_val)

cdec$Param_val <- as.numeric(cdec$Param_val)

cdecmerge <- merge(cdec, sensor_codes, by.x = "parameterCd", by.y = "sensor_num")

# Pivoting from long to wide
cdec_wide <- cdecmerge %>% select(-parameterCd) %>% pivot_wider(names_from = sensor, values_from = Param_val)

## This may be incorrect, but stages below 14ft at FWD are forced to 14

cdec_wide$stage_ft <- ifelse(cdec_wide$Site_no == "FWD",
                             ifelse(cdec_wide$stage_ft <= 14, 14, cdec_wide$stage_ft),
                             cdec_wide$stage_ft)

cdec_wide$discharge_cfs <- ifelse(cdec_wide$Site_no == "FRE",
                             ifelse(is.na(cdec_wide$discharge_cfs), 0, cdec_wide$discharge_cfs),
                             cdec_wide$discharge_cfs)

#dput(RColorBrewer::brewer.pal(9, "Set1"))
c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999") 

###Continuous gauge data
unique(cdec$Site_no)
contpal <-  scale_color_manual(values = c("YBT" = RColorBrewer::brewer.pal(9, "Set1")[1], 
                                          "RCS" = RColorBrewer::brewer.pal(9, "Set1")[5], 
                                          "PTC" = RColorBrewer::brewer.pal(9, "Set1")[2], 
                                          "YBY" = RColorBrewer::brewer.pal(9, "Set1")[4],
                                          "FRE" = RColorBrewer::brewer.pal(9, "Set1")[3],
                                          "FWD" = RColorBrewer::brewer.pal(9, "Set1")[9]))

ggplot(cdec_wide[is.na(cdec_wide$discharge_cfs) == F,], 
       aes(x = Datetime, y = discharge_cfs, color = Site_no)) + 
  geom_line() + 
  theme_bw() +  contpal +
  labs(title = "Discharge", y = "Discharge (cfs)", x = NULL)

ggplot(cdec_wide[is.na(cdec_wide$discharge_cfs) == F,], 
       aes(x = Datetime, y = stage_ft, color = Site_no)) + 
  geom_line() + 
  theme_bw() +  contpal +
  labs(title = "Discharge", y = "Discharge (cfs)", x = NULL)

ggplot(cdec_wide[is.na(cdec_wide$discharge_cfs) == F,], 
       aes(x = Datetime, y = discharge_cfs, color = Site_no)) + 
  geom_line() + 
  theme_bw() +  contpal + scale_y_log10() +
  labs(title = "Discharge", y = "Discharge (cfs)", x = NULL)

## Calculate daily flow
cdec_wide$Date <- as.Date(cdec_wide$Datetime)

cdecply <- cdec_wide[!(cdec_wide$Site_no %in% c("LIS", "YBY")),] %>% 
  group_by(Site_no, Date) %>% 
  summarize(medianfcs = median(discharge_cfs, na.rm = T)) %>% 
  group_by(Date) %>% 
  mutate(sumflow = sum(medianfcs, na.rm = T), percflow = 100*medianfcs/sumflow) %>% 
  data.frame()

cdecply$Trib <- ifelse(cdecply$Site_no == "FRE", "Sac", "Tribs")

cdectribply <- cdecply %>% group_by(Trib, Date) %>% 
  summarize(mediancfs = median(medianfcs, na.rm = T), sumperc = sum(percflow, na.rm = T)) 

(pertribflowplot <- ggplot(cdectribply, 
                       aes(x = Date, y = sumperc, fill = Trib)) + 
    geom_bar(stat = "identity") + theme_bw() + ylim(0,100))

(pertribflowhistplot <- ggplot(cdectribply, 
                           aes(x = sumperc, fill = Trib)) + 
    geom_histogram() + theme_bw())

# save(cdectribply, file = "Data/YBLTE_percent_flow.Rdata")

ggplot(cdecply[is.na(cdecply$medianfcs) == F,], 
       aes(x = Date, y = percflow, color = Site_no)) + 
  geom_line() + 
  theme_bw() +  contpal +
  labs(title = "Discharge", y = "Discharge (cfs)", x = NULL)

cdecply$year <- format(cdecply$Date, format = "%Y")

lischl$Date <- as.Date(lischl$Datetime)
lischlply <- lischl %>% 
  group_by(Date) %>% summarize(medchl = median(Param_val, na.rm = T))

lisec$Date <- as.Date(lisec$Datetime)
lisecply <- lisec %>% group_by(Date) %>% summarize(medec = median(Param_val, na.rm = T))

ggplot(cdecply[is.na(cdecply$medianfcs) == F,], 
       aes(x = Date, y = medianfcs, color = Site_no)) + 
  geom_line() + 
  theme_bw() +  contpal +
  labs(title = "Discharge", y = "Discharge (cfs)", x = NULL)

cdec_wide$year <- format(cdec_wide$Date, format = "%Y")

# Loop for all years of interest ------------------------------------------

if(saveplots == T){
  for(year in 2016:2026){
    print(paste0("Working on year: ", year))
    cdec_wide_temp <- cdec_wide[cdec_wide$Datetime > as.POSIXct(paste0(year - 1, "-10-01")) &  
                                  cdec_wide$Datetime < as.POSIXct(paste0(year, "-06-01")),]
    
    ybyplot <- ggplot(cdec_wide_temp[cdec_wide_temp$Site_no %in% c("CCY", "RCS", "PTC", "FRE"),], 
                      aes(x = Datetime, y = discharge_cfs, color = Site_no)) + 
      geom_ribbon(data = cdec_wide_temp[cdec_wide_temp$Site_no %in% c("FRE"),],
                aes(ymax = discharge_cfs, ymin = 0), color = NA, fill = "slateblue4", alpha = .2) + 
      geom_line() + theme_bw() + labs(title = paste("WY", year), x = NULL) +
      scale_color_brewer(palette = "Set1") +
      scale_x_datetime(limits = c(
          as.POSIXct(paste0(year - 1, "-10-01")),
          as.POSIXct(paste0(year, "-06-01"))),
        date_breaks = "1 month",
        date_labels = "%b-%d"
      ) +
      coord_cartesian(clip = "off",
        ylim = c(0, max(cdec_wide_temp[cdec_wide_temp$Site_no %in% c("CCY", "RCS", "PTC"), "discharge_cfs"], na.rm = T)))
    
    perflowplot <- ggplot(cdecply[is.na(cdecply$medianfcs) == F & cdecply$year %in% (year - 1):year,], 
                          aes(x = Date, y = percflow, fill = Site_no)) + 
      scale_fill_brewer(palette = "Set1") + labs(x = NULL) +
      geom_bar(stat = "identity") + theme_bw() + ylim(0,100) +
      scale_x_date(limits = c(as.Date(paste0(year - 1, "-10-01")),
                              as.Date(paste0(year, "-06-01"))),
                   date_breaks = "1 month", date_labels = "%b-%d")
    
    chlplot <- ggplot(lischlply[lischlply$medchl < 100,], aes(x = Date, y = medchl)) + 
      geom_line(color = "forestgreen") + theme_bw() + labs(x = NULL) +
      scale_x_date(limits = c(as.Date(paste0(year - 1, "-10-01")),
                              as.Date(paste0(year, "-06-01"))),
                   date_breaks = "1 month", date_labels = "%b-%d")
    
    ecplot <- ggplot(lisecply, aes(x = Date, y = medec)) + 
      geom_line(color = "orange") + theme_bw() +
      scale_x_date(limits = c(as.Date(paste0(year - 1, "-10-01")),
                              as.Date(paste0(year, "-06-01"))),
                   date_breaks = "1 month", date_labels = "%b-%d")
    
    png(paste0("Output/Figures/YBLTE_contflow/YBLTE_Cont_flow", year,"_%02d.png"),
        height = 10, width = 6, units = "in", res = 1000, family = "serif")
    print(cowplot::plot_grid(ybyplot, perflowplot, chlplot, ecplot, ncol = 1, align = "v"))
    dev.off()
  }
}


# Create cummulative flow plots -------------------------------------------

# 1. Assign water year
cdec_wide_wy <- cdec_wide %>%
  mutate(
    wy = if_else(month(Date) >= 10, year(Date) + 1, year(Date))
  )

# 2. Aggregate to daily discharge per tributary per water year
daily_flow <- cdec_wide_wy %>%
  group_by(Site_no, wy, Date) %>%
  summarize(
    daily_cfs = sum(discharge_cfs, na.rm = TRUE),
    .groups = "drop"
  ) %>% mutate(jday = as.integer(format(Date, format = "%j")))



# 3. Compute cumulative discharge within each water year
cum_flow_wy <- daily_flow %>%
  arrange(Site_no, wy, Date) %>%
  group_by(Site_no, wy) %>%
  mutate(
    cum_cfs = cumsum(daily_cfs)
  ) %>%
  ungroup()

daily_leaders <- daily_flow %>%
  group_by(Date) %>%
  filter(daily_cfs == max(daily_cfs, na.rm = TRUE),
         Site_no %in% c("FRE", "CCY", "RCS")) %>%
  ungroup()

leader_counts <- daily_leaders %>%
  count(Site_no, name = "days_as_leader")

leader_counts_wy <- daily_flow %>%
  group_by(Date) %>%
  filter(daily_cfs == max(daily_cfs, na.rm = TRUE),
         jday %in% c(330:366, 0:150),
         Site_no %in% c("FRE", "CCY", "RCS")) %>%
  ungroup() %>%
  count(wy, Site_no, name = "days_as_leader")

seasonal_cumflow <- daily_flow %>%
  filter(jday %in% c(330:366, 0:150),
         Site_no %in% c("FRE", "CCY", "RCS")) %>%
  group_by(wy, Site_no) %>%
  summarize(
    total_cum_flow = sum(daily_cfs, na.rm = TRUE),
    .groups = "drop"
  )

leader_flow_compare <- leader_counts_wy %>%
  left_join(seasonal_cumflow, by = c("wy", "Site_no"))

wy_type_table <- tibble(
  wy = 2016:2026,
  wy_type = c(
    "Below Normal",  # 2016
    "Wet",           # 2017
    "Below Normal",  # 2018
    "Wet",           # 2019
    "Dry",           # 2020
    "Critical",      # 2021
    "Dry",           # 2022
    "Wet",           # 2023
    "Above Normal",  # 2024
    "Above Normal",  # 2025
    "Below Normal")  # 2026
)

leader_flow_compare <- leader_flow_compare %>%
  left_join(wy_type_table, by = "wy")
dput(unique(leader_flow_compare$wy_type))
leader_flow_compare$wy_type <- factor(leader_flow_compare$wy_type, levels = c("Wet", "Above Normal", "Below Normal", "Dry", "Critical"))

cum_flow_wy2 <- cum_flow_wy %>%
  left_join(leader_counts_wy, by = c("Site_no", "wy"))

label_df <- cum_flow_wy2 %>%
  filter(Site_no %in% c("CCY", "FRE", "RCS")) %>%
  group_by(wy, Site_no) %>%
  summarize(
    days_as_leader = first(days_as_leader),
    x = min(Date) + 5,        # left side of facet
    y = max(cum_cfs/1000)*0.95,  # near top of facet
    .groups = "drop"
  )

# 4. Plot cumulative flow by water year
if(saveplots == T){png(paste0("Output/Figures/YBLTE_contflow/YBLTE_Cumm_flow_all_years_%02d.png"),
    width = 10, height = 6, units = "in", res = 1000, family = "serif")}
ggplot(cum_flow_wy2[cum_flow_wy2$Site_no %in% c("CCY", "FRE", "RCS", "PTC"),], 
       aes(Date, cum_cfs/1000, color = Site_no)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ wy, scales = "free") +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  labs(title = "Cumulative Flow by Water Year",
       x = "Date",
       y = "Cumulative Discharge (1kcfs-days)",
       color = "Tributary") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  # Add leader-day labels
  geom_text_repel(
    data = label_df, 
    aes(x = x, y = y,
        label = paste0(Site_no, ": ", days_as_leader, " days")),
    color = "black",
    size = 2.5,
    hjust = 0
  )

ggplot(cum_flow_wy[cum_flow_wy$Site_no %in% c("CCY", "RCS", "PTC"),], 
       aes(Date, cum_cfs/1000, color = Site_no)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ wy, scales = "free_x") +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  labs(title = "Cumulative Flow by Water Year",
       x = "Date",
       y = "Cumulative Discharge (1kcfs-days)",
       color = "Tributary") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b")

ggplot(leader_counts_wy, aes(x = Site_no, y = days_as_leader, fill = Site_no)) + 
  geom_bar(stat = "identity") + facet_wrap(wy ~ .) + theme_bw() +
  scale_fill_brewer(palette = "Set1")

ggplot(leader_flow_compare, aes(x = Site_no, y = total_cum_flow, fill = Site_no)) + 
  geom_bar(stat = "identity") + facet_wrap(wy ~ .) + theme_bw() +
  scale_fill_brewer(palette = "Set1")

ggplot(leader_flow_compare, aes(x = days_as_leader, y = total_cum_flow, shape = Site_no)) + 
  geom_point() + theme_bw() + scale_y_sqrt() +
  scale_color_brewer(palette = "Set1") + 
  geom_text_repel(data = leader_flow_compare[leader_flow_compare$Site_no == "RCS",], 
                  aes(label = wy, color = wy_type), show.legend = F) +
  labs(title = "Cummulative flow versus days as primary tributary", 
       x = "Days as Dominant Tributary",
       y = "Cumulative Flow (cfs sq. root transformed)") +
  geom_path(aes(group = wy, color = wy_type),
    linewidth = 0.8,
    alpha = 0.4)
  

if(saveplots == T){dev.off()}
