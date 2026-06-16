# Yolo LTE hydrology and water quality conditions

## Load libraries ----
library(tidyverse)
library(lubridate)
## Set variables ----
download_data <- T
saveplots <- F

cdec_stations <- c("YBY", "LIS", "RCS", "FWD", "PTC", "FRE", "CCY", "KNL")
  
sensor_codes <- data.frame(sensor = c("discharge_cfs", "stage_ft"), 
                           sensor_num = c(20, 1))
startdate <- "2025-10-1"
enddate <- "2026-9-30"

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
contpal <-   c("YBT" = RColorBrewer::brewer.pal(9, "Set1")[1], 
                       "RCS" = RColorBrewer::brewer.pal(9, "Set1")[7], 
                       "PTC" = RColorBrewer::brewer.pal(9, "Set1")[3], 
                       "YBY" = RColorBrewer::brewer.pal(9, "Set1")[4],
                       "FRE" = RColorBrewer::brewer.pal(9, "Set1")[2],
                       "FWD" = RColorBrewer::brewer.pal(9, "Set1")[9],
                       "CCY" = RColorBrewer::brewer.pal(9, "Set1")[5])
  
  scale_color_manual(values = c("YBT" = RColorBrewer::brewer.pal(9, "Set1")[1], 
                                          "RCS" = RColorBrewer::brewer.pal(9, "Set1")[7], 
                                          "PTC" = RColorBrewer::brewer.pal(9, "Set1")[3], 
                                          "YBY" = RColorBrewer::brewer.pal(9, "Set1")[4],
                                          "FRE" = RColorBrewer::brewer.pal(9, "Set1")[2],
                                          "FWD" = RColorBrewer::brewer.pal(9, "Set1")[9],
                                          "CCY" = RColorBrewer::brewer.pal(9, "Set1")[5]))

ggplot(cdec_wide[is.na(cdec_wide$discharge_cfs) == F & cdec_wide$Site_no %in% c("FRE", "PTC", "RCS", "CCY"),], 
       aes(x = Datetime, y = discharge_cfs, color = Site_no)) + 
  geom_line() + 
  theme_bw() +  scale_color_manual(values = contpal) +
  labs(title = "Discharge", y = "Discharge (cfs)", x = NULL)

(discharge_plot <- ggplot(cdec_wide[is.na(cdec_wide$discharge_cfs) == F & cdec_wide$Site_no %in% c("FRE", "PTC", "RCS", "CCY"),], 
       aes(x = Datetime, y = discharge_cfs, color = Site_no, fill = Site_no)) + 
  geom_ribbon(aes(ymax = discharge_cfs, ymin = 0), alpha = .2) + geom_line() + 
  theme_bw() +  scale_color_manual(values = contpal) + scale_fill_manual(values = contpal) +
  scale_x_datetime(limits = c(as.POSIXct("2025-10-01"), as.POSIXct("2026-05-01")) ) +
  labs(title = "Discharge", y = "Discharge (cfs)", x = NULL))

ggplot(cdec_wide[is.na(cdec_wide$discharge_cfs) == F,], 
       aes(x = Datetime, y = stage_ft, color = Site_no)) + 
  geom_line() + 
  theme_bw() +  scale_color_manual(values = contpal) +
  labs(title = "Discharge", y = "Discharge (cfs)", x = NULL)

## Calculate daily flow
cdec_wide$Date <- as.Date(cdec_wide$Datetime)

cdecply <- cdec_wide[!(cdec_wide$Site_no %in% c("LIS", "YBY")),] %>% 
  group_by(Site_no, Date) %>% 
  summarize(medianfcs = median(pmax(discharge_cfs, 0), na.rm = T)) %>% 
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
       aes(x = Date, y = percflow, fill = Site_no)) + geom_area() +
  theme_bw() +  scale_color_manual(values = contpal) +
  labs(title = "Discharge", y = "Discharge (cfs)", x = NULL)

cdecply$year <- format(cdecply$Date, format = "%Y")


ggplot(cdecply[is.na(cdecply$medianfcs) == F,], 
       aes(x = Date, y = medianfcs, color = Site_no)) + 
  geom_line() + 
  theme_bw() +  scale_color_manual(values = contpal) +
  labs(title = "Discharge", y = "Discharge (cfs)", x = NULL)

cdec_wide$year <- format(cdec_wide$Date, format = "%Y")

# Loop for all years of interest ------------------------------------------


print(paste0("Working on year: ", year))
cdec_wide <- cdec_wide[cdec_wide$Datetime > as.POSIXct(paste0(year - 1, "-10-01")) &  
                         cdec_wide$Datetime < as.POSIXct(paste0(year, "-06-01")),]

(discharge_plot <- ggplot(cdec_wide[is.na(cdec_wide$discharge_cfs) == F & cdec_wide$Site_no %in% c("FRE", "PTC", "RCS", "CCY"),], 
                          aes(x = Datetime, y = discharge_cfs, color = Site_no, fill = Site_no)) + 
    geom_ribbon(aes(ymax = discharge_cfs, ymin = 0), alpha = .2) + geom_line() + 
    theme_bw() +  scale_color_manual(values = contpal) + scale_fill_manual(values = contpal) +
    scale_x_datetime(limits = c(as.POSIXct("2025-10-01"), as.POSIXct("2026-05-01")),
                     date_breaks = "1 month", date_labels = "%b-%d") +
    labs(y = "Discharge (cfs)", x = NULL))

(discharge_plot_noclip <- discharge_plot + coord_cartesian(clip = "off",
                                                           ylim = c(0, max(cdec_wide[cdec_wide$Site_no %in% c("CCY", "RCS", "PTC"), "discharge_cfs"], na.rm = T))))

(perflowplot <- ggplot(cdecply[is.na(cdecply$medianfcs) == F,], 
                       aes(x = Date, y = percflow, fill = Site_no)) + 
    labs(x = NULL, y = "Percent Bypass Inflow") +
    geom_bar(stat = "identity", width = 1) + theme_bw() + ylim(0,100) +
    scale_color_manual(values = contpal) + scale_fill_manual(values = contpal) +
    scale_x_date(limits = c(as.Date("2025-10-01"),
                            as.Date("2026-05-01")),
                 date_breaks = "1 month", date_labels = "%b-%d"))


png("Output/Figures/YBLTE_Cont_flow_2026_%02d.png",
    height = 6.5, width = 6.5, units = "in", res = 1000, family = "serif")
print(cowplot::plot_grid(discharge_plot, perflowplot, ncol = 1, align = "v"))
dev.off()


# Create cummulative flow plots -------------------------------------------
## ============================================================
## 1. Assign water year
## ============================================================

cdec_wide_wy <- cdec_wide %>%
  mutate(
    wy = if_else(month(Date) >= 10, year(Date) + 1, year(Date))
  )

## ============================================================
## 2. Aggregate to daily discharge per tributary
## ============================================================

daily_flow <- cdec_wide_wy %>%
  group_by(Site_no, wy, Date) %>%
  summarize(
    med_cfs = median(discharge_cfs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    med_cfs = pmax(med_cfs, 0),   # fix negative flows
    jday = yday(Date)
  )

## ============================================================
## 3. Build complete grid of all dates × all tributaries
## ============================================================

all_days <- tibble(Date = seq(ymd("2025-10-01"), ymd("2026-05-01"), by = "1 day"))
all_sites <- tibble(Site_no = c("FRE","CCY","RCS","PTC"))

full_grid <- tidyr::crossing(all_days, all_sites)

## ============================================================
## 4. Join daily flow onto full grid and fill missing with zero
## ============================================================

daily_flow_full <- full_grid %>%
  left_join(
    daily_flow %>% filter(Site_no %in% c("FRE","CCY","RCS","PTC")),
    by = c("Date","Site_no")
  ) %>%
  mutate(
    med_cfs = pmax(replace_na(med_cfs, 0), 0),   # replace NA with 0
    wy = if_else(month(Date) >= 10, year(Date) + 1, year(Date))
  )

## ============================================================
## 5. Compute daily leader (now guaranteed 1 per day)
## ============================================================

daily_leaders <- daily_flow_full %>%
  group_by(Date) %>%
  mutate(
    max_flow = max(med_cfs, na.rm = TRUE),
    is_leader = med_cfs == max_flow,
    n_tied = sum(is_leader),
    leader_points = if_else(is_leader, 1 / n_tied, 0)
  ) %>%
  ungroup()

## ============================================================
## 6. Count days as leader
## ============================================================

leader_counts <- daily_leaders %>%
  group_by(Site_no) %>%
  summarize(days_as_leader = sum(leader_points), .groups = "drop")

## ============================================================
## 7. Recompute cumulative flow for plotting
## ============================================================

cum_flow_wy <- daily_flow_full[daily_flow_full$Date >= as.Date("2025-10-1") &
                                 daily_flow_full$Date <= as.Date("2026-5-1"),] %>%
  arrange(Site_no, wy, Date) %>%
  group_by(Site_no, wy) %>%
  mutate(cum_cfs = cumsum(med_cfs),
         cum_taf = cum_cfs * 0.00198347) %>%
  ungroup()

cum_flow_wy2 <- cum_flow_wy %>%
  left_join(leader_counts_wy, by = "Site_no")

## ============================================================
## 8. Label dataframe for facet annotations
## ============================================================

label_df <- cum_flow_wy2 %>%
  filter(Site_no %in% c("CCY", "FRE", "RCS", "PTC")) %>%
  group_by(wy, Site_no) %>%
  summarize(days_as_leader = first(days_as_leader),
            TAF = max(cum_taf),
            maxflow = max(med_cfs),
            x = min(Date) + 3)

label_df

## ============================================================
## 9. Plot 1 — Cumulative Flow by Water Year
## ============================================================

cum_flow_wy2 %>% group_by(Site_no) %>% summarize(TAF = sum(med_cfs) * 0.00198347)

(taf_plot <- ggplot(cum_flow_wy2[ cum_flow_wy2$Site_no %in% c("CCY","FRE","RCS","PTC"),], aes(Date, cum_taf, color = Site_no)) +
    geom_line(linewidth = 0.7) +
    scale_color_brewer(palette = "Set1") +
    theme_bw() + scale_color_manual(values = contpal) + 
    labs(x = NULL, y = "Cumulative Discharge (TAF)", color = "Tributary") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    geom_text(data = label_df,  size = 2.5, hjust = 0, show.legend = F,
              aes(x = x, y = TAF*1.01, color = Site_no,
                  label = paste0(Site_no, ": ", scales::comma(TAF, accuracy = 0.1), " TAF; ", days_as_leader, " days"))))

if(saveplots == T){png(paste0("Output/Figures/YBLTE_flow_panel_%02d.png"),
                       width = 6.5, height = 6, units = "in", res = 1000, family = "serif")}

print(cowplot::plot_grid(discharge_plot + theme(axis.text.x = element_blank()), 
                         perflowplot + theme(axis.text.x = element_blank()), 
                                            taf_plot, ncol = 1, align = "v"))


dev.off()
## ============================================================
## 11. Plot 3 — Days as Leader (bar plot)
## ============================================================

ggplot(leader_counts_wy, aes(x = Site_no, y = days_as_leader, fill = Site_no)) + 
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Days as Dominant Tributary (WY 2026)",
    x = "Tributary",
    y = "Days as Leader"
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
  ggrepel::geom_text_repel(
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
