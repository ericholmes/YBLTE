# Yolo LTE hydrology and water quality conditions

## Load libraries ----
library(tidyverse)

## Set variables ----
download_data <- T
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

#2017

(ybyplot <- ggplot(cdec_wide[cdec_wide$Site_no == "YBY",], 
       aes(x = Datetime, y = discharge_cfs)) + geom_line() + theme_bw() +
  xlim(as.POSIXct("2017-01-01"), as.POSIXct("2017-07-01")))

(perflowplot <- ggplot(cdecply[is.na(cdecply$medianfcs) == F & cdecply$year == 2017,], 
       aes(x = Date, y = percflow, fill = Site_no)) + 
  geom_bar(stat = "identity") + theme_bw() + ylim(0,100) +
  xlim(as.Date("2017-01-01"), as.Date("2017-07-01")))



(chlplot <- ggplot(lischlply[lischlply$medchl < 100,], aes(x = Date, y = medchl)) + 
  geom_line(color = "forestgreen") + theme_bw() +
    xlim(as.Date("2017-01-01"), as.Date("2017-07-01")))

(ecplot <- ggplot(lisecply, aes(x = Date, y = medec)) + 
  geom_line(color = "orange") + theme_bw() +
    xlim(as.Date("2017-01-01"), as.Date("2017-07-01")))

if(saveplots == T){png("Output/Figures/YBLTE_Cont_flow2017_%02d.png",
    height = 10, width = 6, units = "in", res = 1000, family = "serif")}
cowplot::plot_grid(ybyplot, perflowplot, chlplot, ecplot, ncol = 1, align = "v")
if(saveplots == T){dev.off()}



#2016

(ybyplot <- ggplot(cdec_wide[cdec_wide$Site_no == "YBY" & cdec_wide$year %in% 2016,], 
                   aes(x = Datetime, y = discharge_cfs)) + geom_line() + theme_bw() +
    xlim(as.POSIXct("2016-01-01"), as.POSIXct("2016-07-01")))

(perflowplot <- ggplot(cdecply[is.na(cdecply$medianfcs) == F & cdecply$year == 2016,], 
                       aes(x = Date, y = percflow, fill = Site_no)) + 
    geom_bar(stat = "identity") + theme_bw() + ylim(0,100) +
    xlim(as.Date("2016-01-01"), as.Date("2016-07-01")))

(chlplot <- ggplot(lischlply[lischlply$medchl < 100,], aes(x = Date, y = medchl)) + 
    geom_line(color = "forestgreen") + theme_bw() +
    xlim(as.Date("2016-01-01"), as.Date("2016-07-01")))

(ecplot <- ggplot(lisecply, aes(x = Date, y = medec)) + 
    geom_line(color = "orange") + theme_bw() +
    xlim(as.Date("2016-01-01"), as.Date("2016-07-01")))

if(saveplots == T){png("Output/Figures/YBLTE_Cont_flow2016_%02d.png",
    height = 10, width = 6, units = "in", res = 1000, family = "serif")}
cowplot::plot_grid(ybyplot, perflowplot, chlplot, ecplot, ncol = 1, align = "v")
if(saveplots == T){dev.off()}

#2018

(ybyplot <- ggplot(cdec_wide[cdec_wide$Site_no == "YBY" & cdec_wide$year %in% 2018,], 
                   aes(x = Datetime, y = discharge_cfs)) + geom_line() + theme_bw() +
    xlim(as.POSIXct("2018-01-01"), as.POSIXct("2018-07-01")))

(perflowplot <- ggplot(cdecply[is.na(cdecply$medianfcs) == F & cdecply$year == 2018,], 
                       aes(x = Date, y = percflow, fill = Site_no)) + 
    geom_bar(stat = "identity") + theme_bw() + ylim(0,100) +
    xlim(as.Date("2018-01-01"), as.Date("2018-07-01")))

(chlplot <- ggplot(lischlply[lischlply$medchl < 100,], aes(x = Date, y = medchl)) + 
    geom_line(color = "forestgreen") + theme_bw() +
    xlim(as.Date("2018-01-01"), as.Date("2018-07-01")))

(ecplot <- ggplot(lisecply, aes(x = Date, y = medec)) + 
    geom_line(color = "orange") + theme_bw() +
    xlim(as.Date("2018-01-01"), as.Date("2018-07-01")))

if(saveplots == T){png("Output/Figures/YBLTE_Cont_flow2018_%02d.png",
    height = 10, width = 6, units = "in", res = 1000, family = "serif")}
cowplot::plot_grid(ybyplot, perflowplot, chlplot, ecplot, ncol = 1, align = "v")
if(saveplots == T){dev.off()}

#2019

(ybyplot <- ggplot(cdec_wide[cdec_wide$Site_no == "YBY" & cdec_wide$year %in% 2019,], 
                   aes(x = Datetime, y = discharge_cfs)) + geom_line() + theme_bw() +
    xlim(as.POSIXct("2019-01-01"), as.POSIXct("2019-07-01")))

(perflowplot <- ggplot(cdecply[is.na(cdecply$medianfcs) == F & cdecply$year == 2019,], 
                       aes(x = Date, y = percflow, fill = Site_no)) + 
    geom_bar(stat = "identity") + theme_bw() + ylim(0,100) +
    xlim(as.Date("2019-01-01"), as.Date("2019-07-01")))

(chlplot <- ggplot(lischlply[lischlply$medchl < 100,], aes(x = Date, y = medchl)) + 
    geom_line(color = "forestgreen") + theme_bw() +
    xlim(as.Date("2019-01-01"), as.Date("2019-07-01")))

(ecplot <- ggplot(lisecply, aes(x = Date, y = medec)) + 
    geom_line(color = "orange") + theme_bw() +
    xlim(as.Date("2019-01-01"), as.Date("2019-07-01")))

if(saveplots == T){png("Output/Figures/YBLTE_Cont_flow2019_%02d.png",
    height = 10, width = 6, units = "in", res = 1000, family = "serif")}
cowplot::plot_grid(ybyplot, perflowplot, chlplot, ecplot, ncol = 1, align = "v")
if(saveplots == T){dev.off()}

#2023

(ybyplot <- ggplot(cdec_wide[cdec_wide$Site_no == "YBY" & cdec_wide$year %in% 2023,], 
                   aes(x = Datetime, y = discharge_cfs)) + geom_line() + theme_bw() +
    xlim(as.POSIXct("2023-01-01"), as.POSIXct("2023-07-01")))

(perflowplot <- ggplot(cdecply[is.na(cdecply$medianfcs) == F & cdecply$year == 2023,], 
                       aes(x = Date, y = percflow, fill = Site_no)) + 
    geom_bar(stat = "identity") + theme_bw() + ylim(0,100) +
    xlim(as.Date("2023-01-01"), as.Date("2023-07-01")))

(chlplot <- ggplot(lischlply[lischlply$medchl < 100,], aes(x = Date, y = medchl)) + 
    geom_line(color = "forestgreen") + theme_bw() +
    xlim(as.Date("2023-01-01"), as.Date("2023-07-01")))

(ecplot <- ggplot(lisecply, aes(x = Date, y = medec)) + 
    geom_line(color = "orange") + theme_bw() +
    xlim(as.Date("2023-01-01"), as.Date("2023-07-01")))

if(saveplots == T){png("Output/Figures/YBLTE_Cont_flow2023_%02d.png",
    height = 10, width = 6, units = "in", res = 1000, family = "serif")}
cowplot::plot_grid(ybyplot, perflowplot, chlplot, ecplot, ncol = 1, align = "v")
if(saveplots == T){dev.off()}

#2025

(ybyplot <- ggplot(cdec_wide[cdec_wide$Site_no == "YBY" & cdec_wide$year %in% 2025,], 
                   aes(x = Datetime, y = discharge_cfs)) + geom_line() + theme_bw() +
    xlim(as.POSIXct("2025-01-01"), as.POSIXct("2025-07-01")))

(perflowplot <- ggplot(cdecply[is.na(cdecply$medianfcs) == F & cdecply$year == 2025,], 
                       aes(x = Date, y = percflow, fill = Site_no)) + 
    geom_bar(stat = "identity") + theme_bw() + ylim(0,100) +
    xlim(as.Date("2025-01-01"), as.Date("2025-07-01")))

(chlplot <- ggplot(lischlply[lischlply$medchl < 100,], aes(x = Date, y = medchl)) + 
    geom_line(color = "forestgreen") + theme_bw() +
    xlim(as.Date("2025-01-01"), as.Date("2025-07-01")))

(ecplot <- ggplot(lisecply, aes(x = Date, y = medec)) + 
    geom_line(color = "orange") + theme_bw() +
    xlim(as.Date("2025-01-01"), as.Date("2025-07-01")))

if(saveplots == T){png("Output/Figures/YBLTE_Cont_flow2025_%02d.png",
                       height = 10, width = 6, units = "in", res = 1000, family = "serif")}
cowplot::plot_grid(ybyplot, perflowplot, chlplot, ecplot, ncol = 1, align = "v")
if(saveplots == T){dev.off()}

#2026
cdec_wide2026 <- cdec_wide[cdec_wide$Datetime > as.POSIXct("2025-10-01") &  
                               cdec_wide$Datetime < as.POSIXct("2026-05-01"),]

(ybyplot <- ggplot(cdec_wide2026[cdec_wide2026$Site_no %in% c("CCY", "RCS", "FRE", "PTC"),], 
                   aes(x = Datetime, y = discharge_cfs, color = Site_no)) + 
    geom_line() + theme_bw() + 
    scale_color_brewer(palette = "Set1") +
    xlim(as.POSIXct("2025-10-01"), as.POSIXct("2026-05-01")))

(perflowplot <- ggplot(cdecply[is.na(cdecply$medianfcs) == F & cdecply$year %in% 2025:2026,], 
                       aes(x = Date, y = percflow, fill = Site_no)) + 
    scale_fill_brewer(palette = "Set1") +
    geom_bar(stat = "identity") + theme_bw() + ylim(0,100) +
    xlim(as.Date("2025-10-01"), as.Date("2026-05-01")))

(chlplot <- ggplot(lischlply[lischlply$medchl < 100,], aes(x = Date, y = medchl)) + 
    geom_line(color = "forestgreen") + theme_bw() +
    xlim(as.Date("2025-10-01"), as.Date("2026-05-01")))

(ecplot <- ggplot(lisecply, aes(x = Date, y = medec)) + 
    geom_line(color = "orange") + theme_bw() +
    xlim(as.Date("2025-10-01"), as.Date("2026-05-01")))

if(saveplots == T){png("Output/Figures/YBLTE_Cont_flow2026_%02d.png",
                       height = 10, width = 6, units = "in", res = 1000, family = "serif")}
cowplot::plot_grid(ybyplot, perflowplot, chlplot, ecplot, ncol = 1, align = "v")
if(saveplots == T){dev.off()}
