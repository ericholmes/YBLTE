# Yolo LTE hydrology and water quality conditions

## Load libraries ----
library(tidyverse)

## Load functions ----
source("Code/YBLTE_useful_functions.R")

## Load point wq data

wqp <- readxl::read_excel("Data/tabular/YBLTE_point_wq.xlsx")

wqp$Sitefac <- factor(wqp$Site, levels = c("FWBN", "FW1", "RD22", "I80", "AL0", 
                                           "AL1", "LIS", "STTD", "TEW", "TER",
                                           "KNG3", "KLWW"))

wqp$week <- as.integer(format(wqp$Date, format = "%W")) - 43
wqp$weekchr <- as.character(wqp$week)

wqp$fdom_qsu <- as.numeric(wqp$fdom_qsu)

# wqp <- wqp[wqp$week > 0,]

##Download gage data ----
### CDEC ----

cdec_stations <- c("YBT", "YBY", "LIS", "RCS", "FWD", "PTC", "KNL", "I80")

sensor_codes <- data.frame(sensor = c("chla", "ec", "discharge_cfs", "fdom", 
                                      "wtemp_f", "domgl","ph", "turb_fnu",
                                      "stage_ft"), 
                           sensor_num = c(28, 100, 20, 266, 
                                          25, 61, 62, 221,
                                          1))
startdate <- "2025-10-1"
enddate <- Sys.Date()

cdec <- data.frame()
for(station in cdec_stations){
  for(param in sensor_codes$sensor_num){
    print(paste("downloading:", station, param))
    try(cdec <- rbind(cdec, downloadCDEC(site_no = station, parameterCd = param, startDT = startdate , endDT = enddate)))
  }
}

cdec$Param_val <- as.numeric(cdec$Param_val)

cdecmerge <- merge(cdec, sensor_codes, by.x = "parameterCd", by.y = "sensor_num")

# Pivoting from long to wide
cdec_wide <- cdecmerge %>% select(-parameterCd) %>% 
  pivot_wider(names_from = sensor, values_from = Param_val)

cdec_wide$wtemp_c <- (cdec_wide$wtemp_f - 32) * 5 / 9

## This may be incorrect, but stages below 14ft at FWD are forced to 14

cdec_wide$stage_ft <- ifelse(cdec_wide$Site_no == "FWD",
                             ifelse(cdec_wide$stage_ft <= 14, 14, cdec_wide$stage_ft),
                             cdec_wide$stage_ft)

#This may be inaccurate...It looks like it is already in SPC, so maybe the formula is backwards
cdec_wide$spc <- cdec_wide$ec / (1 + 0.02 * (cdec_wide$wtemp_c - 25))

## Preliminary plotting ----
dput(unique(wqp$Site))

###Spot measurements

(tempplot <- ggplot(wqp, aes(x = Sitefac, y = Temp, color = weekchr)) + 
  geom_point(stat = "identity") + labs(x = NULL, color = "Week") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_color_viridis_d(option = "C", begin = .2, end = .8))

(doplot <- ggplot(wqp, aes(x = Sitefac, y = DO_mgl, color = weekchr)) + 
  geom_point(stat = "identity") + labs(x = NULL, color = "Week") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_color_viridis_d(option = "C", begin = .2, end = .8))

(spcplot <- ggplot(wqp, aes(x = Sitefac, y = SPC_uscm, color = weekchr)) + 
  geom_point(stat = "identity") + labs(x = NULL, color = "Week") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_color_viridis_d(option = "C", begin = .2, end = .8))

(turbplot <- ggplot(wqp, aes(x = Sitefac, y = Turb_fnu, color = weekchr)) + 
  geom_point(stat = "identity") + labs(x = NULL, color = "Week") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_color_viridis_d(option = "C", begin = .2, end = .8))

(chlplot <- ggplot(wqp, aes(x = Sitefac, y = CHL_ugl, color = weekchr)) + 
  geom_point(stat = "identity") + labs(x = NULL, color = "Week") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1),
                     legend.position = "bottom") + 
  scale_color_viridis_d(option = "C", begin = .2, end = .8))

(fdomplot <- ggplot(wqp, aes(x = Sitefac, y = fdom_qsu, color = weekchr)) + 
    geom_point(stat = "identity") + labs(x = NULL, color = "Week") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    scale_color_viridis_d(option = "C", begin = .2, end = .8))

png("Output/Figures/YBLTE_Point_wq_%02d.png",
    height = 7, width = 6, units = "in", res = 1000, family = "serif")

cowplot::plot_grid(cowplot::plot_grid(tempplot + theme(legend.position = "none"),
                   doplot+ theme(legend.position = "none"),
                   spcplot+ theme(legend.position = "none"),
                   turbplot+ theme(legend.position = "none"),
                   chlplot+ theme(legend.position = "none"),
                   fdomplot+ theme(legend.position = "none"),nrow = 3),
                   cowplot::get_plot_component(chlplot, 'guide-box-bottom', return_all = TRUE),
                   nrow = 2, rel_heights = c(9,1))

dev.off()

#dput(RColorBrewer::brewer.pal(9, "Set1"))
c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999") 

###Continuous gauge data

contpal <-  scale_color_manual(values = c("LIS" = RColorBrewer::brewer.pal(8, "Set1")[1], 
                                          "RCS" = RColorBrewer::brewer.pal(8, "Set1")[5], 
                                          "PTC" = RColorBrewer::brewer.pal(8, "Set1")[2], 
                                          "YBY" = RColorBrewer::brewer.pal(8, "Set1")[4],
                                          "YBT" = RColorBrewer::brewer.pal(8, "Set1")[3],
                                          "FWD" = RColorBrewer::brewer.pal(8, "Set1")[9]))

(contspcplot <- ggplot(cdec_wide[is.na(cdec_wide$ec) == F,], aes(x = Datetime, y = ec, color = Site_no)) + 
  geom_line() + 
  theme_bw() +  contpal +
    labs(title = "Conductivity", y = "EC (US/cm)", x = NULL) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = SPC_uscm), color = "black", size = 3) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = SPC_uscm, color = Site)))

ggplot(cdec_wide[is.na(cdec_wide$spc) == F,], aes(x = Datetime, y = spc, color = Site_no)) + 
  geom_line(alpha = .2) + geom_line(stat = "smooth", method = "loess", span = .2, linewidth = 1) + 
  theme_bw() + labs(title = "Specific Conductivity", y = "SPC (US/cm)", x = NULL)+  contpal +
  geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = SPC_uscm, color = Site))

(contdoplot <- ggplot(cdec_wide[is.na(cdec_wide$domgl) == F,], aes(x = Datetime, y = domgl, color = Site_no)) + 
  geom_line(alpha = .2) + geom_line(stat = "smooth", method = "loess", span = .2, linewidth = 1) + 
  theme_bw() +  contpal +
    labs(title = "Dissolved Oxygen", y = "DO (mg/L)", x = NULL) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = DO_mgl, color = Site)))

(conttempplot <- ggplot(cdec_wide[is.na(cdec_wide$wtemp_c) == F & cdec_wide$wtemp_c < 50, ], aes(x = Datetime, y = wtemp_c, color = Site_no)) + 
    geom_line(alpha = .2) + geom_line(stat = "smooth", method = "loess", span = .2, linewidth = .8) + 
  theme_bw() +  contpal +
    labs(title = "Water temperature", y = "Temperature (C)", x = NULL))

(contchlplot <- ggplot(cdec_wide[is.na(cdec_wide$chla) == F & cdec_wide$chla < 60,], aes(x = Datetime, y = chla, color = Site_no)) + 
  geom_line(alpha = .2) + geom_line(stat = "smooth", method = "loess", span = .2, linewidth = .8) + 
  theme_bw() +  contpal +
    labs(title = "Chlorophyll-a", y = "Chl-a (ug/L)", x = NULL))

(contflowplot <- ggplot(cdec_wide[cdec_wide$Site_no %in% c("LIS", "RCS", "YBY", "PTC") & is.na(cdec_wide$discharge_cfs) == F,], 
                        aes(x = Datetime, y = discharge_cfs, color = Site_no)) + 
  geom_line(alpha = .2) +
  geom_line(stat = "smooth", method = "loess", span = .2, linewidth = .8) + contpal +
  theme_bw() + labs(title = "Discharge smoothed", y = "Discharge (cfs)", x = NULL))

(contflowplot2 <- ggplot(cdec_wide[cdec_wide$Site_no %in% c("RCS", "YBY", "PTC") & 
                                     is.na(cdec_wide$discharge_cfs) == F,], 
                        aes(x = Datetime, y = discharge_cfs, color = Site_no)) + 
    geom_line(alpha = .8, linewidth = .8) +
    geom_line(data = cdec_wide[cdec_wide$Site_no %in% c("LIS") & 
                                 is.na(cdec_wide$discharge_cfs) == F,], alpha = .2) +
    geom_line(data = cdec_wide[cdec_wide$Site_no %in% c("LIS") & 
                                 is.na(cdec_wide$discharge_cfs) == F,],
              stat = "smooth", method = "loess", span = .2, linewidth = .8) + contpal +
    theme_bw() + labs(title = "Discharge smoothed", y = "Discharge (cfs)", x = NULL))

(bignotchplot <- ggplot(cdec_wide[cdec_wide$Site_no %in% c("FWD"),], 
                        aes(x = Datetime, y = stage_ft, color = Site_no)) + 
    geom_line(color = "Navy", linewidth = 1) +
    # geom_line(stat = "smooth", method = "loess", span = .2, linewidth = 1) + 
    # scale_color_brewer(palette = "Set1") +
    theme_bw() + labs(title = "Big notch downstream stage", y = "Stage (ft)", x = NULL))

(tulepondplot <- ggplot(cdec_wide[cdec_wide$Site_no %in% c("YBT") & 
                                    cdec_wide$stage_ft > 12 &cdec_wide$stage_ft < 60,], 
                        aes(x = Datetime, y = stage_ft, color = Site_no)) + 
    geom_line(color = "Navy", linewidth = 1) +
    # geom_line(stat = "smooth", method = "loess", span = .2, linewidth = 1) + 
    # scale_color_brewer(palette = "Set1") +
    theme_bw() + labs(title = "Tule Pond Stage", y = "Stage (ft)", x = NULL))

cdec_wide_stage <- cdec_wide[cdec_wide$Site_no %in% c("KNL", "RCS", "YBT", "YBY", "I80", "LIS") &
                               cdec_wide$stage_ft < 60 & 
                               !(cdec_wide$stage_ft > 20 & cdec_wide$Site_no == "LIS") &
                               !(cdec_wide$stage_ft < 10 & cdec_wide$Site_no == "YBT"),]

cdec_wide_stage$Sitefac <-  factor(cdec_wide_stage$Site_no, levels = c("KNL", "RCS", "YBT", "YBY", "I80", "LIS"))

(stageplot <- ggplot(cdec_wide_stage[is.na(cdec_wide_stage$Site_no) == F, ], 
                     aes(x = Datetime, y = stage_ft, color = Sitefac)) + 
    geom_line(linewidth = 1) +  
    geom_line(data = cdec_wide_stage[cdec_wide_stage$Site_no == "KNL" & is.na(cdec_wide_stage$Site_no) == F, ], linewidth = 3, alpha = .5) +
    scale_color_viridis_d() + 
  theme_bw() + labs(title = "Stage ft", y = "Stage (ft)", x = NULL))

png("Output/Figures/YBLTE_Cont_wq_%02d.png",
    height = 10, width = 6, units = "in", res = 1000, family = "serif")

cowplot::plot_grid(tulepondplot,
                   contflowplot,
                   conttempplot,
                   contdoplot,
                   contspcplot,
                   contchlplot,
                   align  = "v",
                   nrow = 6)

cowplot::plot_grid(tulepondplot,
                   contflowplot2,
                   conttempplot,
                   contdoplot,
                   contspcplot,
                   contchlplot,
                   align  = "v",
                   nrow = 6)

cowplot::plot_grid(stageplot + theme(axis.text.x = element_blank()),
                   contflowplot2 + theme(axis.text.x = element_blank()),
                   conttempplot + theme(axis.text.x = element_blank()),
                   contdoplot + theme(axis.text.x = element_blank()),
                   contspcplot + theme(axis.text.x = element_blank()),
                   contchlplot,
                   align  = "v",
                   nrow = 6)

dev.off()
