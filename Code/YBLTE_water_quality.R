# Yolo LTE hydrology and water quality conditions
## Load libraries ----
library(tidyverse)

## Load functions ----
source("Code/YBLTE_useful_functions.R")

##Set colors
# fix colors for animations
animCol <-  scale_color_manual(values = c("KNL" = "#440154FF",
                                          "RCS" = "#482878FF",
                                          "FRE" = "#B63679FF",
                                          "YBT" = "#3E4A89FF",
                                          "CCY" = "#FB8861FF",
                                          "YBY" = "#31688EFF",
                                          "PTC" = "#FBFCA4",
                                          "PTC" = "#A8AB7D",
                                          "FWBN" = "#B63679FF", 
                                          "FW1" = "#440154FF", 
                                          "KLWW" = "#482878FF",
                                          "KNG3" = "#453581FF", 
                                          "CCSYB" = "#FB8861FF",
                                          "CNW" = "#34618DFF",
                                          "RD22" = "#2B748EFF", 
                                          "YBLR4" = "#24878EFF", 
                                          "SB4" = "#1F998AFF",
                                          "I80" = "#25AC82FF", 
                                          "AL0" = "#40BC72FF",
                                          "LIS" = "#67CC5CFF", 
                                          "STTD" = "#73A32F", 
                                          "TEW" = "#CBE11EFF",
                                          "TER" = "#FDE725FF"))
animFill <-  scale_fill_manual(values = c("KNL" = "#440154FF",
                                          "RCS" = "#482878FF",
                                          "FRE" = "#B63679FF",
                                          "YBT" = "#3E4A89FF",
                                          "CCY" = "#FB8861FF",
                                          "YBY" = "#31688EFF",
                                          "PTC" = "#FBFCA4",
                                          "FWBN" = "#B63679FF", 
                                          "FW1" = "#440154FF", 
                                          "KLWW" = "#482878FF",
                                          "KNG3" = "#453581FF", 
                                          "CCSYB" = "#FB8861FF",
                                          "CNW" = "#34618DFF",
                                          "RD22" = "#2B748EFF", 
                                          "YBLR4" = "#24878EFF", 
                                          "SB4" = "#1F998AFF",
                                          "I80" = "#25AC82FF", 
                                          "AL0" = "#40BC72FF",
                                          "LIS" = "#67CC5CFF", 
                                          "STTD" = "#73A32F", 
                                          "TEW" = "#CBE11EFF",
                                          "TER" = "#FDE725FF"))

## Load point wq data

wqp <- readxl::read_excel("Data/tabular/YBLTE_point_wq.xlsx")

wqp <- filter(wqp, Sample_Type=="zoop")

# for plotting, not listed sites were dropped (sitefac is NA)
wqp$Sitefac <- factor(wqp$Site, levels = c("FWBN", "FW1", 
                                           "KLWW","KNG3", "CCSYB",
                                           "CNW","RD22", 
                                           "YBLR4", "SB4", #"I80", 
                                           "AL0","LIS", "STTD", "TEW","TER"))


wqp$week <- as.integer(format(wqp$Date, format = "%W"))
wqp$week <- ifelse(wqp$week>=43, wqp$week-43, wqp$week+9)
wqp$weekchr <- as.character(wqp$week)
wqp$fdom_qsu <- as.numeric(wqp$fdom_qsu)

wqp$Zoop_score <- as.numeric(wqp$Zoop_score)
dput(wqp[wqp$week == 26,])
dput(wqp[wqp$week == 26 & wqp$Sample_Type == "wq",])
# wqp <- wqp[wqp$week > 0,]

##Download gage data ----
### CDEC ----

cdec_stations <- c("YBT", "YBY", "LIS", "RCS", "FRE", "CCY", "FWD", "FWU", "PTC", "KNL", "KLG", "I80")

# sensor is param name, sensor_num is for access
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
cdec_wide$Date <- as.Date(cdec_wide$Datetime)
# cdec_wide <- cdec_wide[cdec_wide$ec > 50, ]

## Preliminary plotting ----
dput(unique(wqp$Site))

###Spot measurements
# heat map to see change over time and site variation
# boxplots for more specific range/differences between sites

(tempplot <- ggplot(wqp %>% drop_na(Sitefac), aes(x = week, y = Sitefac, fill = Temp)) + 
  geom_raster() + labs(x = "Week", y=NULL, fill = "Temp (C)") +
    scale_x_continuous(limits = c(0, max(wqp$week)+1)) +
  theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev))

(tempbox <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Sitefac, y = Temp)) + 
    geom_boxplot() + labs(x = NULL, y = "Temp (C)") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))

(doplot <- ggplot(wqp %>% drop_na(Sitefac), aes(x = week, y = Sitefac, fill = DO_mgl)) + 
    geom_raster() + labs(x = "Week", y=NULL, fill = "DO (mgl)") +
    scale_x_continuous(limits = c(0, max(wqp$week)+1)) +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev))
(dobox <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Sitefac, y = DO_mgl)) + 
    geom_boxplot() + labs(x = NULL, y = "DO (mgl)") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))

(spcplot <- ggplot(wqp %>% drop_na(Sitefac), aes(x = week, y = Sitefac, fill = SPC_uscm)) + 
    geom_raster() + labs(x = "Week", y=NULL, fill = "SPC (uscm)") +
    scale_x_continuous(limits = c(0, max(wqp$week)+1)) +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev))
(spcbox <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Sitefac, y = SPC_uscm)) + 
    geom_boxplot() + labs(x = NULL, y = "SPC (uscm)") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))

(turbplot <- ggplot(wqp %>% drop_na(Sitefac), aes(x = week, y = Sitefac, fill = Turb_fnu)) + 
    geom_raster() + labs(x = "Week", y=NULL, fill = "Turb (FNU)") +
    scale_x_continuous(limits = c(0, max(wqp$week)+1)) +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev))
(turbbox <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Sitefac, y = Turb_fnu)) + 
    geom_boxplot() + labs(x = NULL, y = "Turb (FNU)") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))

(chlplot <- ggplot(wqp %>% drop_na(Sitefac), aes(x = week, y = Sitefac, fill = CHL_ugl)) + 
    geom_raster() + labs(x = "Week", y=NULL, fill = "Chl (ugl)") +
    scale_x_continuous(limits = c(0, max(wqp$week)+1)) +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev))
(chlbox <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Sitefac, y = CHL_ugl)) + 
    geom_boxplot() + labs(x = NULL, y = "Chl (ugl)") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))

(fdomplot <- ggplot(wqp %>% drop_na(c(Sitefac, fdom_qsu)), aes(x = week, y = Sitefac, fill = fdom_qsu)) + 
    geom_raster() + labs(x = "Week", y=NULL, fill = "FDOM (qsu)") +
    scale_x_continuous(limits = c(0, max(wqp$week)+1)) +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev))
(fdombox <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Sitefac, y = fdom_qsu)) + 
    geom_boxplot() + labs(x = NULL, y = "FDOM (qsu)") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))

(zoopplot <- ggplot(wqp %>% drop_na(c(Sitefac, Zoop_score)), aes(x = week, y = Sitefac, fill = Zoop_score)) + 
    geom_raster() + labs(x = "Week", y=NULL, fill = "Zoop score") +
    scale_x_continuous(limits = c(0, max(wqp$week)+1)) +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev))
(zoopbox <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Sitefac, y = Zoop_score)) + 
    geom_boxplot() + labs(x = NULL, y = "Zoop score") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))

png("Output/Figures/YBLTE_Point_wq_%02d.png",
    height = 6, width = 7, units = "in", res = 1000, family = "serif")

# cowplot::plot_grid(cowplot::plot_grid(tempplot + theme(legend.position = "none"),
#                    doplot+ theme(legend.position = "none"),
#                    spcplot+ theme(legend.position = "none"),
#                    turbplot+ theme(legend.position = "none"),
#                    chlplot+ theme(legend.position = "none"),
#                    fdomplot+ theme(legend.position = "none"),nrow = 3),
#                    cowplot::get_plot_component(chlplot, 'guide-box-bottom', return_all = TRUE),
#                    nrow = 2, rel_heights = c(9,1))

# merge heat maps
cowplot::plot_grid(cowplot::plot_grid(zoopplot,doplot,spcplot,turbplot,chlplot,fdomplot,
                                      align  = "v", nrow = 3))
# merge box plots
cowplot::plot_grid(cowplot::plot_grid(zoopbox,dobox,spcbox,turbbox,chlbox,fdombox,
                                      align  = "v", nrow = 3))
dev.off()

#dput(RColorBrewer::brewer.pal(9, "Set1"))
c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999") 

###Continuous gauge data

contpal <-  scale_color_manual(values = c("LIS" = RColorBrewer::brewer.pal(8, "Set1")[1], 
                                          "RCS" = RColorBrewer::brewer.pal(8, "Set1")[5], 
                                          "PTC" = RColorBrewer::brewer.pal(8, "Set1")[6], 
                                          "YBY" = RColorBrewer::brewer.pal(8, "Set1")[4],
                                          "YBT" = RColorBrewer::brewer.pal(8, "Set1")[3],
                                          "FRE" = RColorBrewer::brewer.pal(8, "Set1")[2],
                                          "CCY" = RColorBrewer::brewer.pal(8, "Set1")[7]))

(contspcplot <- ggplot(cdec_wide[is.na(cdec_wide$ec) == F,], aes(x = Datetime, y = ec, color = Site_no)) + 
  geom_line(alpha = .8) + #geom_line(stat = "smooth", method = "loess", span = .1, linewidth = 1) +
  theme_bw() +  contpal + scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
    labs(y = "EC (US/cm)", x = NULL) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = SPC_uscm), color = "black", size = 3) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = SPC_uscm, color = Site)))

# ggplot(cdec_wide[is.na(cdec_wide$spc) == F,], aes(x = Datetime, y = spc, color = Site_no)) + 
#   geom_line(alpha = .2) + geom_line(stat = "smooth", method = "loess", span = .2, linewidth = 1) + 
#   theme_bw() + labs(title = "Specific Conductivity", y = "SPC (US/cm)", x = NULL)+  contpal +
#   geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = SPC_uscm, color = Site))

(contdoplot <- ggplot(cdec_wide[is.na(cdec_wide$domgl) == F,], aes(x = Datetime, y = domgl, color = Site_no)) + 
    geom_line(alpha = .2) + geom_line(stat = "smooth", method = "loess", span = .1, linewidth = 1) + 
    theme_bw() +  contpal + scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
    labs(y = "DO (mg/L)", x = NULL) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = DO_mgl), color = "black", size = 3) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = DO_mgl, color = Site)))

(conttempplot <- ggplot(cdec_wide[is.na(cdec_wide$wtemp_c) == F & cdec_wide$wtemp_c < 50, ], aes(x = Datetime, y = wtemp_c, color = Site_no)) + 
    geom_line(alpha = .2) + geom_line(stat = "smooth", method = "loess", span = .1, linewidth = .8) + 
    theme_bw() +  contpal + scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
    labs(y = "Temperature (C)", x = NULL) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = Temp), color = "black", size = 3) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = Temp, color = Site)))

(contchlplot <- ggplot(cdec_wide[is.na(cdec_wide$chla) == F & cdec_wide$chla < 60,], aes(x = Datetime, y = chla, color = Site_no)) + 
    geom_line(alpha = .2) + geom_line(stat = "smooth", method = "loess", span = .1, linewidth = .8) + 
    theme_bw() +  contpal + scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
    labs(y = "Chl-a (ug/L)", x = NULL) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = CHL_ugl), color = "black", size = 3) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = CHL_ugl, color = Site)))

# new plots ---
(contphplot <- ggplot(cdec_wide[is.na(cdec_wide$ph) == F & cdec_wide$ph < 60,], aes(x = Datetime, y = ph, color = Site_no)) + 
    geom_line(alpha = .2) + geom_line(stat = "smooth", method = "loess", span = .1, linewidth = .8) + 
    theme_bw() +  contpal + scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
    labs(y = "pH", x = NULL) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = pH), color = "black", size = 3) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = pH, color = Site)))

(contturbplot <- ggplot(cdec_wide[is.na(cdec_wide$turb_fnu) == F & cdec_wide$turb_fnu < 80,], aes(x = Datetime, y = turb_fnu, color = Site_no)) + 
    geom_line(alpha = .2) + geom_line(stat = "smooth", method = "loess", span = .1, linewidth = .8) + 
    theme_bw() +  contpal + scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
    labs(y = "Turb (FNU)", x = NULL) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = Turb_fnu), color = "black", size = 3) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = Turb_fnu, color = Site)))

(contfdomplot <- ggplot(cdec_wide[is.na(cdec_wide$fdom) == F & cdec_wide$fdom < 120,], aes(x = Datetime, y = fdom, color = Site_no)) + 
    geom_line(alpha = .2) + geom_line(stat = "smooth", method = "loess", span = .1, linewidth = .8) + 
    theme_bw() +  contpal + scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
    labs(y = "FDOM (QSU)", x = NULL) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = fdom_qsu), color = "black", size = 3) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = fdom_qsu, color = Site)))

(contflowplot <- ggplot(cdec_wide[cdec_wide$Site_no %in% c("LIS", "RCS", "YBY", "PTC") & is.na(cdec_wide$discharge_cfs) == F,], 
                        aes(x = Datetime, y = discharge_cfs, color = Site_no)) + 
  geom_line(alpha = .2) + scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
  geom_line(stat = "smooth", method = "loess", span = .2, linewidth = .8) + contpal +
  theme_bw() + labs(y = "Discharge (cfs)", x = NULL))
# only LIS smoothed for flow plt 2

(contflowplot2 <- ggplot(cdec_wide[cdec_wide$Site_no %in% c("CCY", "RCS", "PTC", "FRE") & 
                                     is.na(cdec_wide$discharge_cfs) == F,], aes(x = Datetime, y = discharge_cfs, color = Site_no)) + 
    geom_ribbon(data = cdec_wide[cdec_wide$Site_no %in% c("FRE"),],
                aes(ymax = discharge_cfs, ymin = 0), color = "slateblue4", fill = "slateblue4", alpha = .2) +
    geom_line(alpha = .8, linewidth = .8) + scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
    geom_line(data = cdec_wide[cdec_wide$Site_no %in% c("LIS") & 
                                 is.na(cdec_wide$discharge_cfs) == F,], alpha = .2) + animCol +
    theme_bw() + labs(y = "Discharge (cfs)", x = NULL) +
    coord_cartesian(clip = "off",
                    ylim = c(0, max(cdec_wide[cdec_wide$Site_no %in% c("CCY", "RCS", "PTC"), "discharge_cfs"], na.rm = T))))
##Percent flow
flow_in_time <- cdec_wide %>% filter(Site_no %in% c("RCS", "FRE", "CCY", "PTC")) %>%drop_na(discharge_cfs)

flow_zero <- flow_in_time
flow_zero[flow_zero$discharge_cfs<0, 'discharge_cfs'] <- 0

flow_perc <- flow_zero %>% group_by(Date, Site_no) %>% 
  summarize(median_flow = median(discharge_cfs)) %>% 
  group_by(Date) %>% 
  mutate(sumflow = sum(median_flow), percflow = 100*median_flow/sumflow)

# percent flow plot, stacked bar plot (daily increments)
(contpercflowplot <- ggplot(data = flow_perc, aes(x = Date, y = percflow, group = Site_no, fill = Site_no)) +
  geom_bar(stat = "identity", alpha = 0.9, width = 1) + animFill +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-1") +
  labs(x = NULL, y = "Percent Flow") + theme_bw())

(bignotchplot <- ggplot(cdec_wide[cdec_wide$Site_no %in% c("FWB"),], 
                        aes(x = Datetime, y = discharge_cfs, color = Site_no)) + 
    geom_line(color = "Navy", linewidth = 1) +
    # geom_line(stat = "smooth", method = "loess", span = .2, linewidth = 1) + 
    # scale_color_brewer(palette = "Set1") +
    theme_bw() + labs(y = "Stage (ft)", x = NULL))

(tulepondplot <- ggplot(cdec_wide[cdec_wide$Site_no %in% c("YBT") & 
                                    cdec_wide$stage_ft > 12 &cdec_wide$stage_ft < 60,], 
                        aes(x = Datetime, y = stage_ft, color = Site_no)) + 
    geom_line(color = "Navy", linewidth = 1) +
    # geom_line(stat = "smooth", method = "loess", span = .2, linewidth = 1) + 
    # scale_color_brewer(palette = "Set1") +
    theme_bw() + labs(title = "Tule Pond Stage", y = "Stage (ft)", x = NULL))

cdec_wide_stage <- cdec_wide[cdec_wide$Site_no %in% c("KNL", "RCS", "FRE", "YBT", "YBY", "I80", "LIS", "KLG", "FRE") &
                               cdec_wide$stage_ft < 60 & 
                               !(cdec_wide$stage_ft > 20 & cdec_wide$Site_no == "LIS") &
                               !(cdec_wide$stage_ft < 10 & cdec_wide$Site_no == "YBT"),]
unique(cdec_wide_stage$Site_no)
cdec_wide_stage$Sitefac <-  factor(cdec_wide_stage$Site_no, levels = c("KNL", "KLG", "RCS", "FRE", "YBT", "YBY", "I80", "LIS"))

(stageplot <- ggplot(cdec_wide_stage[is.na(cdec_wide_stage$Site_no) == F, ], 
                     aes(x = Datetime, y = stage_ft, color = Sitefac)) + 
    geom_line(linewidth = 1) +  
    scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
    geom_line(data = cdec_wide_stage[cdec_wide_stage$Site_no == "KNL" & is.na(cdec_wide_stage$Site_no) == F, ], linewidth = 3, alpha = .5) +
    scale_color_viridis_d() + 
  theme_bw() + labs(title = "Stage ft", y = "Stage (ft)", x = NULL))

png("Output/Figures/YBLTE_Cont_wq_%02d.png",
    height = 10, width = 6, units = "in", res = 1000, family = "serif")

cowplot::plot_grid(
                   contflowplot2 + theme(axis.text.x = element_blank()),
                   contpercflowplot + theme(axis.text.x = element_blank()),
                   conttempplot + theme(axis.text.x = element_blank()),
                   contdoplot + theme(axis.text.x = element_blank()),
                   contspcplot + theme(axis.text.x = element_blank()),
                   contfdomplot + theme(axis.text.x = element_blank()),
                   contturbplot  + theme(axis.text.x = element_blank()),
                   contchlplot,
                   align  = "v",
                   nrow = 8)

dev.off()

stage_wide <- cdec_wide_stage  %>% 
  filter(Site_no %in% c("KLG", "KNL", "FRE")) %>% 
  select(Datetime, Site_no, stage_ft) %>% 
  pivot_wider(
    names_from = Site_no,
    values_from = stage_ft
  ) %>% 
  arrange(Datetime)

stage_wide <- stage_wide  %>% 
  mutate(above =  KLG > 26,   # TRUE when KLG > KNL
    ymin  = pmin(KLG, 26),
    ymax  = pmax(KLG, 26),
    above32 = FRE > 32,
    yminfre = pmin(FRE, 32),
    ymaxfre = pmax(FRE, 32)
  )

stage_wide$above32col <- ifelse(stage_wide$above32, "Overtop", "Below32")
stage_wide$above <- ifelse(stage_wide$above, "Flowing", "Below")

png("Output/Figures/YBLTE_Stage_plot_%02d.png",
    height = 6, width = 8, units = "in", res = 1000, family = "serif")

print(stageplot)

ggplot(cdec_wide_stage[is.na(cdec_wide_stage$Site_no) == F, ], 
                     aes(x = Datetime)) + 
    geom_ribbon(data = stage_wide,
              aes(ymin = yminfre, ymax = ymaxfre, fill = above32),
              alpha = 0.35, show.legend = F) +
  geom_ribbon(data = stage_wide,
    aes(ymin = ymin, ymax = ymax, fill = above),
    alpha = 0.35, show.legend = F) +
    theme_bw() + labs(title = "Stage ft", y = "Stage (ft)", x = NULL) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
  geom_line(alpha = 0, aes(y = stage_ft, color = Sitefac)) +  
    geom_line(data = cdec_wide_stage[cdec_wide_stage$Site_no %in% c("KLG", "KNL", "LIS"), ], linewidth = 1, 
              aes(y = stage_ft, color = Sitefac)) +
    geom_line(data = cdec_wide_stage[cdec_wide_stage$Site_no == "KNL" & is.na(cdec_wide_stage$Site_no) == F, ], 
              aes(y = stage_ft, color = Sitefac), linewidth = 3, alpha = .5) +
    scale_color_viridis_d() + 
  
  scale_fill_manual(values = c("white", "white","black", "blue"))

ggplot(cdec_wide_stage[is.na(cdec_wide_stage$Site_no) == F, ], 
       aes(x = Datetime)) + 
  geom_line(alpha = 0, aes(y = stage_ft, color = Sitefac)) +  
  geom_line(data = cdec_wide_stage[cdec_wide_stage$Site_no %in% c("FRE", "YBT", "LIS"), ], linewidth = 1, 
            aes(y = stage_ft, color = Sitefac)) +
  scale_color_viridis_d() + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
  geom_ribbon(data = stage_wide,
              aes(ymin = yminfre, ymax = ymaxfre, fill = above32),
              alpha = 0.35, show.legend = F) +
  geom_hline(yintercept = 32, color = "navy") +
  theme_bw() + labs(title = "Stage ft", y = "Stage (ft)", x = NULL) + 
  scale_fill_manual(values = c("white", "blue"))

ggplot(cdec_wide_stage[is.na(cdec_wide_stage$Site_no) == F, ], 
       aes(x = Datetime, y = stage_ft, color = Sitefac)) + 
  geom_line(alpha = 0) +  
  geom_line(data = cdec_wide_stage[cdec_wide_stage$Site_no %in% c("YBT", "YBY"), ], linewidth = 1) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
  # geom_line(data = cdec_wide_stage[cdec_wide_stage$Site_no == "KNL" & is.na(cdec_wide_stage$Site_no) == F, ], linewidth = 3, alpha = .5) +
  scale_color_viridis_d() + 
  theme_bw() + labs(title = "Stage ft", y = "Stage (ft)", x = NULL)

ggplot(cdec_wide_stage[is.na(cdec_wide_stage$Site_no) == F, ], 
       aes(x = Datetime, y = stage_ft, color = Sitefac)) + 
  geom_line(alpha = 0) +  
  geom_line(data = cdec_wide_stage[cdec_wide_stage$Site_no %in% c("I80", "LIS"), ], linewidth = 1) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
  # geom_line(data = cdec_wide_stage[cdec_wide_stage$Site_no == "KNL" & is.na(cdec_wide_stage$Site_no) == F, ], linewidth = 3, alpha = .5) +
  scale_color_viridis_d() + 
  theme_bw() + labs(title = "Stage ft", y = "Stage (ft)", x = NULL)

dev.off()

##Quanitfying zoop inputs ----


cowplot::plot_grid(
  ggplot(cdec_wide[cdec_wide$Site_no %in% c("CCY", "RCS", "PTC", "FRE") & 
                     is.na(cdec_wide$discharge_cfs) == F,], aes(x = Datetime, y = discharge_cfs, color = Site_no)) + 
    geom_ribbon(data = cdec_wide[cdec_wide$Site_no %in% c("FRE"),],
                aes(ymax = discharge_cfs, ymin = 0), color = "slateblue4", fill = "slateblue4", alpha = .2) +
    geom_line(alpha = .8, linewidth = .8) +
    geom_line(data = cdec_wide[cdec_wide$Site_no %in% c("LIS") & 
                                 is.na(cdec_wide$discharge_cfs) == F,], alpha = .2) + contpal +
    theme_bw() + labs(title = "Discharge smoothed", y = "Discharge (cfs)", x = NULL) +
    coord_cartesian(clip = "off",
                    ylim = c(0, max(cdec_wide[cdec_wide$Site_no %in% c("CCY", "RCS", "PTC"), "discharge_cfs"], na.rm = T)),
                    xlim = c(as.POSIXct("2025-11-1"), as.POSIXct("2026-4-1"))) + 
    theme(axis.text.x = element_blank()),
  
  ggplot(wqp[wqp$Sitefac %in% c("RD22", "STTD"),], aes(x = Date, y = Zoop_score, color = Sitefac)) + 
    geom_point() + geom_line() + theme_bw() + xlim(c(as.POSIXct("2025-11-1"), as.POSIXct("2026-4-1"))),
  nrow = 2, align = "v")


# PCA for point wq ----

# filter var of interest; temp too variable (discrete data),
  # sal and TDS like SPC, pc like chlorop, zoop not significant and not at every site
pc_in <- data.frame(wqp[,c("Sitefac", "Date", "DO_mgl", "SPC_uscm", "pH",           
                "Turb_fnu", "CHL_ugl", "fdom_qsu", "week")])
rownames(pc_in) <- paste(wqp$Site,wqp$RowID)
pc_in <- drop_na(pc_in)

# PCA calculation
pc <- prcomp(subset(pc_in, select=-c(Sitefac, Date, week)), scale=T)
pc$rotation <- -1*pc$rotation
pc$x <- -1*pc$x

# get variance per PC
pc_var <- pc$sdev^2 / sum(pc$sdev^2) # PC 1-4 explain most variance (36%, 28, 14, 9)
pc1_v <- round(pc_var[1] * 100, 1)
pc2_v <- round(pc_var[2] * 100, 1)

# create new df for plotting scores
pc_score <- as.data.frame(pc$x[, 1:2])
pc_score$Sitefac <- pc_in$Sitefac
pc_score$Date <- pc_in$Date
pc_score$week <- pc_in$week

# df for plotting each var within the PC
pc_load <- as.data.frame(pc$rotation[, 1:2])
scaling_factor <- 1.2 * max(abs(pc_score[, 1:2]))
pc_load_scaled <- pc_load*scaling_factor

# calculate convex hull by site
conv_hull <- pc_score %>% group_by(Sitefac) %>% slice(chull(PC1, PC2)) %>% subset(select=-c(week))

# plot, need to adjust aesthetics
  # tds and spc are basically the same, full overlap

png("Output/Figures/YBLTE_wq_PCA_%02d.png",
    height = 10, width = 10, units = "in", res = 1000, family = "serif")

(pcaplot <- ggplot(data = pc_score, aes())+
  geom_point(data=pc_score, aes(x=PC1, y=PC2, color=Sitefac, shape=Sitefac))+
  geom_polygon(data=conv_hull, aes(x=PC1, y=PC2, fill=Sitefac, color=Sitefac),
               alpha=0.1)+
  geom_segment(data=pc_load_scaled, aes(x=0, y=0, xend=PC1, yend=PC2),
               alpha=0.5, color="black", linewidth=0.8)+
  ggrepel::geom_label_repel(data=pc_load_scaled, aes(x=PC1, y=PC2),
                            fill="dimgrey", color="white", 
                            segment.color="dimgrey", alpha=0.8,
                            label=rownames(pc_load_scaled), seed=25)+
  labs(title = "Point Water Quality PCA",
       x=paste0("PC1 (", pc1_v, "% Variance)"),
       y=paste0("PC2 (", pc2_v, "% Variance)"),
       color="Site", fill="Site", shape="Site")+
  theme_bw()+ scale_color_viridis_d()+ scale_fill_viridis_d() + 
  scale_shape_manual(values = c(1:14)))

(pcaplot <- ggplot(data = pc_score, aes())+
    geom_point(data=pc_score, aes(x=PC1, y=PC2, color=Sitefac, shape=Sitefac))+
    geom_polygon(data=conv_hull, 
                 aes(x=PC1, y=PC2, fill=Sitefac),
                 alpha=0.0, linewidth = NULL)+
    geom_polygon(data=conv_hull[conv_hull$Sitefac %in% c("FWBN", "KLWW", "CCSYB"),], 
                 aes(x=PC1, y=PC2, fill=Sitefac, color=Sitefac),
                 alpha=0.1)+
    geom_segment(data=pc_load_scaled, aes(x=0, y=0, xend=PC1, yend=PC2),
                 alpha=0.5, color="black", linewidth=0.8)+
    ggrepel::geom_label_repel(data=pc_load_scaled, aes(x=PC1, y=PC2),
                              fill="dimgrey", color="white", 
                              segment.color="dimgrey", alpha=0.8,
                              label=rownames(pc_load_scaled), seed=25)+
    labs(title = "Point Water Quality PCA",
         x=paste0("PC1 (", pc1_v, "% Variance)"),
         y=paste0("PC2 (", pc2_v, "% Variance)"),
         color="Site", fill="Site", shape="Site")+
    theme_bw()+ scale_color_viridis_d()+ scale_fill_viridis_d() + 
    scale_shape_manual(values = c(1:14)))

dev.off()

# testing animation for pca
library(gganimate)
library(magick)

# pca plots, animated by week (starts at 2 where no missing data)
pcaplots <- ggplot()+
  # conv hulls for tributaries
  geom_polygon(data=conv_hull%>% filter(Sitefac %in% c("FWBN", "KLWW", "CCSYB")), 
               aes(x=PC1, y=PC2, fill=Sitefac), alpha=0.3)+
  # contributing variables to PC (lines)
  geom_segment(data=pc_load_scaled, aes(x=0, y=0, xend=PC1, yend=PC2),
               alpha=0.2, color="black", linewidth=0.8)+
  # background points
  geom_point(data=pc_score%>% subset(select=-c(week)), aes(x=PC1, y=PC2, 
      color=Sitefac, shape=Sitefac), alpha=0.4)+
  geom_path(data=pc_score[pc_score$Sitefac %in% "STTD",] %>% 
               subset(select=-c(week)), 
             aes(x=PC1, y=PC2, color=Sitefac), alpha=0.4, linewidth= 1.2)+
  # variable labels
  ggrepel::geom_label_repel(data=pc_load_scaled, aes(x=PC1, y=PC2),
                            fill="dimgrey", color="white",
                            segment.color="dimgrey", alpha=0.5,
                            label=rownames(pc_load_scaled), seed=25)+
  # animated points, sites of interest are bigger
  geom_point(data=pc_score, aes(x=PC1, y=PC2, color=Sitefac, shape=Sitefac, 
             size=ifelse(Sitefac %in% c("FWBN", "KLWW", "CCSYB", "STTD"), 2, 1)), stroke=2,
             alpha = 0.7)+
  # stat_ellipse(data = pc_score %>% filter(Sitefac %in% c("FWBN", "KLWW", "CCSYB")) %>%
  #                subset(select = -c(week)),
  #              aes(x = PC1, y = PC2, color = Sitefac)) +  
  labs(title = "Point Water Quality PCA during Week {round(frame_time, 0)}",
       x=paste0("PC1 (", pc1_v, "% Variance)"),
       y=paste0("PC2 (", pc2_v, "% Variance)"),
       color="Site", shape="Site")+guides(fill = "none", size = "none")+
  theme_bw()+ animCol+ animFill +
  scale_shape_manual(values = c(2:15))

pcaplots_lebls <- ggplot()+
  # convex hulls for tributaries
  geom_polygon(
    data = conv_hull %>% filter(Sitefac %in% c("FWBN","KLWW","CCSYB")),
    aes(x = PC1, y = PC2, fill = Sitefac), alpha = 0.3
  ) +
  
  # contributing variables to PC (lines)
  geom_segment(
    data = pc_load_scaled,
    aes(x = 0, y = 0, xend = PC1, yend = PC2),
    alpha = 0.2, color = "black", linewidth = 0.8
  ) +
  
  # background points
  geom_point(
    data = pc_score %>% subset(select = -c(week)),
    aes(x = PC1, y = PC2, color = Sitefac, shape = Sitefac),
    alpha = 0.4
  ) +
  
  # ⭐ UPDATED: track SB4 + YBLR4 instead of STTD
  geom_path(
    data = pc_score[pc_score$Sitefac %in% c("SB4","YBLR4"),] %>% subset(select = -c(week)),
    aes(x = PC1, y = PC2, color = Sitefac),
    alpha = 0.6, linewidth = 1.4
  ) +
  
  # variable labels
  ggrepel::geom_label_repel(
    data = pc_load_scaled,
    aes(x = PC1, y = PC2),
    fill = "dimgrey", color = "white",
    segment.color = "dimgrey", alpha = 0.5,
    label = rownames(pc_load_scaled), seed = 25
  ) +
  
  # ⭐ UPDATED: highlight SB4 + YBLR4 as large points
  geom_point(
    data = pc_score,
    aes(
      x = PC1, y = PC2, color = Sitefac, shape = Sitefac,
      size = ifelse(Sitefac %in% c("FWBN","KLWW","CCSYB","SB4","YBLR4"), 2, 1)
    ),
    stroke = 2, alpha = 0.7
  ) +
  
  labs(
    title = "Point Water Quality PCA during Week {round(frame_time, 0)}",
    x = paste0("PC1 (", pc1_v, "% Variance)"),
    y = paste0("PC2 (", pc2_v, "% Variance)"),
    color = "Site", shape = "Site"
  ) +
  guides(fill = "none", size = "none") +
  theme_bw() + animCol + animFill +
  scale_shape_manual(values = c(2:15))


# animate pca plot
pcgif <- animate(
  pcaplots + transition_time(week) + enter_fade() + exit_fade(),
  height = 500, width = 600, fps = 10
)

pcgif_lebls <- animate(
  pcaplots_lebls + transition_time(week) + enter_fade() + exit_fade(),
  height = 500, width = 600, fps = 10
)

# stage plots, animated by date (should line up with pca plots because same time frame)
stage_in_time <- cdec_wide_stage[is.na(cdec_wide_stage$Site_no) == F,]%>% 
  filter(between(Datetime, min(pc_score$Date),max(pc_score$Date)))
stageplot <- ggplot(stage_in_time,
                     aes(x = Datetime, y = stage_ft, color = Sitefac)) +
    # add extra line to RCS to differentiate better
    geom_line(data = stage_in_time %>% filter(Site_no == "RCS"), linewidth = 2, alpha = 0.5) +
    # FRE overtop line
    geom_hline(yintercept = 32, color="red4", alpha = 0.5, linetype = 2, linewidth = 1)+
    geom_line(linewidth = 1, alpha = 0.7) +
    # label for overtop line
    annotate("text", x = unique(pc_score$Date)[4], y=33, color="red4",
             label="Fremont Weir Overtop at 32 ft")+
    # add fill before overtop and FRE level
    geom_ribbon(data=stage_in_time %>% filter(Site_no == "FRE") %>% filter(stage_ft>=32), 
                aes(ymin=32,ymax=stage_ft), fill="red4", alpha=0.5, outline.type="lower")+
    animCol +
    theme_bw() + labs(, y = "Stage (ft)", x = NULL, color = "Site")

# animate stage plot
stggif <- animate(
  stageplot+transition_reveal(Datetime),
  height=400, width=600, fps = 10)

# flow plots, animated by date (should line up with pca plots because same time frame)

# get flow for tributaries in same time frame as pca
flow_in_time <- cdec_wide %>% filter(Site_no %in% c("RCS", "FRE", "CCY", "PTC")) %>% 
  filter(between(Datetime, min(pc_score$Date), max(pc_score$Date))) %>% drop_na(discharge_cfs)

# flow plot raw
(tribflowplot1 <- ggplot(flow_in_time, 
                        aes(x = Datetime, y = discharge_cfs, color = Site_no)) + 
    geom_line() + animCol +
    theme_bw() + labs(title = "Discharge smoothed", y = "Discharge (cfs)", x = NULL))

# edited flow plot
(tribflowplot2 <- ggplot(flow_in_time, 
                         aes(x = Datetime, y = discharge_cfs, color = Site_no)) + 
    geom_line() + animCol + animFill +
    # text for when FRE is out of frame
    geom_text(data = flow_in_time %>% filter(Site_no=="FRE"),
             aes(x = as.POSIXct("2025-12-01"),
             y = max((flow_in_time %>% filter(Site_no!="FRE"))$discharge_cfs),
             label = ifelse(discharge_cfs>max((flow_in_time %>% filter(Site_no!="FRE"))$discharge_cfs),
                            paste0("FRE at ", discharge_cfs), "")), show.legend = F) +
    # fill under line
    geom_ribbon(aes(ymin=min(discharge_cfs),ymax=discharge_cfs, fill=Site_no), 
                alpha=0.1, outline.type="lower") +
    # frame limits, allow FRE to break out of frame
    coord_cartesian(ylim=c(min(flow_in_time$discharge_cfs), 
        max((flow_in_time %>% filter(Site_no!="FRE"))$discharge_cfs)), clip = "off") +
    theme_bw() + labs(title = "Tributary Flow", y = "Discharge (cfs)", x = NULL))

# plotting for percent flow
flow_in_time$Date <- as.Date(flow_in_time$Datetime)
flow_in_time$Site_no <- factor(flow_in_time$Site_no, levels = c("RCS", "FRE", "CCY", "PTC"))

# if negative flow, set to 0 (in theory, not contributing)
flow_zero <- flow_in_time
flow_zero[flow_zero$discharge_cfs<0, 'discharge_cfs'] <- 0

# percent flow, get daily median per group then divide by sum of daily medians
flow_perc <- flow_zero %>% group_by(Date, Site_no) %>% 
  summarize(median_flow = median(discharge_cfs)) %>% 
  group_by(Date) %>% 
  mutate(sumflow = sum(median_flow), percflow = 100*median_flow/sumflow)

# percent flow plot, stacked bar plot (daily increments)
pflowplot <- ggplot(data = flow_perc, aes(x = Date, y = percflow, group = Site_no, fill = Site_no)) +
  geom_bar(stat = "identity", alpha = 0.9) + animFill +
  labs(x = NULL, y = "Percent Flow") + theme_bw()

# animate flow and percent flow plots
flowgif <- animate(
  tribflowplot2+transition_reveal(Datetime),
  height=300, width=600, fps = 10)
pflowgif <- animate(
  pflowplot+transition_states(Date)+shadow_mark(past=T),
  height=300, width=600, fps = 10)

# combine pca and flow animations
pcgif <- image_read(pcgif)
flowgif <- image_read(flowgif)
pflowgif <- image_read(pflowgif)
pcgif_lebls <- image_read(pcgif_lebls)
animation <- image_append(c(pcgif[1],flowgif[1], pflowgif[1]), stack = T)
for(i in 2:100){
  combined_gif <- image_append(c(pcgif[i],flowgif[i], pflowgif[i]), stack = T)
  animation <- c(animation, combined_gif)
}

animation_lebls <- image_append(c(pcgif_lebls[1],flowgif[1], pflowgif[1]), stack = T)
for(i in 2:100){
  combined_gif <- image_append(c(pcgif_lebls[i],flowgif[i], pflowgif[i]), stack = T)
  animation <- c(animation, combined_gif)
}


# save as gif
anim_save("Output/Figures/pca_flow.gif", animation)
# as mp4
anim_save("Output/Figures/pca_flow.mp4", animation, renderer = ffmpeg_renderer(codec = "libx264"))

# stage plot solo

# combine pca and flow animations horizontally
nframes <- length(pcgif)

# Get target dimensions from pcgif
pc_width  <- image_info(pcgif[1])$width
pc_height <- image_info(pcgif[1])$height

# Each right-hand panel should be half the height of pcgif
half_height <- pc_height / 2

# First frame
flow_resized  <- image_resize(flowgif[1],  paste0(pc_width, "x", half_height, "!"))
pflow_resized <- image_resize(pflowgif[1], paste0(pc_width, "x", half_height, "!"))

right_col <- image_append(c(flow_resized, pflow_resized), stack = TRUE)
animation <- image_append(c(pcgif[1], right_col), stack = FALSE)
animation_lebls <- image_append(c(pcgif_lebls[1], right_col), stack = FALSE)

# Loop through remaining frames
for(i in 2:nframes){
  
  flow_resized  <- image_resize(flowgif[i],  paste0(pc_width, "x", half_height, "!"))
  pflow_resized <- image_resize(pflowgif[i], paste0(pc_width, "x", half_height, "!"))
  
  right_col <- image_append(c(flow_resized, pflow_resized), stack = TRUE)
  
  combined <- image_append(c(pcgif[i], right_col), stack = FALSE)
  
  animation <- c(animation, combined)
}

# Loop through remaining frames
for(i in 2:nframes){
  
  flow_resized  <- image_resize(flowgif[i],  paste0(pc_width, "x", half_height, "!"))
  pflow_resized <- image_resize(pflowgif[i], paste0(pc_width, "x", half_height, "!"))
  
  right_col <- image_append(c(flow_resized, pflow_resized), stack = TRUE)
  
  combined <- image_append(c(pcgif_lebls[i], right_col), stack = FALSE)
  
  animation_lebls <- c(animation_lebls, combined)
}

# Save
anim_save("Output/Figures/pca_flow_horizontal2.gif", animation)
anim_save("Output/Figures/pca_flow_horizontal2.mp4", animation, renderer = ffmpeg_renderer(codec = "libx264"))

anim_save("Output/Figures/pca_flow_horizontal2_lebls.gif", animation_lebls)
anim_save("Output/Figures/pca_flow_horizontal2_lebls.mp4", animation_lebls, renderer = ffmpeg_renderer(codec = "libx264"))

