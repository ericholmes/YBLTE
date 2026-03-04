# Yolo LTE hydrology and water quality conditions
## Load libraries ----
library(tidyverse)

## Load functions ----
source("Code/YBLTE_useful_functions.R")

## Load point wq data

wqp <- readxl::read_excel("Data/tabular/YBLTE_point_wq.xlsx")

wqp <- filter(wqp, Sample_Type=="zoop")
dput(unique(wqp$Site))
# for plotting, not listed sites were dropped (sitefac is NA)
wqp$Sitefac <- factor(wqp$Site, levels = c("FWBN", "FW1", 
                                           "KLWW","KNG3", "CCSYB",
                                           "CNW","RD22", 
                                           "YBLR4", "SB4", #"I80", 
                                           "AL0","LIS", "STTD", "TEW","TER"))

# unique(wqp$Sitefac)
wqp$week <- as.integer(format(wqp$Date, format = "%W"))
wqp$week <- ifelse(wqp$week>=43, wqp$week-43, wqp$week+9)
wqp$weekchr <- as.character(wqp$week)

wqp$fdom_qsu <- as.numeric(wqp$fdom_qsu)

wqp$Zoop_score <- as.numeric(wqp$Zoop_score)

# wqp <- wqp[wqp$week > 0,]

##Download gage data ----
### CDEC ----

cdec_stations <- c("YBT", "YBY", "LIS", "RCS", "FRE", "FWD", "PTC", "KNL", "I80")

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

## Preliminary plotting ----
dput(unique(wqp$Site))

###Spot measurements
# heat map to see change over time and site variation
# boxplots for more specific range/differences between sites

(tempplot <- ggplot(wqp %>% drop_na(Sitefac), aes(x = week, y = Sitefac, fill = Temp)) + 
  geom_raster() + labs(x = "Week", y=NULL, fill = "Temp (C)") +
    scale_x_continuous(limits = c(0, length(unique(wqp$week)))) +
  theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev))

(tempbox <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Sitefac, y = Temp)) + 
    geom_boxplot() + labs(x = NULL, y = "Temp (C)") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))

(doplot <- ggplot(wqp %>% drop_na(Sitefac), aes(x = week, y = Sitefac, fill = DO_mgl)) + 
    geom_raster() + labs(x = "Week", y=NULL, fill = "DO (mgl)") +
    scale_x_continuous(limits = c(0, length(unique(wqp$week)))) +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev))
(dobox <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Sitefac, y = DO_mgl)) + 
    geom_boxplot() + labs(x = NULL, y = "DO (mgl)") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))

(spcplot <- ggplot(wqp %>% drop_na(Sitefac), aes(x = week, y = Sitefac, fill = SPC_uscm)) + 
    geom_raster() + labs(x = "Week", y=NULL, fill = "SPC (uscm)") +
    scale_x_continuous(limits = c(0, length(unique(wqp$week)))) +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev))
(spcbox <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Sitefac, y = SPC_uscm)) + 
    geom_boxplot() + labs(x = NULL, y = "SPC (uscm)") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))

(turbplot <- ggplot(wqp %>% drop_na(Sitefac), aes(x = week, y = Sitefac, fill = Turb_fnu)) + 
    geom_raster() + labs(x = "Week", y=NULL, fill = "Turb (FNU)") +
    scale_x_continuous(limits = c(0, length(unique(wqp$week)))) +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev))
(turbbox <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Sitefac, y = Turb_fnu)) + 
    geom_boxplot() + labs(x = NULL, y = "Turb (FNU)") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))

(chlplot <- ggplot(wqp %>% drop_na(Sitefac), aes(x = week, y = Sitefac, fill = CHL_ugl)) + 
    geom_raster() + labs(x = "Week", y=NULL, fill = "Chl (ugl)") +
    scale_x_continuous(limits = c(0, length(unique(wqp$week)))) +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev))
(chlbox <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Sitefac, y = CHL_ugl)) + 
    geom_boxplot() + labs(x = NULL, y = "Chl (ugl)") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))

(fdomplot <- ggplot(wqp %>% drop_na(c(Sitefac, fdom_qsu)), aes(x = week, y = Sitefac, fill = fdom_qsu)) + 
    geom_raster() + labs(x = "Week", y=NULL, fill = "FDOM (qsu)") +
    scale_x_continuous(limits = c(0, length(unique(wqp$week)))) +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev))
(fdombox <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Sitefac, y = fdom_qsu)) + 
    geom_boxplot() + labs(x = NULL, y = "FDOM (qsu)") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))

(zoopplot <- ggplot(wqp %>% drop_na(c(Sitefac, Zoop_score)), aes(x = week, y = Sitefac, fill = Zoop_score)) + 
    geom_raster() + labs(x = "Week", y=NULL, fill = "Zoop score") +
    scale_x_continuous(limits = c(0, length(unique(wqp$week)))) +
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
                                          "PTC" = RColorBrewer::brewer.pal(8, "Set1")[2], 
                                          "YBY" = RColorBrewer::brewer.pal(8, "Set1")[4],
                                          "YBT" = RColorBrewer::brewer.pal(8, "Set1")[3],
                                          "FWD" = RColorBrewer::brewer.pal(8, "Set1")[9]))

(contspcplot <- ggplot(cdec_wide[is.na(cdec_wide$ec) == F,], aes(x = Datetime, y = ec, color = Site_no)) + 
  geom_line(alpha = .2) + geom_line(stat = "smooth", method = "loess", span = .2, linewidth = 1) +
  theme_bw() +  contpal +
    labs(title = "Conductivity", y = "EC (US/cm)", x = NULL) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = SPC_uscm), color = "black", size = 3) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = SPC_uscm, color = Site)))

# ggplot(cdec_wide[is.na(cdec_wide$spc) == F,], aes(x = Datetime, y = spc, color = Site_no)) + 
#   geom_line(alpha = .2) + geom_line(stat = "smooth", method = "loess", span = .2, linewidth = 1) + 
#   theme_bw() + labs(title = "Specific Conductivity", y = "SPC (US/cm)", x = NULL)+  contpal +
#   geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = SPC_uscm, color = Site))

(contdoplot <- ggplot(cdec_wide[is.na(cdec_wide$domgl) == F,], aes(x = Datetime, y = domgl, color = Site_no)) + 
    geom_line(alpha = .2) + geom_line(stat = "smooth", method = "loess", span = .2, linewidth = 1) + 
    theme_bw() +  contpal +
    labs(title = "Dissolved Oxygen", y = "DO (mg/L)", x = NULL) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = DO_mgl), color = "black", size = 3) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = DO_mgl, color = Site)))

(conttempplot <- ggplot(cdec_wide[is.na(cdec_wide$wtemp_c) == F & cdec_wide$wtemp_c < 50, ], aes(x = Datetime, y = wtemp_c, color = Site_no)) + 
    geom_line(alpha = .2) + geom_line(stat = "smooth", method = "loess", span = .2, linewidth = .8) + 
    theme_bw() +  contpal +
    labs(title = "Water temperature", y = "Temperature (C)", x = NULL) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = Temp), color = "black", size = 3) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = Temp, color = Site)))

(contchlplot <- ggplot(cdec_wide[is.na(cdec_wide$chla) == F & cdec_wide$chla < 60,], aes(x = Datetime, y = chla, color = Site_no)) + 
    geom_line(alpha = .2) + geom_line(stat = "smooth", method = "loess", span = .2, linewidth = .8) + 
    theme_bw() +  contpal +
    labs(title = "Chlorophyll-a", y = "Chl-a (ug/L)", x = NULL) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = CHL_ugl), color = "black", size = 3) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = CHL_ugl, color = Site)))

# new plots ---
(contphplot <- ggplot(cdec_wide[is.na(cdec_wide$ph) == F & cdec_wide$ph < 60,], aes(x = Datetime, y = ph, color = Site_no)) + 
    geom_line(alpha = .2) + geom_line(stat = "smooth", method = "loess", span = .2, linewidth = .8) + 
    theme_bw() +  contpal +
    labs(title = "pH", y = "pH", x = NULL) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = pH), color = "black", size = 3) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = pH, color = Site)))

(contturbplot <- ggplot(cdec_wide[is.na(cdec_wide$turb_fnu) == F & cdec_wide$turb_fnu < 60,], aes(x = Datetime, y = turb_fnu, color = Site_no)) + 
    geom_line(alpha = .2) + geom_line(stat = "smooth", method = "loess", span = .2, linewidth = .8) + 
    theme_bw() +  contpal +
    labs(title = "Turbidity", y = "Turb (FNU)", x = NULL) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = Turb_fnu), color = "black", size = 3) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = Turb_fnu, color = Site)))

(contfdomplot <- ggplot(cdec_wide[is.na(cdec_wide$fdom) == F & cdec_wide$fdom < 60,], aes(x = Datetime, y = fdom, color = Site_no)) + 
    geom_line(alpha = .2) + geom_line(stat = "smooth", method = "loess", span = .2, linewidth = .8) + 
    theme_bw() +  contpal +
    labs(title = "Fluorescent Dissolved Organic Matter", y = "FDOM (QSU)", x = NULL) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = fdom_qsu), color = "black", size = 3) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = fdom_qsu, color = Site)))
# ---

(contflowplot <- ggplot(cdec_wide[cdec_wide$Site_no %in% c("LIS", "RCS", "YBY", "PTC") & is.na(cdec_wide$discharge_cfs) == F,], 
                        aes(x = Datetime, y = discharge_cfs, color = Site_no)) + 
  geom_line(alpha = .2) +
  geom_line(stat = "smooth", method = "loess", span = .2, linewidth = .8) + contpal +
  theme_bw() + labs(title = "Discharge smoothed", y = "Discharge (cfs)", x = NULL))
# only LIS smoothed for flow plt 2
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

cdec_wide_stage <- cdec_wide[cdec_wide$Site_no %in% c("KNL", "RCS", "FRE", "YBT", "YBY", "I80", "LIS") &
                               cdec_wide$stage_ft < 60 & 
                               !(cdec_wide$stage_ft > 20 & cdec_wide$Site_no == "LIS") &
                               !(cdec_wide$stage_ft < 10 & cdec_wide$Site_no == "YBT"),]

cdec_wide_stage$Sitefac <-  factor(cdec_wide_stage$Site_no, levels = c("KNL", "RCS", "FRE", "YBT", "YBY", "I80", "LIS"))

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

cowplot::plot_grid(stageplot + theme(axis.text.x = element_blank()),
                   contflowplot2 + theme(axis.text.x = element_blank()),
                   conttempplot + theme(axis.text.x = element_blank()),
                   contdoplot + theme(axis.text.x = element_blank()),
                   contspcplot + theme(axis.text.x = element_blank()),
                   contfdomplot + theme(axis.text.x = element_blank()),
                   contchlplot,
                   align  = "v",
                   nrow = 7)

cowplot::plot_grid(stageplot + theme(axis.text.x = element_blank()),
                   contflowplot2 + theme(axis.text.x = element_blank()),
                   conttempplot + theme(axis.text.x = element_blank()),
                   contdoplot + theme(axis.text.x = element_blank()),
                   contspcplot + theme(axis.text.x = element_blank()),
                   contchlplot,
                   align  = "v",
                   nrow = 6)

dev.off()

# PCA for point wq ---

# filter var of interest; temp too variable (discrete data), sal and TDS like SPC, pc like chlorop
pc_in <- data.frame(wqp[,c("Sitefac", "Date", "DO_mgl", "SPC_uscm", "pH",           
                "Turb_fnu", "CHL_ugl", "fdom_qsu", "Zoop_score", "week")])
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

dev.off()

# testing animation for pca
library(gganimate)
library(magick)

# pca plots, animated by week (starts at 2 where no missing data)

pcaplots <- ggplot()+
  geom_polygon(data=conv_hull, aes(x=PC1, y=PC2, fill=Sitefac),
               alpha=0.1)+
  geom_segment(data=pc_load_scaled, aes(x=0, y=0, xend=PC1, yend=PC2),
               alpha=0.2, color="black", linewidth=0.8)+
  geom_point(data=pc_score%>% subset(select=-c(week)), aes(x=PC1, y=PC2, 
      color=Sitefac, shape=Sitefac), alpha=0.4)+
  ggrepel::geom_label_repel(data=pc_load_scaled, aes(x=PC1, y=PC2),
                            fill="dimgrey", color="white",
                            segment.color="dimgrey", alpha=0.5,
                            label=rownames(pc_load_scaled), seed=25)+
  geom_point(data=pc_score, aes(x=PC1, y=PC2, color=Sitefac, shape=Sitefac, 
             size=ifelse(Sitefac=="FWBN", 3, 2)), stroke=2)+
  labs(title = "Point Water Quality PCA during Week {round(frame_time, 0)}",
       x=paste0("PC1 (", pc1_v, "% Variance)"),
       y=paste0("PC2 (", pc2_v, "% Variance)"),
       color="Site", shape="Site")+guides(fill = "none", size = "none")+
  theme_bw()+ scale_color_viridis_d()+ scale_fill_viridis_d() +
  scale_shape_manual(values = c(16, 2:14))

pcgif <- image_read(animate(
  pcaplots+transition_time(week)+enter_fade() + exit_fade(),
  height=500, width=600, fps = 5))

# stage plots, animated by date (should line up with pca plots because same time frame)
stage_in_time <- cdec_wide_stage[is.na(cdec_wide_stage$Site_no) == F,]%>% 
  filter(between(Datetime, min(pc_score$Date),max(pc_score$Date)))
stageplot <- ggplot(stage_in_time,
                     aes(x = Datetime, y = stage_ft, color = Sitefac)) +
    geom_line(data = stage_in_time %>% filter(Site_no == "FRE"), linewidth = 2, alpha = 0.5) +
    geom_hline(yintercept = 32, color="red4", alpha = 0.5, linetype = 2, linewidth = 1)+
    geom_line(linewidth = 1, alpha = 0.7) +
    annotate("text", x = unique(pc_score$Date)[4], y=33, color="red4",
             label="Fremont Weir Overtop at 32 ft")+
    geom_ribbon(data=stage_in_time %>% filter(Site_no == "FRE") %>% filter(stage_ft>=32), 
                aes(ymin=32,ymax=stage_ft), fill="red4", alpha=0.5, outline.type="lower")+
    scale_color_viridis_d() +
    theme_bw() + labs(, y = "Stage (ft)", x = NULL, color = "Site")

stggif <- image_read(animate(
  stageplot+transition_reveal(Datetime),
  height=400, width=600, fps = 5))

# combine pca and stage animations
animation <- image_append(c(pcgif[1], stggif[1]), stack = T)
for(i in 2:100){
  combined_gif <- image_append(c(pcgif[i], stggif[i]), stack = T)
  animation <- c(animation, combined_gif)
}

# save as gif
anim_save("Output/Figures/pca_stage.gif", animation)
