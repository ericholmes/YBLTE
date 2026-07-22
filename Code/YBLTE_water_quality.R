# Yolo LTE hydrology and water quality conditions ⭐ ⭐ ⭐ ⭐ ⭐ 
## Load libraries ----
library(tidyverse)
library(gganimate)
library(magick)
library(sf)
library(plotly)

## Load functions ----
source("Code/YBLTE_useful_functions.R")

## Set colors for animations ----
animCol <- scale_color_manual(values = c("KNL" = "#440154FF",
                                          "KLG" = "#444444",
                                          "RCS" = "#482878FF",
                                          "FRE" = "#B63679FF",
                                          "YBT" = "#3E4A89FF",
                                          "CCY" = "#FB8861FF",
                                          "YBY" = "#31688EFF",
                                          "PTC" = "#CED483",
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
animFill <- scale_fill_manual(values = c("KNL" = "#440154FF",
                                          "RCS" = "#482878FF",
                                          "FRE" = "#B63679FF",
                                          "YBT" = "#3E4A89FF",
                                          "CCY" = "#FB8861FF",
                                          "YBY" = "#31688EFF",
                                          "PTC" = "#E0E092",
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

## Load point wq data ----

wqp <- readxl::read_excel("Data/tabular/YBLTE_point_wq.xlsx")

wqp <- filter(wqp, Sample_Type=="zoop")

## Point wq data clean ----

# Factor for plotting, not listed sites were dropped (sitefac is NA)
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

wqp$Date <-as.Date(wqp$Date)

# dput(wqp[wqp$week == 26,])
# dput(wqp[wqp$week == 26 & wqp$Sample_Type == "wq",])
# wqp <- wqp[wqp$week > 0,]

## Download gage data ----
### CDEC ----

cdec_stations <- c("YBT", "YBY", "LIS", "RCS", "FRE", "CCY", "FWD", "FWU", "PTC", "KNL", "KLG", "I80")

# sensor is parameter name, sensor_num is for access
sensor_codes <- data.frame(sensor = c("chla", "ec", "discharge_cfs", "fdom", 
                                      "wtemp_f", "domgl","ph", "turb_fnu",
                                      "stage_ft"), 
                           sensor_num = c(28, 100, 20, 266, 
                                          25, 61, 62, 221, 1))

# Focus on water year and sampling season
startdate <- "2025-10-1"
enddate <- "2026-05-10"

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

# This may be incorrect, but stages below 14ft at FWD are forced to 14
cdec_wide$stage_ft <- ifelse(cdec_wide$Site_no == "FWD",
                             ifelse(cdec_wide$stage_ft <= 14, 14, cdec_wide$stage_ft),
                             cdec_wide$stage_ft)

# This may be inaccurate...It looks like it is already in SPC, so maybe the formula is backwards
cdec_wide$spc <- cdec_wide$ec / (1 + 0.02 * (cdec_wide$wtemp_c - 25))

cdec_wide$Date <- as.Date(cdec_wide$Datetime)

# cdec_wide <- cdec_wide[cdec_wide$ec > 50, ]

### Tidal Habitat Sondes ----
yff <- read.csv("Data/tabular/Tidal_Habitat_sonde_data/2025-11_YFF_2026-05.csv")

yff_cdec <- yff %>%
  mutate(
    Site_no  = SiteName,
    Datetime = as.POSIXct(datetime, format = "%m/%d/%Y %H:%M"),
    Date     = as.Date(Datetime),
    
    # CDEC-equivalent variables (YFF has no discharge or stage)
    discharge_cfs = NA_real_,
    stage_ft      = NA_real_,
    wtemp_f       = Temp_C * 9/5 + 32,
    wtemp_c       = Temp_C,
    ec            = SpCond_uScm,
    turb_fnu      = Turb_FNU,
    ph            = pH,
    fdom          = fDOM_RFU,
    spc           = SpCond_uScm
  ) %>%
  select(
    Site_no, Datetime, stage_ft, discharge_cfs, wtemp_f,
    chla = Chloro_ugL, domgl = ODO_mgL, ph, ec, turb_fnu,
    fdom, wtemp_c, spc, Date
  )

cdec_wide <- rbind(cdec_wide, yff_cdec)

## Point wq plotting ----
# dput(unique(wqp$Site))

# Heat map to see change over time and site variation; with week number or date
# Boxplots for more specific range/differences between sites

# Temperature
(tempplot <- ggplot(wqp %>% drop_na(Sitefac), aes(x = week, y = Sitefac, fill = Temp)) + 
  geom_raster() + labs(x = "Week", y=NULL, fill = "Temp (C)") +
    scale_x_continuous(limits = c(0, max(wqp$week)+1)) +
  theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev))
(tempplotdate <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Date, y = Sitefac, fill = Temp)) + 
    geom_tile(width = 7) + labs(x = NULL, y=NULL, fill = "Temp (C)") +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b"))
(tempbox <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Sitefac, y = Temp)) + 
    geom_boxplot() + labs(x = NULL, y = "Temp (C)") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))

# Dissolved oxygen
(doplot <- ggplot(wqp %>% drop_na(Sitefac), aes(x = week, y = Sitefac, fill = DO_mgl)) + 
    geom_raster() + labs(x = "Week", y=NULL, fill = "DO (mgl)") +
    scale_x_continuous(limits = c(0, max(wqp$week)+1)) +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev))
(doplotdate <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Date, y = Sitefac, fill = DO_mgl)) + 
    geom_tile(width = 8) + labs(x = NULL, y=NULL, fill = "DO (mgl)") +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev) + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b"))
(dobox <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Sitefac, y = DO_mgl)) + 
    geom_boxplot() + labs(x = NULL, y = "DO (mgl)") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))

# Specific conductivity
(spcplot <- ggplot(wqp %>% drop_na(Sitefac), aes(x = week, y = Sitefac, fill = SPC_uscm)) + 
    geom_raster() + labs(x = "Week", y=NULL, fill = "SPC (uscm)") +
    scale_x_continuous(limits = c(0, max(wqp$week)+1)) +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev))
(spcplotdate <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Date, y = Sitefac, fill = SPC_uscm)) + 
    geom_tile(width = 8) + labs(x = NULL, y=NULL, fill = "SPC (uscm)") +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev) + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b"))
(spcbox <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Sitefac, y = SPC_uscm)) + 
    geom_boxplot() + labs(x = NULL, y = "SPC (uscm)") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))

# Turbidity
(turbplot <- ggplot(wqp %>% drop_na(Sitefac), aes(x = week, y = Sitefac, fill = Turb_fnu)) + 
    geom_raster() + labs(x = "Week", y=NULL, fill = "Turb (FNU)") +
    scale_x_continuous(limits = c(0, max(wqp$week)+1)) +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev))
(turbplotdate <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Date, y = Sitefac, fill = Turb_fnu)) + 
    geom_tile(width = 8) + labs(x = NULL, y=NULL, fill = "Turb (FNU)") +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev) + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b"))
(turbbox <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Sitefac, y = Turb_fnu)) + 
    geom_boxplot() + labs(x = NULL, y = "Turb (FNU)") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))

# Chlorophyll-a
(chlplot <- ggplot(wqp %>% drop_na(Sitefac), aes(x = week, y = Sitefac, fill = CHL_ugl)) + 
    geom_raster() + labs(x = "Week", y=NULL, fill = "Chl (ugl)") +
    scale_x_continuous(limits = c(0, max(wqp$week)+1)) +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev))
(chlplotdate <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Date, y = Sitefac, fill = CHL_ugl)) + 
    geom_tile(width = 8) + labs(x = NULL, y= NULL, fill = "Chl (ugl)") +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev) + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b"))
(chlbox <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Sitefac, y = CHL_ugl)) + 
    geom_boxplot() + labs(x = NULL, y = "Chl (ugl)") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))

# Fluorescent dissolved organic matter (FDOM)
(fdomplot <- ggplot(wqp %>% drop_na(c(Sitefac, fdom_qsu)), aes(x = week, y = Sitefac, fill = fdom_qsu)) + 
    geom_raster() + labs(x = "Week", y=NULL, fill = "FDOM (qsu)") +
    scale_x_continuous(limits = c(0, max(wqp$week)+1)) +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev))
(fdomplotdate <- ggplot(wqp %>% drop_na(c(Sitefac, fdom_qsu)), aes(x = Date, y = Sitefac, fill = fdom_qsu)) + 
    geom_tile(width = 7) + labs(x = NULL, y=NULL, fill = "FDOM (qsu)") +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev) + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b"))
(fdombox <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Sitefac, y = fdom_qsu)) + 
    geom_boxplot() + labs(x = NULL, y = "FDOM (qsu)") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))

# Zooplankton score (1-5)
(zoopplot <- ggplot(wqp %>% drop_na(c(Sitefac, Zoop_score)), aes(x = week, y = Sitefac, fill = Zoop_score)) + 
    geom_raster() + labs(x = "Week", y=NULL, fill = "Zoop score") +
    scale_x_continuous(limits = c(0, max(wqp$week)+1)) +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev))
(zoopplotdate <- ggplot(wqp %>% drop_na(c(Sitefac, Zoop_score)), aes(x = Date, y = Sitefac, fill = Zoop_score)) + 
    geom_tile(width = 8) + labs(x = NULL, y=NULL, fill = "Zoop score") +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev) + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b"))
(zoopbox <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Sitefac, y = Zoop_score)) + 
    geom_boxplot() + labs(x = NULL, y = "Zoop score") +
    theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))

# Saving plots
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

# Merge heat maps
cowplot::plot_grid(cowplot::plot_grid(zoopplot,doplot,spcplot,turbplot,chlplot,fdomplot,
                                      align  = "v", nrow = 3))

cowplot::plot_grid(cowplot::plot_grid(zoopplotdate,doplotdate,spcplotdate,turbplotdate,chlplotdate,fdomplotdate,
                                      align  = "v", nrow = 3))
# Merge box plots
cowplot::plot_grid(cowplot::plot_grid(zoopbox,dobox,spcbox,turbbox,chlbox,fdombox,
                                      align  = "v", nrow = 3))
dev.off()


## Correlation plot for point wq ----

# Select numeric WQ parameters
wq_numeric <- wqp %>% 
  select("Temp", "DO_mgl", "SPC_uscm","pH", "Turb_fnu", 
         "PC_ugl", "CHL_ugl", "fdom_qsu", "Zoop_score") %>% 
  drop_na()
# dput(colnames(wqp))

# Compute correlation matrix
cor_mat <- cor(wq_numeric, use = "pairwise.complete.obs")

# Convert to long format
cor_df <- as.data.frame(cor_mat) %>%
  rownames_to_column("Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "correlation")

# Plot
corplot <- (ggplot(cor_df, aes(x = Var1, y = Var2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "red",
    mid = "beige",
    high = "blue",
    midpoint = 0,
    name = "Correlation", limits = c(-1, 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(fill = "Correlation", x = NULL, y = NULL))

## Continuous gauge data plotting ----
#dput(RColorBrewer::brewer.pal(9, "Set1"))
# c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999") 
# contpal <-  scale_color_manual(values = c("LIS" = RColorBrewer::brewer.pal(8, "Set1")[1], 
#                                           "RCS" = RColorBrewer::brewer.pal(8, "Set1")[5], 
#                                           "PTC" = RColorBrewer::brewer.pal(8, "Set1")[6], 
#                                           "YBY" = RColorBrewer::brewer.pal(8, "Set1")[4],
#                                           "YBT" = RColorBrewer::brewer.pal(8, "Set1")[3],
#                                           "FRE" = RColorBrewer::brewer.pal(8, "Set1")[2],
#                                           "CCY" = RColorBrewer::brewer.pal(8, "Set1")[7],
#                                           "YFF" = "#999999"))

# Specific conductivity
(contspcplot <- ggplot(cdec_wide[is.na(cdec_wide$ec) == F,], aes(x = Datetime, y = ec, color = Site_no)) + 
  geom_line(alpha = .8) + #geom_line(stat = "smooth", method = "loess", span = .1, linewidth = 1) +
  theme_bw() +  animCol + scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
    labs(y = "SpC (US/cm)", x = NULL, color = "Site") +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = SPC_uscm), color = "black", size = 3) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = SPC_uscm, color = Site)))

# ggplot(cdec_wide[is.na(cdec_wide$spc) == F,], aes(x = Datetime, y = spc, color = Site_no)) + 
#   geom_line(alpha = .2) + geom_line(stat = "smooth", method = "loess", span = .2, linewidth = 1) + 
#   theme_bw() + labs(title = "Specific Conductivity", y = "SPC (US/cm)", x = NULL)+  contpal +
#   geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = SPC_uscm, color = Site))

# Dissolved oxygen
(contdoplot <- ggplot(cdec_wide[is.na(cdec_wide$domgl) == F,], aes(x = Datetime, y = domgl, color = Site_no)) + 
    geom_line(alpha = .2) + geom_line(stat = "smooth", method = "loess", span = .1, linewidth = 1) + 
    theme_bw() +  animCol + scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
    labs(y = "DO (mg/L)", x = NULL, color = "Site") +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = DO_mgl), color = "black", size = 3) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = DO_mgl, color = Site)))

# Temperature
(conttempplot <- ggplot(cdec_wide[is.na(cdec_wide$wtemp_c) == F & cdec_wide$wtemp_c < 50 & 
    cdec_wide$Site_no != "RCS", ], aes(x = Datetime, y = wtemp_c, color = Site_no)) + 
    geom_line(alpha = .2) + geom_line(stat = "smooth", method = "loess", span = .1, linewidth = .8) + 
    theme_bw() +  animCol + scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
    labs(y = "Temperature (C)", x = NULL, color = "Site") +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = Temp), color = "black", size = 3) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = Temp, color = Site)))

# Chlorophyll-a
(contchlplot <- ggplot(cdec_wide[is.na(cdec_wide$chla) == F & cdec_wide$chla < 60,], aes(x = Datetime, y = chla, color = Site_no)) + 
    geom_line(alpha = .2) + geom_line(stat = "smooth", method = "loess", span = .1, linewidth = .8) + 
    theme_bw() +  animCol + scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
    labs(y = "Chl-a (ug/L)", x = NULL, color = "Site") +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = CHL_ugl), color = "black", size = 3) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = CHL_ugl, color = Site)))

# pH
(contphplot <- ggplot(cdec_wide[is.na(cdec_wide$ph) == F & cdec_wide$ph < 60,], aes(x = Datetime, y = ph, color = Site_no)) + 
    geom_line(alpha = .2) + geom_line(stat = "smooth", method = "loess", span = .1, linewidth = .8) + 
    theme_bw() +  animCol + scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
    labs(y = "pH", x = NULL, color = "Site") +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = pH), color = "black", size = 3) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = pH, color = Site)))

# Turbidity
(contturbplot <- ggplot(cdec_wide[is.na(cdec_wide$turb_fnu) == F & cdec_wide$turb_fnu < 80,], aes(x = Datetime, y = turb_fnu, color = Site_no)) + 
    geom_line(alpha = .2) + geom_line(stat = "smooth", method = "loess", span = .1, linewidth = .8) + 
    theme_bw() +  animCol + scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
    labs(y = "Turb (FNU)", x = NULL, color = "Site") +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = Turb_fnu), color = "black", size = 3) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = Turb_fnu, color = Site)))
 
# Fluorescent dissolved organic matter (FDOM)
(contfdomplot <- ggplot(cdec_wide[is.na(cdec_wide$fdom) == F & cdec_wide$fdom < 120,], aes(x = Datetime, y = fdom, color = Site_no)) + 
    geom_line(alpha = .2) + geom_line(stat = "smooth", method = "loess", span = .1, linewidth = .8) + 
    theme_bw() +  animCol + scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
    labs(y = "FDOM (QSU)", x = NULL, color = "Site") +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = fdom_qsu), color = "black", size = 3) +
    geom_point(data = wqp[wqp$Site == "LIS", ], aes(x = Date, y = fdom_qsu, color = Site)))

### Flow ----
(contflowplot <- ggplot(cdec_wide[cdec_wide$Site_no %in% c("LIS", "RCS", "YBY", "PTC") & is.na(cdec_wide$discharge_cfs) == F,], 
                        aes(x = Datetime, y = discharge_cfs, color = Site_no)) + 
  geom_line(alpha = .5) + scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
  geom_line(stat = "smooth", method = "loess", span = .2, linewidth = .8) + animCol +
  theme_bw() + labs(y = "Discharge (cfs)", x = NULL, color = "Site"))

# Only LIS smoothed for flow plot 2
(contflowplot2 <- ggplot(cdec_wide[cdec_wide$Site_no %in% c("CCY", "RCS", "PTC", "FRE") & 
                                     is.na(cdec_wide$discharge_cfs) == F,], aes(x = Datetime, y = discharge_cfs, color = Site_no)) + 
    geom_ribbon(data = cdec_wide[cdec_wide$Site_no %in% c("FRE"),],
                aes(ymax = discharge_cfs, ymin = 0), color = "slateblue4", fill = "slateblue4", alpha = .5) +
    geom_line(alpha = .8, linewidth = .8) + scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
    geom_line(data = cdec_wide[cdec_wide$Site_no %in% c("LIS") & 
                                 is.na(cdec_wide$discharge_cfs) == F,], alpha = .5) + animCol +
    theme_bw() + labs(y = "Discharge (cfs)", x = NULL, color = "Site") +
    coord_cartesian(clip = "off",
                    ylim = c(0, max(cdec_wide[cdec_wide$Site_no %in% c("CCY", "RCS", "PTC"), "discharge_cfs"], na.rm = T))))

# Percent flow determination for major tributaries
flow_in_time <- cdec_wide %>% filter(Site_no %in% c("RCS", "FRE", "CCY", "PTC")) %>%drop_na(discharge_cfs)

flow_zero <- flow_in_time
flow_zero[flow_zero$discharge_cfs<0, 'discharge_cfs'] <- 0

flow_perc <- flow_zero %>% group_by(Date, Site_no) %>% 
  summarize(median_flow = median(discharge_cfs)) %>% 
  group_by(Date) %>% 
  mutate(sumflow = sum(median_flow), percflow = 100*median_flow/sumflow)

flow_perc$Site_no <- factor(flow_perc$Site_no, levels = c("RCS", "FRE", "CCY", "PTC"))

# Percent flow plot, stacked bar plot (daily increments)
(contpercflowplot <- ggplot(data = flow_perc, aes(x = Date, y = percflow, group = Site_no, fill = Site_no)) +
  geom_bar(stat = "identity", alpha = 0.7, width = 1) + animFill +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-1") +
  labs(x = NULL, y = "Percent Flow", fill = "Site") + theme_bw())

# Flow at big notch (no cdec data for FWB, limited at FRE)
# (bignotchplot <- ggplot(cdec_wide[cdec_wide$Site_no %in% c("FWB"),], 
#     aes(x = Datetime, y = discharge_cfs)) + 
#     geom_line(color = "Navy", linewidth = 1) +                  
#     # geom_line(stat = "smooth", method = "loess", span = .2, linewidth = 1) + 
#     # scale_color_brewer(palette = "Set1") +
#     theme_bw() + labs(y = "Stage (ft)", x = NULL))

### Stage ----

# Stage at tule pond
(tulepondplot <- ggplot(cdec_wide[cdec_wide$Site_no %in% c("YBT") & 
                        cdec_wide$stage_ft > 12 &cdec_wide$stage_ft < 60,], 
                        aes(x = Datetime, y = stage_ft)) + 
    geom_line(color = "Navy", linewidth = 1) + 
    # geom_line(stat = "smooth", method = "loess", span = .2, linewidth = 1) + 
    # scale_color_brewer(palette = "Set1") +
    theme_bw() + labs(title = "Tule Pond Stage", y = "Stage (ft)", x = NULL))

# Selecting sites and filtering data for stage plot
cdec_wide_stage <- cdec_wide[cdec_wide$Site_no %in% 
                               c("KNL", "RCS", "FRE", "YBT", "YBY", "I80", "LIS", "KLG", "FRE") &
                               cdec_wide$stage_ft < 60 & 
                               !(cdec_wide$stage_ft > 20 & cdec_wide$Site_no == "LIS") &
                               !(cdec_wide$stage_ft < 10 & cdec_wide$Site_no == "YBT"),]
# unique(cdec_wide_stage$Site_no)
cdec_wide_stage$Sitefac <-  factor(cdec_wide_stage$Site_no, levels = c("KNL", "KLG", "RCS", "FRE", "YBT", "YBY", "I80", "LIS"))

# Assigning when special sites are flowing
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

# Stage plot for different sites
(stageplot <- ggplot(cdec_wide_stage[is.na(cdec_wide_stage$Site_no) == F, ], 
                     aes(x = Datetime, y = stage_ft, color = Sitefac)) + 
    geom_line(linewidth = 1, alpha = 0.9) +  
    scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
    geom_line(data = cdec_wide_stage[cdec_wide_stage$Site_no == "KNL" & is.na(cdec_wide_stage$Site_no) == F, ], linewidth = 3, alpha = .5) +
    animCol + 
  theme_bw() + labs(title = "River Stage ", y = "Stage (ft)", x = NULL, color = "Site"))

### Save outputs ----
# All gauge plots
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

# Flow plot
png("Output/Yolo_hydrographs_2026%03d.png", 
    family = "serif", res = 500, height = 1.8, width = 14, units = "in")

(contflowplot2_class <- ggplot(cdec_wide[cdec_wide$Site_no %in% c("CCY", "RCS", "PTC", "FRE") & 
                                     is.na(cdec_wide$discharge_cfs) == F,], aes(x = Datetime, y = discharge_cfs, color = Site_no)) + 
    geom_ribbon(data = cdec_wide[cdec_wide$Site_no %in% c("FRE"),],
                aes(ymax = discharge_cfs, ymin = 0), color = "slateblue4", fill = "slateblue4", alpha = .2) +
    geom_line(alpha = .8, linewidth = .8) + scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
    geom_line(data = cdec_wide[cdec_wide$Site_no %in% c("LIS") & 
                                 is.na(cdec_wide$discharge_cfs) == F,], alpha = .2) + animCol +
    geom_vline(xintercept = c(as.POSIXct("2026-1-8"), as.POSIXct("2026-2-12"), as.POSIXct("2026-3-4"), as.POSIXct("2026-3-9")),
               color = "yellow",
               linetype = "dashed") +
    theme_bw() + labs(y = "Discharge (cfs)", x = NULL, color = "Site") +
    coord_cartesian(clip = "off",
                    ylim = c(0, max(cdec_wide[cdec_wide$Site_no %in% c("CCY", "RCS", "PTC"), "discharge_cfs"], na.rm = T))))

dev.off()
(contflowplot2_class <- ggplot(cdec_wide[cdec_wide$Site_no %in% c("CCY", "RCS", "PTC", "FRE") & 
                                           is.na(cdec_wide$discharge_cfs) == F,], aes(x = Datetime, y = discharge_cfs, color = Site_no)) + 
    geom_ribbon(data = cdec_wide[cdec_wide$Site_no %in% c("FRE"),],
                aes(ymax = discharge_cfs, ymin = 0), color = "slateblue4", fill = "slateblue4", alpha = .2) +
    geom_line(alpha = .8, linewidth = .8) + scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") + animCol +
    theme_bw() + labs(y = "Discharge (cfs)", x = NULL, color = "Site") +
    coord_cartesian(clip = "off",
                    ylim = c(0, max(cdec_wide[cdec_wide$Site_no %in% c("CCY", "RCS", "PTC"), "discharge_cfs"], na.rm = T))))

# Stage plots, two sites at a time
png("Output/Figures/YBLTE_Stage_plot_%02d.png",
    height = 6, width = 8, units = "in", res = 1000, family = "serif")

# print(stageplot)

ggplot(cdec_wide_stage[is.na(cdec_wide_stage$Site_no) == F, ], 
                     aes(x = Datetime)) + 
    geom_ribbon(data = stage_wide %>% filter(above32),
              aes(ymin = yminfre, ymax = ymaxfre), fill = "blue",
              alpha = 0.35, show.legend = F) +
  geom_ribbon(data = stage_wide %>% filter(above=="Flowing"),
    aes(ymin = ymin, ymax = ymax), fill = "black",
    alpha = 0.35, show.legend = F) +
    theme_bw() + labs(y = "Stage (ft)", x = NULL, color = "Site") +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
  geom_line(alpha = 0, aes(y = stage_ft, color = Sitefac)) +  
    geom_line(data = cdec_wide_stage[cdec_wide_stage$Site_no %in% c("KLG", "KNL", "LIS"), ], linewidth = 1, 
              aes(y = stage_ft, color = Sitefac)) +
    geom_line(data = cdec_wide_stage[cdec_wide_stage$Site_no == "KNL" & is.na(cdec_wide_stage$Site_no) == F, ], 
              aes(y = stage_ft, color = Sitefac), linewidth = 3, alpha = .5) +
    animCol

ggplot(cdec_wide_stage[is.na(cdec_wide_stage$Site_no) == F, ], 
       aes(x = Datetime)) + 
  geom_line(alpha = 0, aes(y = stage_ft, color = Sitefac)) +  
  geom_line(data = cdec_wide_stage[cdec_wide_stage$Site_no %in% c("FRE", "YBT", "LIS"), ], linewidth = 1, 
            aes(y = stage_ft, color = Sitefac)) +
  animCol + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
  geom_ribbon(data = stage_wide %>% filter(above32),
              aes(ymin = yminfre, ymax = ymaxfre), fill = "blue",
              alpha = 0.35, show.legend = F) +
  geom_hline(yintercept = 32, color = "navy") +
  theme_bw() + labs(y = "Stage (ft)", x = NULL, color = "Site")

ggplot(cdec_wide_stage[is.na(cdec_wide_stage$Site_no) == F, ], 
       aes(x = Datetime, y = stage_ft, color = Sitefac)) + 
  geom_line(alpha = 0) +  
  geom_line(data = cdec_wide_stage[cdec_wide_stage$Site_no %in% c("YBT", "YBY"), ], linewidth = 1) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
  # geom_line(data = cdec_wide_stage[cdec_wide_stage$Site_no == "KNL" & is.na(cdec_wide_stage$Site_no) == F, ], linewidth = 3, alpha = .5) +
  animCol + 
  theme_bw() + labs(y = "Stage (ft)", x = NULL, color = "Site")

ggplot(cdec_wide_stage[is.na(cdec_wide_stage$Site_no) == F, ], 
       aes(x = Datetime, y = stage_ft, color = Sitefac)) + 
  geom_line(alpha = 0) +  
  geom_line(data = cdec_wide_stage[cdec_wide_stage$Site_no %in% c("I80", "LIS"), ], linewidth = 1) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
  # geom_line(data = cdec_wide_stage[cdec_wide_stage$Site_no == "KNL" & is.na(cdec_wide_stage$Site_no) == F, ], linewidth = 3, alpha = .5) +
  animCol + 
  theme_bw() + labs(y = "Stage (ft)", x = NULL, color = "Site")

dev.off()

## Quantifying zooplankton inputs ----

cowplot::plot_grid(
  ggplot(cdec_wide[cdec_wide$Site_no %in% c("CCY", "RCS", "PTC", "FRE") & 
                     is.na(cdec_wide$discharge_cfs) == F,], aes(x = Datetime, y = discharge_cfs, color = Site_no)) + 
    geom_ribbon(data = cdec_wide[cdec_wide$Site_no %in% c("FRE"),],
                aes(ymax = discharge_cfs, ymin = 0), color = "slateblue4", fill = "slateblue4", alpha = .2) +
    geom_line(alpha = .8, linewidth = .8) + 
    geom_line(data = cdec_wide[cdec_wide$Site_no %in% c("LIS") & 
                                 is.na(cdec_wide$discharge_cfs) == F,], alpha = .5) + animCol +
    theme_bw() + labs(title = "Zooplankton Inputs", y = "Discharge (cfs)", x = NULL, color = "Site") +
    coord_cartesian(clip = "off",
                    ylim = c(0, max(cdec_wide[cdec_wide$Site_no %in% c("CCY", "RCS", "PTC"), "discharge_cfs"], na.rm = T)),
                    xlim = c(as.POSIXct("2025-11-1"), as.POSIXct("2026-4-1"))) + 
    theme(axis.text.x = element_blank()),
  
  ggplot(wqp[wqp$Sitefac %in% c("RD22", "STTD"),], aes(x = Date, y = Zoop_score, color = Sitefac)) + 
    geom_point() + geom_line() + theme_bw() + xlim(c(as.POSIXct("2025-11-1"), as.POSIXct("2026-4-1"))) +
    labs(y = "Zooplankton Score", x = NULL, color = "Site") + animCol,
  nrow = 2, align = "v")

## PCA for point wq ----

### Cleaning wqp to plug into PCA ----

# Filter var of interest; temp too variable (discrete data),
  # sal and TDS like SPC, pc like chlorop, zoop not at every site
pc_in <- data.frame(wqp[wqp$Date > as.Date("2025-11-15")& wqp$Date < as.Date("2026-04-01"),
                        c("RowID","Site","Sitefac", "Date", "DO_mgl", "SPC_uscm", "pH",           
                          "Turb_fnu", "CHL_ugl", "fdom_qsu", "week")])

rownames(pc_in) <- paste(pc_in$Site, pc_in$RowID)
pc_in <- drop_na(pc_in)
pc_in <- pc_in[pc_in$RowID != 257,] ## dropping KLWW on day when flow was reversing

### PCA calculations ----
pc <- prcomp(subset(pc_in, select=-c(RowID, Site,Sitefac, Date, week)), scale=T)
pc$rotation <- -1*pc$rotation
pc$x <- -1*pc$x

# Get variance per PC
pc_var <- pc$sdev^2 / sum(pc$sdev^2) # PC 1-4 explain most variance (36%, 28, 14, 9)
pc1_v <- round(pc_var[1] * 100, 1)
pc2_v <- round(pc_var[2] * 100, 1)

# Create new df for plotting scores
pc_score <- as.data.frame(pc$x[, 1:2])
pc_score$Sitefac <- pc_in$Sitefac
pc_score$Date <- pc_in$Date
pc_score$week <- pc_in$week

# df for plotting each var within the PC
pc_load <- as.data.frame(pc$rotation[, 1:2])
scaling_factor <- 1.2 * max(abs(pc_score[, 1:2]))
pc_load_scaled <- pc_load*scaling_factor

# Calculate convex hull by site
conv_hull <- pc_score %>% group_by(Sitefac) %>% slice(chull(PC1, PC2)) %>% subset(select=-c(week))

### Plotting PCA ----

# All sites with conv hulls
(pcaplot1 <- ggplot()+
  geom_point(data=pc_score, aes(x=PC1, y=PC2, color=Sitefac, shape=Sitefac))+
  # geom_polygon(data=conv_hull, aes(x=PC1, y=PC2, fill=Sitefac, color=Sitefac),
  #              alpha=0.1)+
  stat_ellipse(data = pc_score %>%
                subset(select = -c(week)), geom = "polygon",
              aes(x = PC1, y = PC2, fill = Sitefac), alpha=0.1) +
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
  theme_bw()+ animCol+animFill+
  scale_shape_manual(values = c(1:14)))

# All sites with only tributary conv hulls
(pcaplot2 <- ggplot()+
    # geom_polygon(data=conv_hull, 
    #              aes(x=PC1, y=PC2, fill=Sitefac),
    #              alpha=0.0, linewidth = NULL)+
    # geom_polygon(data=conv_hull[conv_hull$Sitefac %in% c("FWBN", "KLWW", "CCSYB"),], 
    #              aes(x=PC1, y=PC2, fill=Sitefac, color=Sitefac),
    #              alpha=0.2)+
    stat_ellipse(data = pc_score %>% filter(Sitefac %in% c("FWBN", "KLWW", "CCSYB")) %>%
                   subset(select = -c(week)), geom = "polygon",
                 aes(x = PC1, y = PC2, fill = Sitefac), alpha = 0.2) +
    geom_segment(data=pc_load_scaled, aes(x=0, y=0, xend=PC1, yend=PC2),
                 alpha=0.5, color="black", linewidth=0.8)+
    geom_point(data=pc_score, aes(x=PC1, y=PC2, color=Sitefac, shape=Sitefac))+
    ggrepel::geom_label_repel(data=pc_load_scaled, aes(x=PC1, y=PC2),
                              fill="dimgrey", color="white", 
                              segment.color="dimgrey", alpha=0.8,
                              label=rownames(pc_load_scaled), seed=25)+
    labs(title = "Point Water Quality PCA",
         x=paste0("PC1 (", pc1_v, "% Variance)"),
         y=paste0("PC2 (", pc2_v, "% Variance)"),
         color="Site", fill="Site", shape="Site")+
    theme_bw()+ animCol+ animFill + 
    scale_shape_manual(values = c(1:14)))

# Color code each point to be dominating tributary, highlights SB4, STTD, TEW
pc_score$weekDate <- floor_date(pc_score$Date, "week")
pc_score <- pc_score[order(pc_score$Sitefac, pc_score$week),]
pc_score$pc1_next <- c(pc_score$PC1[-1], NA)
pc_score$pc2_next <- c(pc_score$PC2[-1], NA)

pc_cdec <- cdec_wide %>% subset(select=c(Site_no, Datetime, discharge_cfs)) %>% 
  drop_na(discharge_cfs) %>% filter(Site_no %in% c("RCS", "FRE", "CCY")) %>% 
  mutate(Site_no = factor(Site_no, levels=c("RCS", "FRE", "CCY")))
pc_cdec$weekDate <- floor_date(pc_cdec$Datetime, "week")
pc_cdec <- pc_cdec %>% group_by(weekDate, Site_no) %>%
  summarize(discharge_total = sum(discharge_cfs)) %>% 
  group_by(weekDate) %>% filter(discharge_total == max(discharge_total))

pc_cdec <- left_join(pc_score, pc_cdec, by="weekDate")

pc_cdec$Site_no <- factor(pc_cdec$Site_no, )

(pcaplot3 <- ggplot()+
    # geom_polygon(data=conv_hull[conv_hull$Sitefac %in% c("FWBN", "KLWW", "CCSYB"),], 
    #             aes(x=PC1, y=PC2, fill=Sitefac), color=NA,
    #             alpha=0.2)+
    stat_ellipse(data = pc_score %>% filter(Sitefac %in% c("FWBN", "KLWW", "CCSYB")) %>%
                   subset(select = -c(week)), geom = "polygon",
                 aes(x = PC1, y = PC2, fill = Sitefac), alpha=0.2) +
    geom_point(data=pc_cdec, 
               aes(x=PC1, y=PC2, color=Site_no, shape=Sitefac))+
    geom_point(data=pc_cdec %>% filter(Sitefac %in% c("SB4", "STTD", "TEW")), 
                aes(x=PC1, y=PC2, color=Site_no, shape=Sitefac), size=5)+
    # geom_segment(data=pc_cdec %>% filter(Sitefac == "STTD"),
    #              aes(x=PC1, y=PC2, xend=pc1_next, yend=pc2_next),
    #              alpha=0.5, color="black", arrow=arrow())+
    # geom_polygon(data=conv_hull, 
    #              aes(x=PC1, y=PC2, fill=Sitefac),
    #              alpha=0.0, linewidth = NULL)+
    geom_segment(data=pc_load_scaled, aes(x=0, y=0, xend=PC1, yend=PC2),
                 alpha=0.5, color="black", linewidth=0.8)+
    ggrepel::geom_label_repel(data=pc_load_scaled, aes(x=PC1, y=PC2),
                              fill="dimgrey", color="white", 
                              segment.color="dimgrey", alpha=0.8,
                              label=rownames(pc_load_scaled), seed=25)+
    # geom_text(data = pc_cdec %>% filter(Sitefac == "STTD"), 
    #           aes(x=PC1, y=PC2, label = week), size = 4, vjust = 1, hjust = 1)+
    labs(title = "Point Water Quality PCA",
         x=paste0("PC1 (", pc1_v, "% Variance)"),
         y=paste0("PC2 (", pc2_v, "% Variance)"),
         color="Highest Flow", fill="Source", shape="Site")+
    theme_bw()+ animCol + animFill + guides(size="none")+
    scale_shape_manual(values = c(1:14)))

# Save plot
png("Output/Figures/YBLTE_wq_PCA_%02d.png",
    height = 5.5, width = 6.5, units = "in", res = 1000, family = "serif")

pcaplot1; pcaplot2; pcaplot3

dev.off()

### 3D plot, considers time, highlights STTD and YBLR4

tribs <- c("CCSYB", "KLWW", "FWBN")

trib_cols <- c("#B63679FF","#482878FF", "#FB8861FF")

plotly_col <- c("#B63679FF",  "#440154FF",  "#482878FF", "#453581FF",  "#FB8861FF",
                "#34618DFF", "#2B748EFF",  "#24878EFF", "#1F998AFF", "#40BC72FF",
                "#67CC5CFF",  "#73A32F", "#CBE11EFF", "#FDE725FF")
plotly_col <- setNames(plotly_col, c("FWBN", "FW1", "KLWW","KNG3", "CCSYB",
                                  "CNW","RD22", "YBLR4", "SB4", "AL0",
                                  "LIS", "STTD", "TEW", "TER"))

trib_stats <- pc_score %>%
  filter(Sitefac %in% tribs) %>%
  group_by(Sitefac) %>%
  group_modify(~{
    pts <- st_as_sf(.x, coords = c("PC1","PC2"), crs = NA)
    hull <- st_convex_hull(st_combine(pts))
    tibble(
      geometry = hull,
      area = as.numeric(st_area(hull)),
      radius = sqrt(as.numeric(st_area(hull)) / pi),
      cx = st_coordinates(st_centroid(hull))[1],
      cy = st_coordinates(st_centroid(hull))[2]
    )
  })

make_cylinder <- function(cx, cy, r, zmin, zmax, n = 60) {
  theta <- seq(0, 2*pi, length.out = n)
  x_bottom <- cx + r * cos(theta)
  y_bottom <- cy + r * sin(theta)
  z_bottom <- rep(zmin, n)
  x_top <- cx + r * cos(theta)
  y_top <- cy + r * sin(theta)
  z_top <- rep(zmax, n)
  x <- c(x_bottom, x_top)
  y <- c(y_bottom, y_top)
  z <- c(z_bottom, z_top)
  i <- c(); j <- c(); k <- c()
  for (t in 1:(n-1)) {
    i <- c(i, t, t+1)
    j <- c(j, t+1, t+1+n)
    k <- c(k, t+n, t+n)
  }
  i <- c(i, n, 1, n)
  j <- c(j, 1, 1+n, 1+n)
  k <- c(k, n+n, n+1, n+n)
  list(x = x, y = y, z = z, i = i-1, j = j-1, k = k-1)
}

p <- plot_ly() %>%
  add_trace(
    data = pc_score,
    x = ~PC1, y = ~PC2, z = ~week,
    color = ~Sitefac,
    colors = plotly_col,
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 3),
    opacity = 0.7
  ) %>%
  layout(
    scene = list(
      xaxis = list(title = "PC1"),
      yaxis = list(title = "PC2"),
      zaxis = list(title = "Week"),
      aspectmode = "manual",
      aspectratio = list(x = 2, y = 2, z = 0.7)
    )
  )


for(i in 1:nrow(trib_stats)) {
  t <- trib_stats[i, ]
  trib_col <- trib_cols[i]
  cyl <- make_cylinder(
    cx = t$cx,
    cy = t$cy,
    r  = t$radius,
    zmin = min(pc_score$week),
    zmax = max(pc_score$week)
  )
  p <- p %>%
    add_trace(
      x = cyl$x,
      y = cyl$y,
      z = cyl$z,
      i = cyl$i,
      j = cyl$j,
      k = cyl$k,
      type = "mesh3d",
      opacity = 0.15,
      facecolor = rep(trib_col, length(cyl$i)),
      name = paste(t$Sitefac, "region")
    )
}

for(s in unique(pc_score$Sitefac)) {
  df <- pc_score %>% filter(Sitefac == s) %>% arrange(week)
  p <- p %>%
    add_trace(
      data = df,
      x = ~PC1,
      y = ~PC2,
      z = ~week,
      color = ~Sitefac,
      colors = plotly_col,
      type = "scatter3d",
      mode = "lines",
      line = list(width = 4),
      opacity = 0.5,
      name = paste(s, "trajectory"),
      showlegend = TRUE
    )
}

p <- p %>% layout(
  legend = list(y = 1, itemsizing = "constant")
)

p

## Animations ----

### PCA animation plots, will go by week (starts at 2 where no missing data) ----
pcaplots <- ggplot()+
  # Conv hulls for tributaries
  # geom_polygon(data=conv_hull%>% filter(Sitefac %in% c("FWBN", "KLWW", "CCSYB")), 
  #              aes(x=PC1, y=PC2, fill=Sitefac), alpha=0.3)+
  stat_ellipse(data = pc_score %>% filter(Sitefac %in% c("FWBN", "KLWW", "CCSYB")) %>%
                 subset(select = -c(week)), geom="polygon",
               aes(x = PC1, y = PC2, fill = Sitefac), alpha=0.2) +
  # Contributing variables to PC (lines)
  geom_segment(data=pc_load_scaled, aes(x=0, y=0, xend=PC1, yend=PC2),
               alpha=0.2, color="black", linewidth=0.8)+
  # Background points
  geom_point(data=pc_score%>% subset(select=-c(week)), aes(x=PC1, y=PC2, 
      color=Sitefac, shape=Sitefac), alpha=0.4)+
  # Variable labels
  ggrepel::geom_label_repel(data=pc_load_scaled, aes(x=PC1, y=PC2),
                            fill="dimgrey", color="white",
                            segment.color="dimgrey", alpha=0.5,
                            label=rownames(pc_load_scaled), seed=25)+
  # Animated points, sites of interest are bigger
  geom_point(data=pc_score, aes(x=PC1, y=PC2, color=Sitefac, shape=Sitefac, 
             size=ifelse(Sitefac %in% c("FWBN", "KLWW", "CCSYB"), 2, 1)), stroke=2,
             alpha = 0.7)+
  labs(title = "Point Water Quality PCA during Week {round(frame_time, 0)}",
       x=paste0("PC1 (", pc1_v, "% Variance)"),
       y=paste0("PC2 (", pc2_v, "% Variance)"),
       color="Site", shape="Site")+guides(fill = "none", size = "none")+
  theme_bw()+ animCol+ animFill +
  scale_shape_manual(values = c(2:15))

# Highlights STTD
pcaplots_sttd <- ggplot()+
  # Conv hulls for tributaries
  # geom_polygon(data=conv_hull%>% filter(Sitefac %in% c("FWBN", "KLWW", "CCSYB")), 
  #              aes(x=PC1, y=PC2, fill=Sitefac), alpha=0.3)+
  stat_ellipse(data = pc_score %>% filter(Sitefac %in% c("FWBN", "KLWW", "CCSYB")) %>%
                 subset(select = -c(week)), geom="polygon",
               aes(x = PC1, y = PC2, fill = Sitefac), alpha=0.2) +
  # Contributing variables to PC (lines)
  geom_segment(data=pc_load_scaled, aes(x=0, y=0, xend=PC1, yend=PC2),
               alpha=0.2, color="black", linewidth=0.8)+
  # Background points
  geom_point(data=pc_score%>% subset(select=-c(week)), aes(x=PC1, y=PC2, 
                                                           color=Sitefac, shape=Sitefac), alpha=0.4)+
  # Tracks path of STTD
  geom_path(data=pc_score[pc_score$Sitefac %in% "STTD",] %>% 
              subset(select=-c(week)), 
            aes(x=PC1, y=PC2, color=Sitefac), alpha=0.4, linewidth= 1.2)+
  # Variable labels
  ggrepel::geom_label_repel(data=pc_load_scaled, aes(x=PC1, y=PC2),
                            fill="dimgrey", color="white",
                            segment.color="dimgrey", alpha=0.5,
                            label=rownames(pc_load_scaled), seed=25)+
  # Animated points, sites of interest are bigger
  geom_point(data=pc_score, aes(x=PC1, y=PC2, color=Sitefac, shape=Sitefac, 
                                size=ifelse(Sitefac %in% c("FWBN", "KLWW", "CCSYB", "STTD"), 2, 1)), stroke=2,
             alpha = 0.7)+
  labs(title = "Point Water Quality PCA during Week {round(frame_time, 0)}",
       x=paste0("PC1 (", pc1_v, "% Variance)"),
       y=paste0("PC2 (", pc2_v, "% Variance)"),
       color="Site", shape="Site")+guides(fill = "none", size = "none")+
  theme_bw()+ animCol+ animFill +
  scale_shape_manual(values = c(2:15))

# Highlights LEBLS
pcaplots_lebls <- ggplot()+
  # Convex hulls for tributaries
  # geom_polygon(
  #   data = conv_hull %>% filter(Sitefac %in% c("FWBN","KLWW","CCSYB")),
  #   aes(x = PC1, y = PC2, fill = Sitefac), alpha = 0.3
  # ) +
  stat_ellipse(data = pc_score %>% filter(Sitefac %in% c("FWBN", "KLWW", "CCSYB")) %>%
                 subset(select = -c(week)), geom="polygon",
               aes(x = PC1, y = PC2, fill = Sitefac), alpha=0.2) +
  # Contributing variables to PC (lines)
  geom_segment(
    data = pc_load_scaled,
    aes(x = 0, y = 0, xend = PC1, yend = PC2),
    alpha = 0.2, color = "black", linewidth = 0.8
  ) +
  # Background points
  geom_point(
    data = pc_score %>% subset(select = -c(week)),
    aes(x = PC1, y = PC2, color = Sitefac, shape = Sitefac),
    alpha = 0.4
  ) +
  # Tracks paths of SB4 + YBLR4 
  geom_path(
    data = pc_score[pc_score$Sitefac %in% c("SB4","YBLR4"),] %>% subset(select = -c(week)),
    aes(x = PC1, y = PC2, color = Sitefac),
    alpha = 0.6, linewidth = 1.4
  ) +
  # Variable labels
  ggrepel::geom_label_repel(
    data = pc_load_scaled,
    aes(x = PC1, y = PC2),
    fill = "dimgrey", color = "white",
    segment.color = "dimgrey", alpha = 0.5,
    label = rownames(pc_load_scaled), seed = 25
  ) +
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


# Animate PCA plots
pcgif <- animate(
  pcaplots + transition_time(week) + enter_fade() + exit_fade(),
  height = 500, width = 600, fps = 10
)

pcgif_sttd <- animate(
  pcaplots_sttd + transition_time(week) + enter_fade() + exit_fade(),
  height = 500, width = 600, fps = 10
)

pcgif_lebls <- animate(
  pcaplots_lebls + transition_time(week) + enter_fade() + exit_fade(),
  height = 500, width = 600, fps = 10
)

### Stage plots, animated by date (lines up with PCA plots because same time frame) ----

# Get stage for tributaries in same time frame as PCA
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

# Animate stage plot
stggif <- animate(
  stageplot+transition_reveal(Datetime),
  height=400, width=600, fps = 10)

### Flow plots, animated by date ----

# Get flow for tributaries in same time frame as PCA
flow_in_time <- cdec_wide %>% filter(Site_no %in% c("RCS", "FRE", "CCY", "PTC")) %>% 
  filter(between(Datetime, min(pc_score$Date), max(pc_score$Date))) %>% drop_na(discharge_cfs)

# Flow plot raw
(tribflowplot1 <- ggplot(flow_in_time, 
                        aes(x = Datetime, y = discharge_cfs, color = Site_no)) + 
    geom_line() + animCol +
    theme_bw() + labs(title = "Discharge smoothed", y = "Discharge (cfs)", x = NULL))

# Edited flow plot, allows FRE to go out of frame for other details
(tribflowplot2 <- ggplot(flow_in_time, 
                         aes(x = Datetime, y = discharge_cfs, color = Site_no)) + 
    geom_line() + animCol + animFill +
    # Text for when FRE is out of frame
    geom_text(data = flow_in_time %>% filter(Site_no=="FRE"),
             aes(x = as.POSIXct("2025-12-01"),
             y = max((flow_in_time %>% filter(Site_no!="FRE"))$discharge_cfs),
             label = ifelse(discharge_cfs>max((flow_in_time %>% filter(Site_no!="FRE"))$discharge_cfs),
                            paste0("FRE at ", discharge_cfs), "")), show.legend = F) +
    # Fill under line
    geom_ribbon(aes(ymin=min(discharge_cfs),ymax=discharge_cfs, fill=Site_no), 
                alpha=0.1, outline.type="lower") +
    # Frame limits, allow FRE to break out of frame
    coord_cartesian(ylim=c(min(flow_in_time$discharge_cfs), 
        max((flow_in_time %>% filter(Site_no!="FRE"))$discharge_cfs)), clip = "off") +
    theme_bw() + labs(title = "Tributary Flow", y = "Discharge (cfs)", x = NULL))

# Plotting percent flow
flow_in_time$Date <- as.Date(flow_in_time$Datetime)
flow_in_time$Site_no <- factor(flow_in_time$Site_no, levels = c("RCS", "FRE", "CCY", "PTC"))

# If negative flow, set to 0 (in theory, not contributing)
flow_zero <- flow_in_time
flow_zero[flow_zero$discharge_cfs<0, 'discharge_cfs'] <- 0

# Percent flow, get daily median per group then divide by sum of daily medians
flow_perc <- flow_zero %>% group_by(Date, Site_no) %>% 
  summarize(median_flow = median(discharge_cfs)) %>% 
  group_by(Date) %>% 
  mutate(sumflow = sum(median_flow), percflow = 100*median_flow/sumflow)

# Percent flow plot, stacked bar plot (daily increments)
pflowplot <- ggplot(data = flow_perc, aes(x = Date, y = percflow, group = Site_no, fill = Site_no)) +
  geom_bar(stat = "identity", alpha = 0.9) + animFill +
  labs(x = NULL, y = "Percent Flow") + theme_bw()

# Animate flow and percent flow plots
flowgif <- animate(
  tribflowplot2+transition_reveal(Datetime),
  height=300, width=600, fps = 10)
pflowgif <- animate(
  pflowplot+transition_states(Date)+shadow_mark(past=T),
  height=300, width=600, fps = 10)

### Combine pca and flow animations ----
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
  animation_lebls <- c(animation_lebls, combined_gif)
}

# Combine pca and flow animations horizontally
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
animation_h <- image_append(c(pcgif[1], right_col), stack = FALSE)
animation_lebls <- image_append(c(pcgif_lebls[1], right_col), stack = FALSE)

# Loop through remaining frames
for(i in 2:nframes){
  
  flow_resized  <- image_resize(flowgif[i],  paste0(pc_width, "x", half_height, "!"))
  pflow_resized <- image_resize(pflowgif[i], paste0(pc_width, "x", half_height, "!"))
  
  right_col <- image_append(c(flow_resized, pflow_resized), stack = TRUE)
  
  combined <- image_append(c(pcgif[i], right_col), stack = FALSE)
  
  animation_h <- c(animation_h, combined)
}

# Loop through remaining frames
for(i in 2:nframes){
  
  flow_resized  <- image_resize(flowgif[i],  paste0(pc_width, "x", half_height, "!"))
  pflow_resized <- image_resize(pflowgif[i], paste0(pc_width, "x", half_height, "!"))
  
  right_col <- image_append(c(flow_resized, pflow_resized), stack = TRUE)
  
  combined <- image_append(c(pcgif_lebls[i], right_col), stack = FALSE)
  
  animation_lebls <- c(animation_lebls, combined)
}

# Save as gif
anim_save("Output/Figures/pca_flow.gif", animation)
anim_save("Output/Figures/pca_flow_horizontal2.gif", animation_h)
anim_save("Output/Figures/pca_flow_horizontal2_lebls.gif", animation_lebls)

# Save as mp4
anim_save("Output/Figures/pca_flow.mp4", animation, renderer = ffmpeg_renderer(codec = "libx264"))
anim_save("Output/Figures/pca_flow_horizontal2.mp4", animation_h, renderer = ffmpeg_renderer(codec = "libx264"))
anim_save("Output/Figures/pca_flow_horizontal2_lebls.mp4", animation_lebls, renderer = ffmpeg_renderer(codec = "libx264"))

