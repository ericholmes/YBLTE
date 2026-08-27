# Edits to plots for BDSC poster!

### Load libraries
library(tidyverse)
library(sf)
library(ggrepel)
library(ggspatial)
library(readxl)
library(deltamapr)
source("Code/YBLTE_useful_functions.R")
library(lubridate)
library(vegan)
library(reshape2)
library(scales)
library(plotly)
library(dtw)
library(pheatmap)
library(dtwclust)
library(ggtern)
library(purrr)
library(patchwork)

### Define poster specific parameters
tribs <- c("FWBN", "KLWW", "CCSYB")
channel <- c("RD22", "AL0", "LIS", "STTD")
offchannel <- c("YBLR4", "SB4", "TEW")
sites <- c(tribs, channel, offchannel)

startdate <- as.Date("2025-12-01")
enddate <- as.Date("2026-04-01")

satellitedates <- c("2026-01-08", "2026-02-12", "2026-03-04", "2026-03-09") %>% as.Date()

cols <- scale_color_manual(values = c("RCS" = "#0D0887FF",
                                       "FRE" = "#CC4678FF",
                                       "CCY" = "#F89441FF",
                                       "PTC" = "#B3BA18",
                                       "FWBN" = "#CC4678FF", 
                                       "KLWW" = "#0D0887FF",
                                       "CCSYB" = "#F89441FF",
                                       "RD22" = "#46337EFF", 
                                       "AL0" = "#365C8DFF", 
                                       "LIS" = "#277F8EFF",
                                       "STTD" = "#1FA187FF",
                                       "YBLR4" = "#4AC16DFF", 
                                       "SB4" = "#9FDA3AFF", 
                                       "TEW" = "#C4B31D"))
fills <- scale_fill_manual(values = c("RCS" = "#0D0887FF",
                                      "FRE" = "#CC4678FF",
                                      "CCY" = "#F89441FF",
                                      "PTC" = "#F0F921FF",
                                      "FWBN" = "#CC4678FF", 
                                      "KLWW" = "#0D0887FF",
                                      "CCSYB" = "#F89441FF",
                                      "RD22" = "#46337EFF", 
                                      "AL0" = "#365C8DFF", 
                                      "LIS" = "#277F8EFF",
                                      "STTD" = "#1FA187FF",
                                      "YBLR4" = "#4AC16DFF", 
                                      "SB4" = "#9FDA3AFF", 
                                      "TEW" = "#FDE725FF"))

### map
# Read files
proj_raw <- read_excel("Data/tabular/YBLTE_sites.xlsx", sheet = 1)
load(file = "data/spatial/Yolo_map_data.Rdata")

# Convert to sf points (WGS84)
proj_pts <- proj_raw %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326, remove = FALSE)

# Filter data points and add clusters
proj_pts <- proj_pts %>% filter(Site_id %in% sites)
proj_pts$Cluster <- "Channel"
proj_pts[proj_pts$Site_id %in% offchannel, "Cluster"] <- "Off-channel"
proj_pts[proj_pts$Site_id %in% tribs, "Cluster"] <- "Tributary"

# Subset bypasses to only Yolo
yolo_bypass <- bypasses[bypasses$Feature_Name %in% 
                          c("Sacramento Bypass", "Yolo Bypass", "Yolo Bypass and Cache Slough"), ]

# Set CRS of water data
WW_Watershed_wgs84 <- st_transform(WW_Watershed, st_crs(yolo_bypass))

# Ridgecut area
ridgecut <- read_sf("Data/spatial/Ridgecut_ToeDrain.geojson")

# Plot map
tiff("BDSC/YBLTE_Sites%02da.tif",
     height = 6, width = 6, units = "in", res = 1000, family = "serif", compression = "lzw")

ggplot() + 
  geom_sf(data = yolo_bypass, aes(fill = 'a'), color = NA) +
  scale_fill_manual(values = c('a' = alpha('#33599C', 0.5)), 
                    labels = c("Yolo Bypass"), name = NULL) +
  ggnewscale::new_scale_fill() + 
  
  geom_sf(data = nwi, aes(fill = "nwi"), color = NA, alpha = 0.6) +
  scale_fill_manual(
    values = c("nwi" = "forestgreen"),
    labels = c("Wetland"),
    name = NULL
  ) + ggnewscale::new_scale_color() + ggnewscale::new_scale_fill() + 
  
  geom_sf(data = cdl_sf, aes(fill = "b"), color = NA, alpha = 0.9) +
  scale_fill_manual(values = c('b' = 'wheat2'), labels = c("Rice Field"), name = NULL) +
  
  geom_sf(data = WW_Watershed_wgs84, fill = "#33599C", color = "#33599C") +
  geom_sf(data = rivers_major, color = "#33599C") +
  geom_sf(data = ridgecut, color = "#33599C") +
  
  geom_sf(data = roads_filtered, color = "grey60") +
  ggnewscale::new_scale_fill() + theme_bw() +
  
  geom_sf(data = proj_pts, 
          aes(shape = Cluster, fill = Cluster), size = 5, linewidth = 2) +
  scale_shape_manual(values = 21:23) +
  scale_fill_manual(values = c(alpha('steelblue', 0.6), alpha('gold', 0.6), alpha('purple', 0.6))) +
  
  geom_text(aes(x = -121.837, y = 38.705, label = "Cache\nCreek"), 
                  data = NULL, color = "#1A3057", size = 3, fontface = "bold",
                  bg.color = "white", bg.r = 0.1, angle = 45) +
  geom_text_repel(aes(x = -121.87, y = 38.541, label = "Putah Creek"), 
                  data = NULL, color = "#1A3057", size = 3, fontface = "bold",
                  bg.color = "white", bg.r = 0.1, angle = -10) +
  geom_text_repel(aes(x = -121.72, y = 38.77, label = "Ridgecut\nSlough"), 
                  data = NULL, color = "#1A3057", size = 3, fontface = "bold",
                  bg.color = "white", bg.r = 0.1, force = 0, hjust = "right") +
  geom_text_repel(aes(x = -121.671, y = 38.83, label = "Sacramento River"), 
                  data = NULL, color = "#1A3057", size = 3, fontface = "bold",
                  bg.color = "white", bg.r = 0.1, force = 0, hjust = "right") +
  geom_text_repel(aes(x = -121.63, y = 38.825, label = "Feather\nRiver"), 
                  data = NULL, color = "#1A3057", size = 3, fontface = "bold",
                  bg.color = "white", bg.r = 0.1, force = 0, hjust = "left") +
  geom_text_repel(data = proj_pts %>% filter(Sampletype=="main"), 
                  aes(geometry = geometry, label = Site_id), 
                  stat = "sf_coordinates", size = 4, bg.color = alpha("white", 0.6),
                  color = "black", bg.r = 0.1, fontface = "bold") +
  coord_sf(xlim = c(-121.9, -121.4), ylim = c(38.15, 38.85), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.2, line_width = 1) + 
  annotation_north_arrow(location = "bl", which_north = "false", 
                         style = north_arrow_fancy_orienteering(),
                         height = unit(0.3,"in"), width = unit(0.3,"in"),
                         pad_x = unit(0.06, "in"), pad_y = unit(0.25, "in")) + 
  labs(title = "Yolo Bypass Lower Trophic Expansion Sites",
       x = NULL, y = NULL, shape = "Site Type", fill = "Site Type", label = "")

dev.off()

### Flow
# Access data
cdec_stations <- c("RCS", "FRE", "CCY", "PTC")

# sensor is parameter name, sensor_num is for access
sensor_codes <- data.frame(sensor = c("discharge_cfs"), 
                           sensor_num = c(20))
cdec <- data.frame()
for(station in cdec_stations){
  try(cdec <- rbind(cdec, downloadCDEC(site_no = station, parameterCd = 20, startDT = startdate , endDT = enddate)))
}

cdec$Param_val <- as.numeric(cdec$Param_val)

cdecmerge <- merge(cdec, sensor_codes, by.x = "parameterCd", by.y = "sensor_num")

# Pivoting from long to wide
cdec_wide <- cdecmerge %>% select(-parameterCd) %>% 
  pivot_wider(names_from = sensor, values_from = Param_val)

cdec_wide$Date <- as.Date(cdec_wide$Datetime)

cdec_wide$Site_no <- factor(cdec_wide$Site_no, levels = c("RCS", "FRE", "CCY", "PTC"))

cdec_wide <- cdec_wide %>% drop_na(discharge_cfs)

# Plot flow
(tribflowplot1 <- ggplot(cdec_wide, 
                         aes(x = Datetime, y = discharge_cfs, color = Site_no)) + 
    geom_line() + cols + fills +
    # Fill under line
    geom_ribbon(aes(ymin=0, ymax=discharge_cfs, fill=Site_no), 
                alpha=0.1, outline.type="lower") +
    # Frame limits, allow FRE to break out of frame
    coord_cartesian(ylim=c(0, max((cdec_wide %>% filter(Site_no!="FRE"))$discharge_cfs)), clip = "off") +
    geom_vline(xintercept = satellitedates, color = "cornflowerblue", linewidth = 1.5, alpha = 0.7) +
    theme_bw() + labs(title = "Tributary Flow", y = "Discharge (cfs)",
                      color = "Water Source", fill = "Water Source", x = NULL))

# Plotting percent flow
# If negative flow, set to 0 (in theory, not contributing)
flow_zero <- cdec_wide
flow_zero[flow_zero$discharge_cfs<0, 'discharge_cfs'] <- 0

# Percent flow, get daily median per group then divide by sum of daily medians
flow_perc <- flow_zero %>% group_by(Date, Site_no) %>% 
  summarize(median_flow = median(discharge_cfs)) %>% 
  group_by(Date) %>% 
  mutate(sumflow = sum(median_flow), percflow = 100*median_flow/sumflow)

# Percent flow plot, stacked bar plot (daily increments)
pflowplot <- ggplot(data = flow_perc, aes(x = Date, y = percflow, group = Site_no, fill = Site_no)) +
  geom_bar(stat = "identity", alpha = 0.7, width = 1) + fills +
  geom_vline(xintercept = satellitedates, color = "cornflowerblue", linewidth = 1.5, alpha = 0.7) +
  labs(title = " ", x = NULL, y = "Percent Flow", fill = "Water Source") + theme_bw()

### pt wq heat maps
# Load data
wqp_raw <- readxl::read_excel("Data/tabular/YBLTE_point_wq.xlsx")

# Clean data
wq <- wqp_raw %>% filter(Sample_Type=="zoop") %>% 
  mutate(
    Date = as.Date(Date),
    datetime = as_datetime(Date),
    fdom_qsu = suppressWarnings(as.numeric(fdom_qsu)),
    Zoop_score = as.numeric(Zoop_score)
  ) %>% 
  filter(Site %in% sites)

wq$Cluster <- "Channel"
wq[wq$Site %in% offchannel, "Cluster"] <- "Off-channel"
wq[wq$Site %in% tribs, "Cluster"] <- "Tributary"

wq$Sitefac <- factor(wq$Site, levels = c(sites))

cluster_ax_col <- c(rep("gold3", times=3), rep("steelblue", times=4), rep("purple", times=3))

wqp <- wq %>% filter(between(Date, as.Date(startdate), as.Date(enddate)))
wqp$week <- as.integer(format(wqp$Date, format = "%W"))
wqp$week <- ifelse(wqp$week>=43, wqp$week-43, wqp$week+9)
wqp$weekchr <- as.character(wqp$week)

# Plot heat maps
# Temperature
(tempplotdate <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Date, y = Sitefac, fill = Temp)) + 
    geom_tile(width = 7) + labs(x = NULL, y=NULL, fill = "Temp (C)") +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev) + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    geom_hline(yintercept = c(3.5, 7.5)) +
    geom_hline(yintercept = c(8.5, 9.5), linetype = 4) + 
    geom_vline(xintercept = satellitedates, color = "cornflowerblue", linewidth = 1.5, alpha = 0.8) +
    theme(axis.text.y =  element_text(color = cluster_ax_col)))

# Dissolved oxygen
(doplotdate <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Date, y = Sitefac, fill = DO_mgl)) + 
    geom_tile(width = 8) + labs(title = " ", x = NULL, y=NULL, fill = "DO (mg/l)") +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev) + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    geom_hline(yintercept = c(3.5, 7.5)) +
    geom_hline(yintercept = c(8.5, 9.5), linetype = 4) + 
    geom_vline(xintercept = satellitedates, color = "cornflowerblue", linewidth = 1.5, alpha = 0.8) +
    theme(axis.text.y =  element_text(color = cluster_ax_col)))

# Specific conductivity
(spcplotdate <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Date, y = Sitefac, fill = SPC_uscm)) + 
    geom_tile(width = 8) + labs(title = "Point Water Quality", x = NULL, y=NULL, fill = "SPC (us/cm)") +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev) + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_fill_gradientn(colors = viridis::plasma(3), limits = c(100, 1000),
                         breaks = c(seq(100,1000,250)), na.value = "#FDE725FF") +
    geom_hline(yintercept = c(3.5, 7.5)) +
    geom_hline(yintercept = c(8.5, 9.5), linetype = 4) + 
    geom_vline(xintercept = satellitedates, color = "cornflowerblue", linewidth = 1.5, alpha = 0.8) +
    theme(axis.text.y =  element_text(color = cluster_ax_col)))

# Turbidity
(turbplotdate <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Date, y = Sitefac, fill = Turb_fnu)) + 
    geom_tile(width = 8) + labs(x = NULL, y=NULL, fill = "Turb (FNU)") +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev) + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    geom_hline(yintercept = c(3.5, 7.5)) +
    geom_hline(yintercept = c(8.5, 9.5), linetype = 4) + 
    geom_vline(xintercept = satellitedates, color = "cornflowerblue", linewidth = 1.5, alpha = 0.8) +
    theme(axis.text.y =  element_text(color = cluster_ax_col)))

# Fluorescent dissolved organic matter (FDOM)
(fdomplotdate <- ggplot(wqp %>% drop_na(c(Sitefac, fdom_qsu)), aes(x = Date, y = Sitefac, fill = fdom_qsu)) + 
    geom_tile(width = 7) + labs(x = NULL, y=NULL, fill = "FDOM (QSU)") +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev) + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    geom_hline(yintercept = c(3.5, 7.5)) +
    geom_hline(yintercept = c(8.5, 9.5), linetype = 4) + 
    geom_vline(xintercept = satellitedates, color = "cornflowerblue", linewidth = 1.5, alpha = 0.8) +
    theme(axis.text.y =  element_text(color = cluster_ax_col)))

# Chlorophyll-a
(chlplotdate <- ggplot(wqp %>% drop_na(Sitefac), aes(x = Date, y = Sitefac, fill = CHL_ugl)) + 
    geom_tile(width = 8) + labs(x = NULL, y= NULL, fill = "Chl (ug/l)") +
    theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev) + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    geom_hline(yintercept = c(3.5, 7.5)) +
    geom_hline(yintercept = c(8.5, 9.5), linetype = 4) + 
    geom_vline(xintercept = satellitedates, color = "cornflowerblue", linewidth = 1.5, alpha = 0.8) +
    theme(axis.text.y =  element_text(color = cluster_ax_col)))

# Zooplankton score (1-5)
# (zoopplotdate <- ggplot(wqp %>% drop_na(c(Sitefac, Zoop_score)), aes(x = Date, y = Sitefac, fill = Zoop_score)) +
#     geom_tile(width = 8) + labs(x = NULL, y=NULL, fill = "Zoop score") +
#     theme_bw() + scale_fill_viridis_c(option = "C") + scale_y_discrete(limits = rev) +
#     scale_x_date(date_breaks = "1 month", date_labels = "%b") +
#     geom_hline(yintercept = c(3.5, 7.5)) +
#     geom_hline(yintercept = c(8.5, 9.5), linetype = 4) +
#     geom_vline(xintercept = satellitedates, color = "cornflowerblue", linewidth = 1.5, alpha = 0.8) +
#     theme(axis.text.y =  element_text(color = cluster_ax_col)))

### Zooplankton Data
load("Data/YBLTE_zoop_clean.RData")

zoop_weekly_group <- zoop_weekly_group %>% filter(Site %in% sites)
zoop_weekly_group$Cluster <- "Channel"
zoop_weekly_group[zoop_weekly_group$Site %in% offchannel, "Cluster"] <- "Off-channel"
zoop_weekly_group[zoop_weekly_group$Site %in% tribs, "Cluster"] <- "Tributary"

zoop_weekly_group <- zoop_weekly_group %>% filter(between(Date, as.Date(startdate), as.Date(enddate)))

zoop_weekly_group$Site <- factor(zoop_weekly_group$Site, levels = c(sites))

(zoopqplotdate <- ggplot(zoop_weekly_group %>% filter(group=="Large cladocera"), aes(x = Date, y = Site, fill = totezoop, color = "")) +
    geom_tile(width = 7) + scale_fill_gradientn(trans = "log10", colors = viridis::plasma(3), limits = c(1000, NA),
                                                breaks = c(1e3, 1e4, 1e5), labels = c("1k", "10k", "100k")) + 
    labs(x = NULL, y=NULL, fill = bquote("Large\nCladocera "(m^-3))) +
    theme_bw() + scale_y_discrete(limits = rev) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    geom_hline(yintercept = c(3.5, 7.5)) +
    geom_hline(yintercept = c(8.5, 9.5), linetype = 4) +
    geom_vline(xintercept = satellitedates, color = "cornflowerblue", linewidth = 1.5, alpha = 0.8) +
    theme(axis.text.y =  element_text(color = cluster_ax_col)) +
    scale_color_manual(values=NA) + guides(color=guide_legend("<1k", override.aes=list(fill="grey50"))))

### combined wq
# png("BDSC/YBLTE_Point_wq_%02d.png",
#     height = 10, width = 12, units = "in", res = 1000, family = "serif")

# (cowplot::plot_grid(tribflowplot1 + guides(fill = "none"),
#                                       pflowplot,
#                                       tempplotdate + labs(title = "Point Water Quality") + theme(axis.text.x = element_blank()),
#                                       turbplotdate + theme(axis.text.x = element_blank()),
#                                       doplotdate,
#                                       fdomplotdate,
#                                       spcplotdate + theme(axis.text.x = element_blank()),
#                                       chlplotdate + theme(axis.text.x = element_blank()),
#                                       zoopqplotdate,
#                                       align  = "v", ncol = 2))
(cowplot::plot_grid(spcplotdate + theme(axis.text.x = element_blank()),
                    doplotdate + theme(axis.text.x = element_blank()),
                    fdomplotdate + theme(axis.text.x = element_blank()),
                    turbplotdate + theme(axis.text.x = element_blank()),
                    chlplotdate,
                    zoopqplotdate,
                    align  = "v", ncol = 2))

# dev.off()

### pca
# Filter var of interest; temp too variable (discrete data), pH no big influence
# sal and TDS like SPC, pc like chlorop, zoop not at every site
pc_in <- wqp %>% subset(select=c("RowID","Site","Sitefac", "Date", "DO_mgl", "SPC_uscm",          
                          "Turb_fnu", "CHL_ugl", "fdom_qsu", "week"))

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

# PCA plot
pca_plt <- ggplot()+
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
  theme_bw() + cols + fills +
  scale_shape_manual(values = c(1:14))

# Save plot
# png("BDSC/YBLTE_wq_PCA_%02d.png",
#     height = 5.5, width = 6.5, units = "in", res = 1000, family = "serif")

pca_plt

# dev.off()

### 3D plot, considers time, highlights STTD and YBLR4

trib_cols <- c("#CC4678FF","#0D0887FF", "#F89441FF")

plotly_col <- c("#CC4678FF",  "#0D0887FF",  "#F89441FF", 
                "#46337EFF", "#365C8DFF",  "#277F8EFF", "#1FA187FF",
                "#4AC16DFF",  "#9FDA3AFF", "#C4B31D")
plotly_col <- setNames(plotly_col, c("FWBN", "KLWW", "CCSYB",
                                     "RD22", "AL0", "LIS", "STTD", 
                                     "YBLR4", "SB4", "TEW"))

pc_score <- pc_score %>% arrange(Sitefac)

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

# For poster, select channel, off-channel, then everything
p

### DTW
traj_multi <- lapply(sites, function(s) {
  df <- subset(pc_score, Sitefac == s)
  df <- df[order(df$week), ]
  as.matrix(df[, c("PC1", "PC2")])   # multivariate trajectory
})
names(traj_multi) <- as.character(sites)

n <- length(traj_multi)
dtw_multi <- matrix(0, n, n)
rownames(dtw_multi) <- colnames(dtw_multi) <- names(traj_multi)

for(i in 1:n){
  for(j in 1:n){
    dtw_multi[i, j] <- dtw_basic(
      x = traj_multi[[i]],
      y = traj_multi[[j]],
      dist_method = "Euclidean"   # distance between PC1+PC2 vectors
    )
  }
}

an_row <- data.frame(cluster = factor(c(rep("Tributary", times=3), rep("Channel", times=4), rep("Off-channel", times=3))))
rownames(an_row) <- rownames(dtw_multi)

ann_colors = list(
  cluster = c(Tributary = "purple", Channel = "steelblue", `Off-channel` = "gold")
)

# png("BDSC/YBLTE_Point_wq_DTW_clust_mv%02d.png",
#     height = 6, width = 7, units = "in", res = 1000, family = "serif")

pheatmap(
  dtw_multi,
  clustering_distance_rows = "euclidean",
  clustering_distance_cols = "euclidean",
  annotation_row = an_row,
  annotation_colors = ann_colors,
  annotation_legend = F,
  annotation_names_row = F,
  main = "Multivariate DTW Distance (PC1 + PC2)"
)

# dev.off()

### EMMA
wq_emma <- wq %>%
  mutate(
    datetime = as_datetime(Date),
    week = floor_date(datetime, "week"),
    SPC = SPC_uscm,
    FDOM = fdom_qsu
  ) %>%
  filter(!is.na(SPC), !is.na(FDOM))

# weekly endmembers
endm <- wq_emma %>%
  filter(Site %in% tribs) %>%
  group_by(week, Site) %>%
  summarize(SPC = mean(SPC), FDOM = mean(FDOM), .groups = "drop") %>%
  pivot_wider(
    names_from = Site,
    values_from = c(SPC, FDOM),
    names_sep = "_"
  )

# weekly mixing sites
mix <- wq_emma %>%
  filter(!Site %in% tribs) %>%
  group_by(week, Site) %>%
  summarize(SPC = mean(SPC), FDOM = mean(FDOM), .groups = "drop")

# join mixing sites with endmembers
emma_df <- mix %>%
  left_join(endm, by = "week")

# PCA
pca_emma <- prcomp(emma_df %>% select(SPC, FDOM), scale. = TRUE)

emma_df <- emma_df %>%
  mutate(
    PC1 = pca_emma$x[,1],
    PC2 = pca_emma$x[,2]
  )

# Project endmembers into PC space
project_pc <- function(spc, fdom) {
  predict(pca_emma, newdata = data.frame(SPC = spc, FDOM = fdom))
}

endm_pc <- endm %>%
  mutate(
    PC_FWBN  = project_pc(SPC_FWBN,  FDOM_FWBN),
    PC_CCSYB = project_pc(SPC_CCSYB, FDOM_CCSYB),
    PC_KLWW  = project_pc(SPC_KLWW,  FDOM_KLWW)
  ) %>%
  mutate(
    PC1_FWBN  = PC_FWBN[,1],  PC2_FWBN  = PC_FWBN[,2],
    PC1_CCSYB = PC_CCSYB[,1], PC2_CCSYB = PC_CCSYB[,2],
    PC1_KLWW  = PC_KLWW[,1],  PC2_KLWW  = PC_KLWW[,2]
  ) %>%
  select(-PC_FWBN, -PC_CCSYB, -PC_KLWW)

# EMMA solver
solve_emma <- function(px, py, ex1, ey1, ex2, ey2, ex3, ey3) {
  A <- matrix(c(
    ex1, ex2, ex3,
    ey1, ey2, ey3,
    1,   1,   1
  ), 3, 3, byrow = TRUE)
  
  b <- c(px, py, 1)
  
  if (any(is.na(c(A, b)))) return(c(NA, NA, NA))
  
  out <- tryCatch(solve(A, b), error = function(e) c(NA, NA, NA))
  as.numeric(out)
}

# Compute EMMA fractions
emma_fractions <- emma_df %>%
  rowwise() %>%
  mutate(
    frac = list(
      solve_emma(
        PC1, PC2,
        endm_pc$PC1_FWBN[match(week, endm_pc$week)],
        endm_pc$PC2_FWBN[match(week, endm_pc$week)],
        endm_pc$PC1_CCSYB[match(week, endm_pc$week)],
        endm_pc$PC2_CCSYB[match(week, endm_pc$week)],
        endm_pc$PC1_KLWW[match(week, endm_pc$week)],
        endm_pc$PC2_KLWW[match(week, endm_pc$week)]
      )
    ),
    f_FWBN_raw  = frac[1],
    f_CCSYB_raw = frac[2],
    f_KLWW_raw  = frac[3]
  ) %>%
  ungroup()

# Diagnostics + clamping + renormalization
emma_clean <- emma_fractions %>%
  mutate(
    # raw sum
    sum_raw = f_FWBN_raw + f_CCSYB_raw + f_KLWW_raw,
    
    # clamp
    f_FWBN  = pmax(pmin(f_FWBN_raw,  1), 0),
    f_CCSYB = pmax(pmin(f_CCSYB_raw, 1), 0),
    f_KLWW  = pmax(pmin(f_KLWW_raw,  1), 0),
    
    sum_clamp = f_FWBN + f_CCSYB + f_KLWW,
    
    # renormalize for plotting
    f_FWBN_plot  = f_FWBN  / sum_clamp,
    f_CCSYB_plot = f_CCSYB / sum_clamp,
    f_KLWW_plot  = f_KLWW  / sum_clamp,
    
    # diagnostics
    neg_flag = (f_FWBN_raw < 0 | f_CCSYB_raw < 0 | f_KLWW_raw < 0),
    deviation = abs(sum_raw - 1),
    unexplained = pmax(0, 1 - sum_raw),
    
    unexplained_flag = case_when(
      neg_flag ~ "Extrapolated",
      deviation > 0.25 ~ "Large mismatch",
      deviation > 0.10 ~ "Moderate mismatch",
      TRUE ~ "Well explained"
    )
  )

# Percent-source long format
emma_long <- emma_clean %>%
  select(week, Site, f_FWBN_plot, f_CCSYB_plot, f_KLWW_plot) %>%
  pivot_longer(
    cols = starts_with("f_"),
    names_to = "source",
    values_to = "fraction"
  ) %>%
  mutate(
    source = recode(source,
                    f_FWBN_plot  = "FWBN",
                    f_CCSYB_plot = "CCSYB",
                    f_KLWW_plot  = "KLWW"
    ),
    Site = factor(Site, levels = c(channel, offchannel))
  )

# Percent-source time series (channel then offchannel)
(emmaplot <- ggplot(emma_long[is.na(emma_long$Site) == F,], aes(week, fraction, fill = source)) +
   geom_col(alpha=0.7) +
   facet_grid(Site ~ .) + fills +
   labs(
     title = "EMMA Percent Source Contribution",
     x = NULL,
     y = "Fraction",
     fill = "Water Source"
   ) + 
    geom_vline(xintercept = satellitedates, color = "cornflowerblue", linewidth = 1.5, alpha = 0.7) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b-1",
                    limits = c(startdate, enddate)) +
   theme_bw() +
    # Saving with blue then yellow, will paste one over other
    theme(strip.background = element_rect(fill=alpha("steelblue", 0.5))))
                                                    #"gold3", 0.5))))

dates <- unique(data.frame(emma_long[is.na(emma_long$Site) == F,"week"]))

# Add flow
final_plot <- tribflowplot1  /
  pflowplot /
  emmaplot +
  plot_layout(heights = c(1, 1, 5))

png(paste("BDSC/YBLTE_EMMA_%02d.png", sep = ""), 
    height = 10, width = 8, unit = "in", res = 1000)

final_plot

dev.off()

