# Load required libraries
library(tidyverse)
library(sf)
library(nhdplusTools)
library(elevatr)
library(hillshader)
library(raster)
library(terra)
library(tidyterra)
library(ggspatial)
library(ggpattern)
library(ggnewscale)
library(ggrepel)
library(scales)
library(cowplot)

saveplot = T

cache_df <- data.frame(Y =  38.706542, X = -121.817925)
cache_sf <- st_as_sf(cache_df, coords = c("X", "Y"), crs = 4326)

comid <- nhdplusTools::discover_nhdplus_id(cache_sf)
comid

basin <- nhdplusTools::get_nldi_basin(list(featureSource = "comid",
                                           featureID = comid))
cache <- st_make_valid(basin) |> st_cast("POLYGON") |> st_union() |> st_as_sf()

cache_bbox <- st_bbox(cache)
cache_bbox_sf<- st_as_sfc(cache_bbox)

# Get US state map data
us_states <- map_data("state")
cali <- us_states[us_states$region == "california",]
polygon <- st_polygon(list(as.matrix(us_states[us_states$region == "california",c("long", "lat")])))
cali_sf <- st_sf(id = cali$region[1], geometry = st_sfc(polygon), crs = 4326)

#Load rivers
nhd_flow <- nhdplusTools::get_nhdplus(AOI = cache, realization = "flowline")
nhd_flow <- st_transform(nhd_flow, st_crs(cali_sf))
nhd_flow <- merge(nhd_flow, data.frame(streamorde = 1:7, width = rev(c(1.1,1,.8, .6, .4, .2,.1))))

#load lakes
nhd_wb <- nhdplusTools::get_waterbodies(AOI = cache)
nhd_wb <- st_transform(nhd_wb, st_crs(cali_sf))

nhd_wb_centroids <- st_centroid(nhd_wb) #%>% st_coordinates()
nhd_wb_centroids[nhd_wb_centroids$areasqkm == 62.408,"gnis_name"] <- "Lake cache"

# Download elevation data using the 'elevatr' package
elevation_data <- get_elev_raster(locations = cache, z = 9, prj = st_crs(4326)$proj4string)
elevation_data <- mask(elevation_data, cache)

r <- rast(elevation_data)
## Create hillshade effect
slope <- terrain(r, "slope", unit = "radians")
aspect <- terrain(r, "aspect", unit = "radians")
hill <- shade(slope, aspect, 30, 270)

# normalize names
names(hill) <- "shades"

# Hillshading, but we need a palette
pal_greys <- hcl.colors(1000, "Grays")

# Use a vector of colors
index <- hill %>%
  mutate(index_col = rescale(shades, to = c(1, length(pal_greys)))) %>%
  mutate(index_col = round(index_col)) %>%
  pull(index_col)
vector_cols <- pal_greys[index]

##Save map
if(saveplot == T){jpeg("Output/Maps/Draped_hypso_dem_cache_inset_nhdfull.jpg",
                      height = 4, width = 6, units = "in", res = 1000, family = "serif")}

map <- ggplot() +
  geom_spatraster(data = hill, fill = vector_cols, maxcell = Inf, alpha = 1) +
  geom_spatraster(data = r, maxcell = Inf, show.legend = F) +
  scale_fill_hypso_tint_c(limits = c(0, 2730),
                           palette = "wiki-2.0_hypso",
                          # palette = "colombia",
                          # palette = "usgs-gswa2",
                           alpha = 0.6,
                           labels = label_comma(),
                           breaks = c(seq(0, 1000, 200),
                             seq(1100, 2500, 100),
                             2600)) +
  geom_sf(data = nhd_wb[nhd_wb$ftype %in% "LakePond",], color = NA, fill = "darkslategrey") +
  geom_sf(data = nhd_flow, color = "darkslategrey", aes(linewidth = width), show.legend = F) +
  scale_linewidth(range = c(0.05, .3)) +
  geom_sf(data = cache, fill = NA, color = "grey40", linetype = 2) +
  geom_sf(fill = NA, color = "black", linetype = 2) +
  theme_bw() +
  geom_text_repel(data = nhd_wb_centroids[nhd_wb_centroids$areasqkm >14, ], 
                   aes(geometry = geometry, label = gnis_name),stat = "sf_coordinates",
                   bg.color = "white", color = "darkslategrey",
                   bg.r = 0.25, nudge_x = .1, nudge_y = .03, size = 3) +
  coord_sf(xlim = c(cache_bbox["xmin"]-.02, cache_bbox["xmax"]+.02),
    ylim = c(cache_bbox["ymin"]-.02, cache_bbox["ymax"]+.02),
    expand = FALSE) +
  labs(x = "Longitude", y = "Latitude") +
  annotation_scale(location = "bl", width_hint = 0.2, line_width = 1,
                   pad_x = unit(.35, "in")) + 
  annotation_north_arrow(location = "bl", which_north = "false", 
                         style = north_arrow_fancy_orienteering(),
                         height = unit(0.3,"in"), width = unit(0.3,"in"),
                         pad_x = unit(.02, "in"), pad_y = unit(.02, "in"))

inset <- ggplot() +
  geom_sf(data = cali_sf, fill = "gray90") +
  geom_sf(data = cache, fill = "grey20") +
  geom_sf(data = cache_bbox_sf, fill = NA, color = "black") +
  theme_void()

(combined_map <- ggdraw() +
  draw_plot(map) +
  draw_plot(inset, x = .73, y = .65, width = 0.25, height = 0.25))

if(saveplot == T){dev.off()}
