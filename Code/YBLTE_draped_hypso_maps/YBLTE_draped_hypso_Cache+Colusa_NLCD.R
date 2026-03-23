library(sf)
library(nhdplusTools)
library(dplyr)

# Load required libraries
library(tidyverse)
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
library(FedData)
library(geodata)

saveplot = TRUE

# -------------------------
# 1. HUC8s
# -------------------------
cache_huc8  <- nhdplusTools::get_huc(id = "18020116", type = "huc08")
colusa_huc8 <- nhdplusTools::get_huc(id = "18020104", type = "huc08")

watersheds <- rbind(
  cache_huc8  |> mutate(name = "Cache Creek"),
  colusa_huc8 |> mutate(name = "Colusa Basin")
)

watersheds_bbox <- st_bbox(watersheds)
watersheds_bbox_sf <- st_as_sfc(watersheds_bbox)

# -------------------------
# 2. Watershed metrics: AREA
# -------------------------
watersheds_aea <- st_transform(watersheds, 5070)  # NAD83 / Conus Albers
watersheds_metrics <- watersheds_aea |>
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) |>
  st_drop_geometry() |>
  select(name, area_km2) |>
  mutate(area_acres = 247.105381 * area_km2)

# -------------------------
# 3. Landcover (NLCD via FedData, RasterLayer workflow)
# -------------------------

# 1. Download NLCD
nlcd_raster <- get_nlcd(
  template = watersheds,
  label = "cache_colusa",
  year = 2019,
  dataset = "landcover",
  extraction.dir = "NLCD_data"
)

# 2. Reproject NLCD to WGS84 for mapping
nlcd_wgs84 <- project(nlcd_raster, "EPSG:4326", method = "near")

# 3. Reproject watersheds to WGS84
watersheds_wgs84 <- st_transform(watersheds, 4326)

# 4. Quick sanity map
ggplot() +
  geom_spatraster(data = nlcd_wgs84) +
  geom_sf(data = watersheds_wgs84, fill = NA, color = "black", size = 1) +
  theme_minimal() +
  labs(title = "NLCD Land Cover (2019) with Watersheds Overlaid")

# 5. Extract NLCD values per watershed (RasterLayer)
lc_raw <- raster::extract(
  nlcd_raster,
  watersheds,
  df = TRUE
)

# Identify NLCD column name
nlcd_col <- names(lc_raw)[2]

# Rename and clean
lc_raw <- lc_raw |>
  rename(HUC = ID, value = !!nlcd_col) |>
  filter(!is.na(value))

lc_clean <- lc_raw |>
  rename(class = Class) |>
  mutate(
    class = as.character(class),   # convert factor → character
    HUC = ID
  ) |>
  select(HUC, class)

lc_summary <- lc_clean |>
  group_by(HUC, class) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(HUC) |>
  mutate(pct = 100 * count / sum(count)) |>
  left_join(
    watersheds |> st_drop_geometry() |> mutate(HUC = row_number()),
    by = "HUC"
  ) |>
  select(name, class, pct)

nlcd_group_map <- list(
  Agriculture = c("Cultivated Crops", "Pasture/Hay"),
  Forest      = c("Deciduous Forest", "Evergreen Forest", "Mixed Forest"),
  Shrub       = c("Shrub/Scrub"),
  Wetlands    = c("Woody Wetlands", "Emergent Herbaceous Wetlands"),
  Developed   = c("Developed High Intensity",
                  "Developed, Medium Intensity",
                  "Developed, Low Intensity",
                  "Developed, Open Space"),
  Grass       = c("Grassland/Herbaceous"),
  Water       = c("Open Water"),
  Barren      = c("Barren Land (Rock/Sand/Clay)")
)

lc_grouped <- lc_summary |>
  mutate(
    group = case_when(
      class %in% nlcd_group_map$Agriculture ~ "Agriculture",
      class %in% nlcd_group_map$Forest      ~ "Forest",
      class %in% nlcd_group_map$Shrub       ~ "Shrub",
      class %in% nlcd_group_map$Wetlands    ~ "Wetlands",
      class %in% nlcd_group_map$Developed   ~ "Developed",
      class %in% nlcd_group_map$Grass       ~ "Grass",
      class %in% nlcd_group_map$Water       ~ "Water",
      class %in% nlcd_group_map$Barren      ~ "Barren",
      TRUE ~ "Other"
    )
  ) |>
  group_by(name, group) |>
  summarise(pct = sum(pct), .groups = "drop") |>
  tidyr::pivot_wider(
    names_from = group,
    values_from = pct,
    values_fill = 0
  )


# -------------------------
# 4. Precipitation (WorldClim)
# -------------------------
# 1. Download precip
precip <- geodata::worldclim_global(var = "prec", res = 10)

# 2. Compute annual precipitation (sum of 12 months)
annual_precip <- terra::app(precip, sum)

# 3. Loop over watersheds
precip_stats <- lapply(1:nrow(watersheds), function(i) {
  masked <- terra::mask(annual_precip, vect(watersheds[i, ]))
  vals <- values(masked, na.rm = TRUE)
  data.frame(
    name = watersheds$name[i],
    precip_mean_mm = mean(vals)
  )
}) |>
  bind_rows() |>
  mutate(precip_mean_in = 0.03937008 * precip_mean_mm)

# -------------------------
# 5. Elevation + hillshade
# -------------------------
elevation_data <- get_elev_raster(locations = watersheds, z = 9,
                                  prj = st_crs(4326)$proj4string)
elevation_data <- mask(elevation_data, watersheds)

r <- rast(elevation_data)

# Elevation metrics
elev_stats <- lapply(1:nrow(watersheds), function(i) {
  vals <- terra::mask(r, vect(watersheds[i,])) |> values(na.rm = TRUE)
  data.frame(
    name = watersheds$name[i],
    elev_mean_m = mean(vals)
  )
}) |>
  bind_rows() |>
  mutate(elev_mean_ft = 3.2808399 * elev_mean_m)

# -------------------------
# 6. Combine all metrics
# -------------------------
metrics <- watersheds_metrics |>
  left_join(elev_stats, by = "name") |>
  left_join(precip_stats, by = "name") |>
  left_join(lc_grouped, by = "name")

# metrics <- metrics |>
#   left_join(lc_grouped, by = "name")

# -------------------------
# 7. Create label text for map
# -------------------------
watersheds_labels <- st_centroid(watersheds) |>
  left_join(metrics, by = "name") |>
  mutate(
    label = paste0(
      name, "\n",
      "Area: ", round(area_km2), " km²\n",
      "Elev: ", round(elev_mean_m), " m\n",
      "Ppt: ", round(precip_mean_mm), " mm/yr\n",
      "Ag: ", round(Agriculture), "%  ",
      "Forest: ", round(Forest), "%\n",
      "Wetlands: ", round(Wetlands), "%  ",
      "Shrub: ", round(Shrub), "%"
    )
  )

# -------------------------
# 8. Load rivers and lakes
# -------------------------
us_states <- map_data("state")
cali <- us_states[us_states$region == "california",]
polygon <- st_polygon(list(as.matrix(us_states[us_states$region == "california",
                                               c("long", "lat")])))
cali_sf <- st_sf(id = cali$region[1], geometry = st_sfc(polygon), crs = 4326)
watersheds_single <- st_union(watersheds)

nhd_flow <- nhdplusTools::get_nhdplus(AOI = watersheds_single, realization = "flowline")
nhd_flow <- st_transform(nhd_flow, st_crs(cali_sf))
nhd_flow <- merge(nhd_flow,
                  data.frame(streamorde = 1:7,
                             width = rev(c(1.1,1,.8,.6,.4,.2,.1))))

nhd_wb <- nhdplusTools::get_waterbodies(AOI = watersheds_single)
nhd_wb <- st_transform(nhd_wb, st_crs(cali_sf))

nhd_wb_centroids <- st_centroid(nhd_wb)
nhd_wb_centroids[nhd_wb_centroids$areasqkm == 62.408,"gnis_name"] <- "Lake watersheds"

# -------------------------
# 9. Hillshade
# -------------------------
slope <- terrain(r, "slope", unit = "radians")
aspect <- terrain(r, "aspect", unit = "radians")
hill <- shade(slope, aspect, 30, 270)
names(hill) <- "shades"

pal_greys <- hcl.colors(1000, "Grays")
index <- hill %>%
  mutate(index_col = rescale(shades, to = c(1, length(pal_greys)))) %>%
  mutate(index_col = round(index_col)) %>%
  pull(index_col)
vector_cols <- pal_greys[index]

# -------------------------
# 10. Plot
# -------------------------

comparison_table <- metrics |>
  select(
    name,
    area_km2,
    elev_mean_m,
    precip_mean_mm,
    Agriculture,
    Forest,
    Wetlands,
    Shrub
  ) |>
  mutate(
    area_km2 = round(area_km2),
    elev_mean_m = round(elev_mean_m),
    precip_mean_mm = round(precip_mean_mm),
    Agriculture = round(Agriculture),
    Forest = round(Forest),
    Wetlands = round(Wetlands),
    Shrub = round(Shrub)
  ) |>
  pivot_longer(
    cols = -name,
    names_to = "metric",
    values_to = "value"
  ) |>
  pivot_wider(
    names_from = name,
    values_from = value
  ) |>
  mutate(
    metric = recode(metric,
                    area_km2 = "Area (km²)",
                    elev_mean_m = "Elevation (m)",
                    precip_mean_mm = "Precip (mm/yr)",
                    Agriculture = "Agriculture (%)",
                    Forest = "Forest (%)",
                    Wetlands = "Wetlands (%)",
                    Shrub = "Shrub (%)"
    )
  )


library(gridExtra)

table_grob <- tableGrob(
  comparison_table,
  rows = NULL,
  theme = ttheme_minimal(
    base_size = 8,
    padding = unit(c(2, 2), "mm")
  )
)



if (saveplot) {
  png("Output/Maps/Draped_hypso_dem_watersheds_inset_nhdfull_%02d.png",
       height = 6, width = 6, units = "in", res = 1000, family = "serif")
}

map <- ggplot() +
  geom_spatraster(data = hill, fill = vector_cols, maxcell = Inf, alpha = 1) +
  geom_spatraster(data = r, maxcell = Inf, show.legend = FALSE) +
  scale_fill_hypso_tint_c(limits = c(0, 2730),
                          palette = "wiki-2.0_hypso",
                          alpha = 0.6,
                          labels = label_comma(),
                          breaks = c(seq(0, 1000, 200),
                                     seq(1100, 2500, 100),
                                     2600)) +
  geom_sf(data = nhd_wb[nhd_wb$ftype %in% "LakePond",],
          color = NA, fill = "darkslategrey") +
  geom_sf(data = nhd_flow, color = "darkslategrey",
          aes(linewidth = width), show.legend = FALSE) +
  scale_linewidth(range = c(0.05, .3)) +
  geom_sf(data = watersheds, fill = NA, color = "firebrick", linetype = 1, linesize = 3) +
  theme_bw() +
  geom_text_repel(data = nhd_wb_centroids[nhd_wb_centroids$areasqkm > 14 & 
                                            !duplicated(nhd_wb_centroids$gnis_name), ], 
                  aes(geometry = geometry, label = gnis_name),
                  stat = "sf_coordinates",
                  bg.color = "white", color = "darkslategrey",
                  bg.r = 0.25, nudge_x = .1, nudge_y = .03, size = 3) +
  coord_sf(xlim = c(watersheds_bbox["xmin"] - .02, watersheds_bbox["xmax"] + .02),
           ylim = c(watersheds_bbox["ymin"] - .02, watersheds_bbox["ymax"] + .02),
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
  geom_sf(data = watersheds, fill = "grey20") +
  geom_sf(data = watersheds_bbox_sf, fill = NA, color = "black") +
  theme_void()

combined_map <- ggdraw() +
  draw_plot(map) +
  draw_plot(inset, x = .76, y = .65, width = 0.25, height = 0.25) +
  draw_plot(table_grob, x = 0.1, y = 0.70, width = 0.42, height = 0.25)

print(combined_map)

map_nlcd <- ggplot() +
  geom_spatraster(data = nlcd_wgs84, show.legend = F) +
  geom_sf(data = nhd_wb[nhd_wb$ftype %in% "LakePond",],
          color = NA, fill = "darkslategrey") +
  geom_sf(data = nhd_flow, color = "darkslategrey",
          aes(linewidth = width), show.legend = FALSE) +
  scale_linewidth(range = c(0.05, .3)) +
  geom_sf(data = watersheds, fill = NA, color = "firebrick", linetype = 1, size = 3) +
  theme_bw() +
  geom_text_repel(data = nhd_wb_centroids[nhd_wb_centroids$areasqkm > 14 & 
                                            !duplicated(nhd_wb_centroids$gnis_name), ], 
                  aes(geometry = geometry, label = gnis_name),
                  stat = "sf_coordinates",
                  bg.color = "white", color = "darkslategrey",
                  bg.r = 0.25, nudge_x = .1, nudge_y = .03, size = 3) +
  coord_sf(xlim = c(watersheds_bbox["xmin"] - .02, watersheds_bbox["xmax"] + .02),
           ylim = c(watersheds_bbox["ymin"] - .02, watersheds_bbox["ymax"] + .02),
           expand = FALSE) +
  labs(x = "Longitude", y = "Latitude") +
  annotation_scale(location = "bl", width_hint = 0.2, line_width = 1,
                   pad_x = unit(.35, "in")) + 
  annotation_north_arrow(location = "bl", which_north = "false", 
                         style = north_arrow_fancy_orienteering(),
                         height = unit(0.3,"in"), width = unit(0.3,"in"),
                         pad_x = unit(.02, "in"), pad_y = unit(.02, "in"))

combined_map <- ggdraw() +
  draw_plot(map_nlcd) +
  draw_plot(inset, x = .76, y = .65, width = 0.25, height = 0.25) +
  draw_plot(table_grob, x = 0.1, y = 0.70, width = 0.42, height = 0.25)

print(combined_map)

if (saveplot) dev.off()
