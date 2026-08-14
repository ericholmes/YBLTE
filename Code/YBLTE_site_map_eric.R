library(httr)
library(sf)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
# library(ggsflabel)
library(deltamapr)
library(basemaps)
library(nhdplusTools)
library(ggrepel)
library(ggspatial)
library(readxl)
library(CropScapeR)
library(raster)
library(terra)

API = F       # Should spatial data be download from APIs? If F will use cached data.
savecache = F  # Overwrite downloaded spatial data
saveoutput = F # Overwrite map figure
# Data procurement ---------------------------------------------------------

# Read Excel file
proj_raw <- read_excel("Data/tabular/YBLTE_sites.xlsx")

# Convert to sf points (WGS84)
proj_pts <- proj_raw %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326, remove = FALSE)

if(API){
  ## Download Flood bypass spatial data ----
  flood_bypasses_2014_url <- "https://gis.data.cnra.ca.gov/api/download/v1/items/5d56a0c6d8414b29a4769c0c4fbe8536/geojson?layers=0"
  
  ## Download restoration project polygons ----
  restoration_projects_url <- "https://gis.water.ca.gov/arcgis/rest/services/Environment/i07_Habitat_Restoration_Polygons/FeatureServer/1/query?where=1=1&outFields=*&returnGeometry=true&f=pjson"
  
  ##Yolo Wildlife Area polygon ----
  yolo_wildlife_area_url <- "https://data-cdfw.opendata.arcgis.com/api/download/v1/items/b3b6dd29b34247dbb2dd773ea17cc82d/geojson?layers=0"
  
  ## Load the polygon data
  polygons <- st_read(restoration_projects_url)
  bypasses <- st_read(flood_bypasses_2014_url)
  ywa_poly <- st_read(yolo_wildlife_area_url)
  ##Download Feather river polyline ----
  # Define the NHD API endpoint
  url <- "https://hydro.nationalmap.gov/arcgis/rest/services/nhd/MapServer/6/query"
  
  # Sacramento Valley bounding box (xmin, ymin, xmax, ymax)
  sac_bbox <- st_bbox(c(
    xmin = -121.9,
    ymin = 38.15,
    xmax = -121.4,
    ymax = 38.85
  ), crs = 4326)
  
  # Convert bbox to sf polygon
  sac_poly <- sf::st_as_sfc(sac_bbox)
  
  # Load wetland data (CARI), crop to bounding box (needed to change CRS)
  cari <- st_crop(st_transform(H_CARI_wetlands, crs = 4326), sac_bbox)
  # Drop channels (redundant with NHD)
  cari <- cari[!grepl("Channel", cari$leglabellevel1),] 
  

  nwi_url <- "https://fwspublicservices.wim.usgs.gov/wetlandsmapservice/rest/services/Wetlands/MapServer/0/query"
  
  nwi <- st_read(paste0(
    nwi_url,
    "?where=1=1",
    "&outFields=*",
    "&f=geojson",
    "&geometry=-121.9,38.1,-121.4,38.9",  # larger than your map extent
    "&geometryType=esriGeometryEnvelope",
    "&inSR=4326&outSR=4326"
  ))
  
  map_extent <- st_as_sfc(st_bbox(c(
    xmin = -121.9, xmax = -121.4,
    ymin = 38.15, ymax = 38.85
  ), crs = 4326))
  
  nwi_yolo <- st_intersection(nwi, map_extent)
  
  
  # Download agriculture data (CropLand/CDL)
    # DWR wifi will not work (you can use guest wifi, VPN)
    # Was having server issues (too overwhelmed?), worked the next day
  cdl_raw <- GetCDLData(aoi = c(-121.9, 38.15, -121.4, 38.85), year = "2025", type = "b",
                    crs = "+init=epsg:4326")
  
  # Filter for rice only
  cdl <- data.table::copy(cdl_raw)
  cdl[cdl[]!=3] = NA
  
  # Convert to sf (runs a long time)
  cdl_sf <- as.polygons(rast(cdl), na.rm = T)
  cdl_sf <- st_as_sf(cdl_sf, as_points = F)
  # cdl_sf <- cdl_sf %>% rename(crop = CDL_2024_clip_20260416112512_974840496)
  
  # Download NHDPlus flowlines within the bbox
  flowlines <- get_nhdplus(
    sac_poly,
    realization = "flowline",
    streamorder = 1,
    t_srs = 4326
  )

  major_names <- c("Sacramento River",
                   "Feather River",
                   "American River",
                   "Putah Creek",
                   "Yuba River",
                   "Cache Creek")
  
  rivers_major <- flowlines %>%
    filter(gnis_name %in% major_names)
  
  ##Download Tigris major roads ----
  roads <- tigris::primary_roads(year = 2024)
  # Get county boundaries for filtering
  counties_clip_boundary <- tigris::counties(state = "CA", year = 2024) %>%
    filter(NAME %in% c("Sacramento", "Yolo", "Solano", "Placer", "Sutter", "San Joaquin"))
  
  # Filter roads to only those within Sacramento and Yolo Counties
  roads_filtered <- st_intersection(roads, counties_clip_boundary)
  
  if(savecache == T){save(polygons, bypasses, rivers_major, roads_filtered, ywa_poly, cari, cdl_sf, ndwi_yolo,
       file = "data/spatial/Yolo_map_data.Rdata")}

}else{
  ## Short-cut to pre saved data
  load(file = "data/spatial/Yolo_map_data.Rdata")
}

## Subset bypasses to only Yolo
yolo_bypass <- bypasses[bypasses$Feature_Name %in% 
                          c("Sacramento Bypass", "Yolo Bypass", "Yolo Bypass and Cache Slough"), ]

ywa <- ywa_poly[ywa_poly$PROP_NAME ==  "Yolo Bypass Wildlife Area",
                c("PROP_NAME", "PROP_TYPE")]

ywa$Project_Status <- "Completed"
colnames(ywa) <- c("project_name", "project_type", "geometry", "Project_Status")
polygons <- st_transform(polygons, crs = st_crs(ywa))
rest_polys <- rbind(polygons[,c("project_name", "project_type","Project_Status")], ywa)

## Calculate acreage of each polygon
sf::sf_use_s2(FALSE)

rest_polys$acres <- round(st_area(rest_polys)*0.00024711,1)
ywa <- ywa_poly[ywa_poly$PROP_NAME ==  "Yolo Bypass Wildlife Area",]
ywa$acres <- round(st_area(ywa)*0.00024711,1)

## Interactive leaflet map --------------------------------------------------

leaflet(rest_polys) %>%
  addTiles() %>%
  addPolygons(color = "blue", weight = 1, fillOpacity = 0.5,
              group = "rest_polys",
              highlightOptions = highlightOptions(
    color = "red", weight = 3, fillOpacity = 0.7),
    popup = ~paste0("<b>Name:</b> ", project_name, "<br>",
      "<b>Type:</b> ", project_type, "<br>",
      "<b>Status:</b> ", Project_Status, "<br>",
      "<b>Acreage:</b> ", acres
    )) %>%
  addCircleMarkers(
    data = proj_pts,
    lng = ~Lon,
    lat = ~Lat,
    radius = 6,
    color = "red",
    fillColor = "yellow",
    fillOpacity = 0.8,
    popup = ~paste0("<b>Project:</b> ", Site_id)
  ) %>% 

  addMeasurePathToolbar(options = measurePathOptions(imperial = FALSE,
                                                     minPixelDistance = 100,
                                                     showDistances = FALSE,
                                                     showOnHover = TRUE)) %>% 
  addProviderTiles("Esri.WorldStreetMap", group = "Streets") %>%
  addProviderTiles("Esri.WorldImagery", group = "Imagery") %>%
  addLayersControl(baseGroups = c("Streets", "Imagery"),
                   overlayGroups = c("rest_polys"),
                   options = layersControlOptions(collapsed = FALSE))

## Static map ---------------------------------------------------------------
WW_Watershed_wgs84 <- st_transform(WW_Watershed, st_crs(yolo_bypass))

if(saveoutput == T){png("Output/Maps/YBLTE_Sites%02da.png",
                                 height = 6, width = 6, units = "in", res = 1000, family = "serif")}

ggplot() + 
  geom_sf(data = yolo_bypass, aes(fill = 'a'), color = NA) +
  scale_fill_manual(values = c('a' = alpha('#33599C', 0.5)), 
                    labels = c("Yolo Bypass"), name = NULL) +
  ggnewscale::new_scale_fill() + 
  
  # geom_sf(data = cari, aes(fill = 'w', color = 'w'), alpha = 0.9) +
  geom_sf(data = nwi, aes(fill = "nwi"), color = NA, alpha = 0.6) +
  scale_fill_manual(
    values = c("nwi" = "forestgreen"),
    labels = c("NWI Wetlands"),
    name = NULL
  ) +
  # scale_fill_manual(values = c('w' = "#66C2A4"), labels = c("Wetland"), name = NULL) + 
  # scale_color_manual(values = c('w' = "#66C2A4"), labels = c("Wetland"), name = NULL) +
  ggnewscale::new_scale_color() + ggnewscale::new_scale_fill() + 
  
  geom_sf(data = cdl_sf, aes(fill = "b"), color = NA, alpha = 0.9) +
  scale_fill_manual(values = c('b' = 'wheat2'), labels = c("Rice Field"), name = NULL) +
  
  geom_sf(data = WW_Watershed_wgs84, fill = "#33599C", color = "#33599C") +
  geom_sf(data = rivers_major, color = "#33599C") +
 
  geom_sf(data = roads_filtered, color = "grey60") +
  ggnewscale::new_scale_fill() + theme_bw() +
  
  geom_sf(data = proj_pts %>% filter(Sampletype=="main"), 
          aes(shape = Sitetype, fill = Sitetype), size = 4, linewidth = 2) +
  scale_shape_manual(values = 21:23) +
  scale_fill_manual(values = c(alpha('steelblue', 0.6), alpha('gold', 0.6), alpha('purple', 0.6))) +
  
  geom_text_repel(aes(x = -121.848, y = 38.716, label = "Cache Creek"), 
                  data = NULL, color = "#1A3057", size = 3, fontface = "bold",
                  bg.color = "white", bg.r = 0.1, angle = 33.6) +
  geom_text_repel(aes(x = -121.87, y = 38.541, label = "Putah Creek"), 
             data = NULL, color = "#1A3057", size = 3, fontface = "bold",
             bg.color = "white", bg.r = 0.1, angle = -10) +
  geom_text_repel(aes(x = -121.731, y = 38.824, label = "Sacramento\nRiver"), 
                   data = NULL, color = "#1A3057", size = 3, fontface = "bold",
                  bg.color = "white", bg.r = 0.1, force = 0, hjust = "right") +
  geom_text_repel(aes(x = -121.63, y = 38.825, label = "Feather\nRiver"), 
                   data = NULL, color = "#1A3057", size = 3, fontface = "bold",
                   bg.color = "white", bg.r = 0.1, force = 0, hjust = "left") +
  geom_text_repel(data = proj_pts %>% filter(Sampletype=="main"), 
                  aes(geometry = geometry, label = Site_id), 
                   stat = "sf_coordinates", size = 3, bg.color = alpha("white", 0.6),
                   color = "black", bg.r = 0.1, fontface = "bold") +
  coord_sf(xlim = c(-121.9, -121.4), ylim = c(38.15, 38.85), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.2, line_width = 1) + 
  annotation_north_arrow(location = "bl", which_north = "false", 
                         style = north_arrow_fancy_orienteering(),
                         height = unit(0.3,"in"), width = unit(0.3,"in"),
                         pad_x = unit(0.06, "in"), pad_y = unit(0.25, "in")) + 
  labs(x = NULL, y = NULL, shape = "Site Type", fill = "Site Type", label = "")


  

if(saveoutput == T){dev.off()}


# alternate map code ------------------------------------------------------

png("Output/Maps/YBLTE_Sites%02db.png",
    height = 7, width = 6, units = "in", res = 1000, family = "serif")

ggplot() +
  
  #-------------------------
# POLYGON LAYERS (Unified Legend)
#-------------------------

geom_sf(data = yolo_bypass, aes(fill = "Yolo Bypass"), color = NA) +
  geom_sf(data = nwi,         aes(fill = "NWI Wetlands"), color = NA, alpha = 0.6) +
  geom_sf(data = cdl_sf,      aes(fill = "Rice Field"),   color = NA, alpha = 0.9) +
  
  # Unified polygon legend
  scale_fill_manual(
    name   = "Landcover",
    values = c(
      "Yolo Bypass" = alpha('#33599C', 0.5),
      "NWI Wetlands" = "forestgreen",
      "Rice Field"   = "wheat2"
    ),
    breaks = c("Yolo Bypass", "NWI Wetlands", "Rice Field"),
    labels = c("Yolo Bypass", "NWI Wetlands", "Rice Field"),
    guide = guide_legend(
      override.aes = list(
        shape = 22,      # square patch
        size  = 5,
        color = "grey50"
      )
    )
  ) +
  
  ggnewscale::new_scale_fill() +
  ggnewscale::new_scale_color() +
  
  #-------------------------
# HYDRO + ROADS (no legend)
#-------------------------
geom_sf(data = WW_Watershed_wgs84, fill = "#33599C", color = "#33599C") +
  geom_sf(data = rivers_major,       color = "#33599C", size = 0.8) +
  geom_sf(data = roads_filtered,     color = "grey60", size = 0.6) +
  
  #-------------------------
# POINTS (proper legend)
#-------------------------
geom_sf(
  data = proj_pts %>% filter(Sampletype == "main"),
  aes(shape = Sitetype, fill = Sitetype),
  size = 4, linewidth = 0.7
) +
  
  scale_shape_manual(
    name   = "Site Type",
    values = c(21, 22, 23),
    guide  = guide_legend(
      override.aes = list(
        size = 5   # match map point appearance
      )
    )
  ) +
  
  scale_color_manual(values = c()) +   # ensure no unwanted color scale
  scale_fill_manual(
    name   = "Site Type",
    values = c(alpha('steelblue', 0.7),
               alpha('gold',      0.7),
               alpha('purple',    0.7)),
    guide = guide_legend(
      override.aes = list(
        shape = c(21, 22, 23),
        size  = 5,
        color = "black"
      )
    )
  ) +
  
  #-------------------------
# LABEL LAYERS
#-------------------------
geom_text_repel(
  aes(x = c(-121.848, -121.87, -121.731, -121.63), 
      y = c(38.716, 38.541, 38.824, 38.825), 
      label = c("Cache Creek", "Putah Creek", "Sacramento\nRiver", "Feather\nRiver"), 
      angle = c(33.6, -10, 0, 0), 
      hjust = c(0.5, 0.5, 1, 0)),
  color = "#1A3057",
  size = 3,
  fontface = "bold",
  bg.color = "white",
  bg.r = 0.1,
  segment.color = "grey50"
) +

# geom_text_repel(aes(x = -121.848, y = 38.716, label = "Cache Creek"),
#                 data = NULL, color = "#1A3057", size = 3, fontface = "bold",
#                 bg.color = "white", bg.r = 0.1, angle = 33.6) +
#   geom_text_repel(aes(x = -121.87, y = 38.541, label = "Putah Creek"),
#                   data = NULL, color = "#1A3057", size = 3, fontface = "bold",
#                   bg.color = "white", bg.r = 0.1, angle = -10) +
#   geom_text_repel(aes(x = -121.731, y = 38.824, label = "Sacramento\nRiver"),
#                   data = NULL, color = "#1A3057", size = 3, fontface = "bold",
#                   bg.color = "white", bg.r = 0.1, hjust = "right") +
#   geom_text_repel(aes(x = -121.63, y = 38.825, label = "Feather\nRiver"),
#                   data = NULL, color = "#1A3057", size = 3, fontface = "bold",
#                   bg.color = "white", bg.r = 0.1, hjust = "left") +
  geom_text_repel(
    data = proj_pts %>% filter(Sampletype == "main"),
    aes(geometry = geometry, label = Site_id),
    stat = "sf_coordinates",
    size = 3,
    bg.color = alpha("white", 0.6),
    color = "black",
    bg.r = 0.1,
    fontface = "bold"
  ) +
  
  #-------------------------
# COORDINATES + NORTH ARROW + SCALE BAR
#-------------------------
coord_sf(
  xlim = c(-121.9, -121.4),
  ylim = c(38.15, 38.85),
  expand = FALSE
) +
  
  annotation_scale(location = "bl", width_hint = 0.2, line_width = 1) +
  annotation_north_arrow(location = "bl", which_north = "false",
                         style = north_arrow_fancy_orienteering(),
                         height = unit(0.3,"in"), width = unit(0.3,"in"),
                         pad_x = unit(0.06, "in"), pad_y = unit(0.25, "in")) +
  
  #-------------------------
# THEMING + LEGEND POSITION
#-------------------------
labs(
  x = NULL, y = NULL,
) +
  
  theme_bw() +
  theme(
    # Move legend inside top-right
    legend.position   = c(0.98, 0.98),
    legend.justification = c(1, 1),
    
    # Styling
    legend.background = element_rect(fill = alpha("white", 0.9), color = "black", size = 0.5),
    legend.key        = element_rect(fill = alpha("white", 0.9), color = "grey60"),
    legend.title      = element_text(size = 9, face = "bold"),
    legend.text       = element_text(size = 8),
    
    # Shrink legend spacing
    legend.box.spacing = unit(1, "mm"),
    legend.key.size    = unit(3.5, "mm")
  )

dev.off()
