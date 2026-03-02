library(httr)
library(sf)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(ggsflabel)
library(deltamapr)
library(basemaps)
library(nhdplusTools)
library(ggrepel)
library(ggspatial)
library(readxl)

API = F        # Should spatial data be download from APIs? If F will use cached data.
savecache = F  # Overwrite downloaded spatial data
saveoutput = F # Overwrite map figure
# Data procurement ---------------------------------------------------------

# Read Excel file
proj_raw <- read_excel("C:/Users/eholmes/Box/Yolo_Food_Web/Data/tabular/YBLTE_sites.xlsx")

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
    xmin = -122.2,
    ymin = 38.0,
    xmax = -121.0,
    ymax = 39.3
  ), crs = 4326)
  
  # Convert bbox to sf polygon
  sac_poly <- st_as_sfc(sac_bbox)
  
  
  # Download NHDPlus flowlines within the bbox
  flowlines <- get_nhdplus(
    sac_poly,
    realization = "flowline",
    streamorder = 3,        # only larger streams (adjust as needed)
    t_srs = 4326
  )

  major_names <- c("Sacramento River",
                   "Feather River",
                   "American River",
                   "Putah Creek",
                   "Yuba River")
  
  rivers_major <- flowlines %>%
    filter(gnis_name %in% major_names)
  
  ##Download Tigris major roads ----
  roads <- tigris::primary_roads(year = 2024)
  # Get county boundaries for filtering
  counties_clip_boundary <- tigris::counties(state = "CA", year = 2024) %>%
    filter(NAME %in% c("Sacramento", "Yolo", "Solano", "Placer", "Sutter", "San Joaquin"))
  
  # Filter roads to only those within Sacramento and Yolo Counties
  roads_filtered <- st_intersection(roads, counties_clip_boundary)
  
  if(savecache == T){save(polygons, bypasses, rivers_major, roads_filtered, ywa_poly,
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
rest_polys$project_name
## Select Yolo Projects and create short name for labeling
yolo_projects <- data.frame(project_name = c("Yolo Flyway Farms Tidal Habitat Restoration", 
                                             "Lower Yolo Ranch Tidal Habitat Restoration", 
                                             "Lower Elkhorn Basin Levee Setback Project",  
                                             "Little Egbert Multibenefit Project", 
                                             "Lookout Slough Tidal Habitat Restoration and Flood Improvement",  
                                             "Tide's End Multi-benefit Project", 
                                             "Sacramento Weir and Bypass Expansion", 
                                             "Agricultural Road Crossing #4", 
                                             "Big Notch Project - Yolo Bypass Salmonid Habitat Restoration and Fish Passage", 
                                             "Big Notch Project - Supplemental Fish Passage Structure", 
                                             "Big Notch Project - Agricultural Road Crossing #1 and Tule Channel Improvements", 
                                             "Upper Elkhorn Basin Planning Area",
                                             "Tule Canal Corridor Enhancement Planning",
                                             "Fremont Weir Adult Fish Passage", "Yolo Bypass Wildlife Area"),
                            short_name = c("Yolo Flyway Farms", 
                                           "Lower Yolo Ranch", 
                                           "LEBLS",  
                                           "Little Egbert", 
                                           "Lookout Slough",  
                                           "Tide's End", 
                                           "Sac. Weir Expansion", 
                                           "Ag. Crossing #4", 
                                           "Big Notch", 
                                           "Supp. Fish Passage", 
                                           "Ag. Crossing #1 + Tule Channel", 
                                           "UEBLS",
                                           "Tule Canal",
                                           "AFP", "YBWA"),
                            HRL = c(F,F,T,F,F,T,F,F,F,F,F,F,F,F,F),
                            dir = c(-1,-1,1,-1,-1,1,1,1,1,-1,-1,1,1-1,-1,-1))

yolo_rest_polys <- merge(rest_polys, yolo_projects, by = "project_name", all.y = T)
WW_Watershed_wgs84 <- st_transform(WW_Watershed, st_crs(yolo_bypass))

if(saveoutput == T){tiff("Output/Maps/Yolo_restoration_projects_scale_%02da.tif",
                                 height = 6, width = 4, units = "in", res = 1000, family = "serif", compression = "lzw")}

ggplot() + 
  geom_sf(data = yolo_bypass, fill = "wheat2", color = NA) +
  geom_sf(data = WW_Watershed_wgs84, fill = "slateblue3", color = "slateblue3") +
  geom_sf(data = rivers_major, color = "slateblue3") +
  geom_sf(data = yolo_rest_polys[yolo_rest_polys$HRL == F, ], color = "white", fill = "forestgreen") + 
  geom_sf(data = yolo_rest_polys[yolo_rest_polys$HRL == T, ], color = "brown", fill = "orange") + theme_bw() +
  geom_sf(data = roads_filtered, color = "grey60") +
  geom_sf(data = proj_pts, color = "red", size = 3) +
  geom_label_repel(aes(x = -121.89, y = 38.511, label = "Putah Creek"), 
             data = NULL, fill = "tan2", size = 2.5) +
  geom_label_repel(aes(x = -121.79, y = 38.825, label = "Sacramento\nRiver"), 
                   data = NULL, fill = "tan2", size = 2.5, force = 0) +
  geom_label_repel(aes(x = -121.595, y = 38.825, label = "Feather\nRiver"), 
                   data = NULL, fill = "tan2", size = 2.5, force = 0) +
  geom_label_repel( data = proj_pts, aes(geometry = geometry, label = Site_id), 
                    stat = "sf_coordinates", size = 3, fill = "white" ) +
  coord_sf(xlim = c(-121.9, -121.4), ylim = c(38.15, 38.85), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.2, line_width = 1,
                   pad_x = unit(.35, "in")) + 
  annotation_north_arrow(location = "bl", which_north = "false", 
                         style = north_arrow_fancy_orienteering(),
                         height = unit(0.3,"in"), width = unit(0.3,"in"),
                         pad_x = unit(.02, "in"),pad_y = unit(.02, "in")) + 
  labs(main = "Map of Yolo Bypass Restoration Projects",
       x = NULL, y = NULL)

if(saveoutput == T){dev.off()}

