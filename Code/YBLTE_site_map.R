library(httr)
library(sf)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(ggsflabel)
library(deltamapr)
library(basemaps)
library(ggrepel)
library(ggspatial)

API = T
saveoutput = F
# Data procurement ---------------------------------------------------------
## Need to be plugged into DWR network or VPN
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
  
  # Set query parameters to filter for Feather River
  query_params <- list(
    where = "GNIS_NAME='Feather River'",
    outFields = "*",
    f = "geojson"
  )
  
  # Request feather river data from the API
  response <- GET(url, query = query_params)
  
  # Read the feather river response variable into an sf object
  feather_river <- st_read(content(response, "text"))
  
  ##Download Tigris major roads ----
  roads <- tigris::primary_roads(year = 2024)
  # Get county boundaries for filtering
  counties_clip_boundary <- tigris::counties(state = "CA", year = 2024) %>%
    filter(NAME %in% c("Sacramento", "Yolo", "Solano", "Placer", "Sutter", "San Joaquin"))
  
  # Filter roads to only those within Sacramento and Yolo Counties
  roads_filtered <- st_intersection(roads, counties_clip_boundary)
  
  if(saveoutput == T){save(polygons, bypasses, feather_river, roads_filtered, ywa_poly,
       file = "data/spatial/Yolo_polygon_data.Rdata")}

}else{
  ## Short-cut to pre saved data
  load(file = "data/spatial/Yolo_polygon_data.Rdata")
}

## Subset bypasses to only Yolo
yolo_bypass <- bypasses[bypasses$Feature_Name %in% 
                          c("Sacramento Bypass", "Yolo Bypass", "Yolo Bypass and Cache Slough"), ]

ywa <- ywa_poly[ywa_poly$PROP_NAME ==  "Yolo Bypass Wildlife Area",
                c("PROP_NAME", "PROP_TYPE")]

ywa$Project_Status <- "Completed"
colnames(ywa) <- c("project_name", "project_type", "geometry", "Project_Status")

rest_polys <- rbind(polygons[,c("project_name", "project_type","Project_Status")], ywa)

## Calculate acreage of each polygon
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
  geom_sf(data = feather_river, color = "slateblue3") +
  geom_sf(data = yolo_rest_polys[yolo_rest_polys$HRL == F, ], color = "white", fill = "forestgreen") + 
  geom_sf(data = yolo_rest_polys[yolo_rest_polys$HRL == T, ], color = "brown", fill = "orange") + theme_bw() +
  geom_sf(data = roads_filtered, color = "grey60") +
  geom_label_repel(data = yolo_rest_polys[yolo_rest_polys$HRL == F & yolo_rest_polys$dir < 0, ], 
                   aes(geometry = geometry, label = short_name),
                   stat = "sf_coordinates",fill = "palegreen3", size = 2.5, force = 5, 
                   segment.square = T, segment.inflect = T, nudge_x = -.13) +
  geom_label_repel(data = yolo_rest_polys[yolo_rest_polys$HRL == F & yolo_rest_polys$dir > 0, ], 
                   aes(geometry = geometry, label = short_name),
                   stat = "sf_coordinates",fill = "palegreen3", size = 2.5, force = 5, 
                   segment.square = T, segment.inflect = T, nudge_x = .13) +
  geom_label_repel(data = yolo_rest_polys[yolo_rest_polys$HRL == T, ], 
                   aes(geometry = geometry, label = short_name), 
                   stat = "sf_coordinates",fill = "tan2", size = 2.5, nudge_x = -.1) +
  geom_label_repel(aes(x = -121.89, y = 38.511, label = "Putah Creek"), 
             data = NULL, fill = "tan2", size = 2.5) +
  geom_label_repel(aes(x = -121.79, y = 38.825, label = "Sacramento\nRiver"), 
                   data = NULL, fill = "tan2", size = 2.5, force = 0) +
  geom_label_repel(aes(x = -121.595, y = 38.825, label = "Feather\nRiver"), 
                   data = NULL, fill = "tan2", size = 2.5, force = 0) +
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

