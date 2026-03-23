library(nhdplusTools)
library(sf)
library(dplyr)
library(leaflet)

us_states <- map_data("state")
cali <- us_states[us_states$region == "california",]
polygon <- st_polygon(list(as.matrix(us_states[us_states$region == "california",c("long", "lat")])))
cali_sf <- st_sf(id = cali$region[1], geometry = st_sfc(polygon), crs = 4326)

# Download HUCs of all levels
huc4  <- get_huc(AOI = cali_sf, type = "huc04")
huc6  <- get_huc(AOI = cali_sf, type = "huc06")
huc8  <- get_huc(AOI = cali_sf, type = "huc08")
huc10 <- get_huc(AOI = cali_sf, type = "huc10")

leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles("CartoDB.Positron") %>%
  
  # HUC4
  addPolygons(
    data = huc4,
    color = "#1f78b4", weight = 2, fillOpacity = 0.1,
    label = ~paste0("HUC4: ", name, " (", huc4, ")"),
    group = "HUC4"
  ) %>%
  
  # HUC6
  addPolygons(
    data = huc6,
    color = "#33a02c", weight = 1.5, fillOpacity = 0.1,
    label = ~paste0("HUC6: ", name, " (", huc6, ")"),
    group = "HUC6"
  ) %>%
  
  # HUC8
  addPolygons(
    data = huc8,
    color = "#e31a1c", weight = 1.2, fillOpacity = 0.1,
    label = ~paste0("HUC8: ", name, " (", huc8, ")"),
    group = "HUC8"
  ) %>%
  
  # HUC10
  addPolygons(
    data = huc10,
    color = "#ff7f00", weight = 1, fillOpacity = 0.1,
    label = ~paste0("HUC10: ", name, " (", huc10, ")"),
    group = "HUC10"
  ) %>%
  
  # Layer control
  addLayersControl(
    overlayGroups = c("HUC4", "HUC6", "HUC8", "HUC10"),
    options = layersControlOptions(collapsed = FALSE)
  )
