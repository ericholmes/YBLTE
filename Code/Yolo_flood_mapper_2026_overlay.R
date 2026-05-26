library(raster)
library(terrainr)
library(ggplot2)
library(dplyr)
library(patchwork)
library(grid)
library(sf)

# ---- Helper function to convert RGB raster to df ----
rgb_to_df <- function(r) {
  df <- as.data.frame(r, xy = TRUE)
  names(df) <- c("x", "y", "R", "G", "B")
  
  df %>%
    filter(!(R == 0 & G == 0 & B == 0)) %>%
    filter(!is.na(R) & !is.na(G) & !is.na(B)) %>%
    mutate(
      R = R / 255,
      G = G / 255,
      B = B / 255
    )
}

dark_theme_bw <- function(base_size = 12, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Backgrounds
      plot.background   = element_rect(fill = "black", colour = NA),
      panel.background  = element_rect(fill = "black", colour = NA),
      legend.background = element_rect(fill = "black", colour = NA),
      strip.background  = element_rect(fill = "#222222", colour = "white"),
      
      # Gridlines
      panel.grid.major  = element_line(colour = "black", linewidth = 0.3),
      panel.grid.minor  = element_line(colour = "black", linewidth = 0.2),
      
      # Axes
      axis.text         = element_text(colour = "white"),
      axis.title        = element_text(colour = "white"),
      axis.ticks        = element_line(colour = "white"),
      
      # Titles
      plot.title        = element_text(colour = "white", face = "bold"),
      plot.subtitle     = element_text(colour = "white"),
      plot.caption      = element_text(colour = "grey80"),
      
      # Legend
      legend.text       = element_text(colour = "white"),
      legend.title      = element_text(colour = "white"),
      
      # Facet strips
      strip.text        = element_text(colour = "white"),
      plot.margin  = margin(0, 0, 0, 0),
      panel.spacing = unit(0, "pt")
    )
}

clean_theme <- theme_void() +
  theme(legend.text = element_text(color = "grey90"),
        legend.title = element_text(color = "grey90"),
        plot.margin = margin(0,0,0,0),
        panel.spacing = unit(0, "pt"),
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill = NA, colour = NA))

mode_fun <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

overlay_to_df <- function(r) {
  rsmall <- raster::aggregate(r, fact = 4, fun = mode_fun)
  df <- as.data.frame(rsmall, xy = TRUE)
  names(df) <- c("x", "y", "val")
  
  # Example: convert numeric values to hex colors
  df$fill <- ifelse(df$val == 0, NA, "cyan2")  # color overlay
  df$fill2 <- ifelse(df$val == 0, NA, 
                     ifelse(df$val == 1, "purple",
                            ifelse(df$val == 2, "darkorange4",
                                   ifelse(df$val == 3, "red3",
                                          ifelse(df$val == 4, "darkorange",
                                                 ifelse(df$val == 5, "blue",
                                                        "black"))))))
  df$alpha <- ifelse(df$val == 0, 0, 0.9)        # transparency mask
  
  df
}

# ---- Load 4 dates ----
r1 <- stack("data/Spatial/GEE/Yolo/S2_RGB_2026-01-08.tif")
r2 <- stack("data/Spatial/GEE/Yolo/S2_RGB_2026-02-12.tif")
r3 <- stack("data/Spatial/GEE/Yolo/S2_RGB_2026-03-04.tif")
r4 <- stack("data/Spatial/GEE/Yolo/S2_RGB_2026-03-09.tif")

o1 <- raster("data/Spatial/GEE/Yolo/WaterClass_2026-01-08.tif")
o2 <- raster("data/Spatial/GEE/Yolo/WaterClass_2026-02-12.tif")
o3 <- raster("data/Spatial/GEE/Yolo/WaterClass_2026-03-04.tif")
o4 <- raster("data/Spatial/GEE/Yolo/WaterClass_2026-03-09.tif")

# # Convert to data frames
df1 <- rgb_to_df(r1)
df2 <- rgb_to_df(r2)
df3 <- rgb_to_df(r3)
df4 <- rgb_to_df(r4)

o1df <- overlay_to_df(o1)
o2df <- overlay_to_df(o2)
o3df <- overlay_to_df(o3)
o4df <- overlay_to_df(o4)

#Create barchart of pixels and area of each class
o1df$time <- "1"; o2df$time <- "2"; o3df$time <- "3"; o4df$time <- "4"
odf <- rbind(o1df, o2df, o3df, o4df)
odfply <- odf %>% group_by(val, time) %>% 
  summarize(sumpix = length(val)) %>% group_by(val, time) %>% 
  mutate(area_m2 = sumpix*100, acres = sumpix*0.02471054)

odfply$valchar <- paste0("c", odfply$val)

png("Output/maps/Flood_mapper_yolo_areabar.png", 
    height = 5, width = 3, units = "in", res = 600, family = "serif", bg = "transparent")

ggplot(odfply[odfply$val != 0, ], aes(x = valchar, y = acres, fill = valchar)) + labs(x = NULL, y = "Area (acres)") +
  geom_bar(stat = "identity", show.legend = F) + facet_grid(time ~ .) + dark_theme_bw() +
  scale_fill_manual(values = c(c1 = "purple", c2 = "darkorange4", c3 = "red3", c4 = "darkorange", c5 = "blue"))

dev.off()

all_x <- range(c(df1$x, df2$x, df3$x, df4$x))
all_y <- range(c(df1$y, df2$y, df3$y, df4$y))

global_bbox <- st_as_sfc(st_bbox(c(
  xmin = all_x[1],
  xmax = all_x[2],
  ymin = all_y[1],
  ymax = all_y[2]
), crs = 32610))

# ---- Create 4 RGB panels ----
p1 <- ggplot() +
  geom_spatial_rgb(data = df1, aes(x, y, r = R, g = G, b = B)) +
  coord_sf(crs = 32610, xlim = c(all_x[1], all_x[2]), 
           ylim = c(all_y[1], all_y[2]),expand = FALSE) +
  dark_theme_bw()+ 
  theme(axis.text = element_blank(), axis.ticks = element_blank()) + 
  labs(x = NULL, y = NULL)

p2 <- ggplot() +
  geom_spatial_rgb(data = df2, aes(x, y, r = R, g = G, b = B)) +
  coord_sf(crs = 32610, xlim = c(all_x[1], all_x[2]), 
           ylim = c(all_y[1], all_y[2]),expand = FALSE) +
  dark_theme_bw() + 
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  labs(x = NULL, y = NULL)

p3 <- ggplot() +
  geom_spatial_rgb(data = df3, aes(x, y, r = R, g = G, b = B)) +
  coord_sf(crs = 32610, xlim = c(all_x[1], all_x[2]), 
           ylim = c(all_y[1], all_y[2]),expand = FALSE) +
  dark_theme_bw() + 
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  labs(x = NULL, y = NULL)

p4 <- ggplot() +
  geom_spatial_rgb(data = df4, aes(x, y, r = R, g = G, b = B)) +
  coord_sf(crs = 32610, xlim = c(all_x[1], all_x[2]), 
           ylim = c(all_y[1], all_y[2]),expand = FALSE) +
  dark_theme_bw() + 
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  labs(x = NULL, y = NULL)

p1 <- p1 + clean_theme
p2 <- p2 + clean_theme
p3 <- p3 + clean_theme
p4 <- p4 + clean_theme

g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
g4 <- ggplotGrob(p4)

# Add NDSI -------------------------------------------------------------

p1w <- p1 +
  geom_raster(data = o1df, aes(x, y, fill = fill, alpha = alpha)) +
  scale_fill_identity() +
  scale_alpha_identity()

p2w <- p2 +
  geom_raster(data = o2df, aes(x, y, fill = fill, alpha = alpha)) +
  scale_fill_identity() +
  scale_alpha_identity()

p3w <- p3 +
  geom_raster(data = o3df, aes(x, y, fill = fill, alpha = alpha)) +
  scale_fill_identity() +
  scale_alpha_identity()

p4w <- p4 +
  geom_raster(data = o4df, aes(x, y, fill = fill, alpha = alpha)) +
  scale_fill_identity() +
  scale_alpha_identity()

g1w <- ggplotGrob(p1w)
g2w <- ggplotGrob(p2w)
g3w <- ggplotGrob(p3w)
g4w <- ggplotGrob(p4w)

# Add classes -------------------------------------------------------------

p1c <- p1 +
  geom_raster(data = o1df, aes(x, y, fill = fill2, alpha = alpha)) +
  scale_fill_identity() +
  scale_alpha_identity()

p2c <- p2 +
  geom_raster(data = o2df, aes(x, y, fill = fill2, alpha = alpha)) +
  scale_fill_identity() +
  scale_alpha_identity()

p3c <- p3 +
  geom_raster(data = o3df, aes(x, y, fill = fill2, alpha = alpha)) +
  scale_fill_identity() +
  scale_alpha_identity()

p4c <- p4 +
  geom_raster(data = o4df, aes(x, y, fill = fill2, alpha = alpha)) +
  scale_fill_identity() +
  scale_alpha_identity()

g1c <- ggplotGrob(p1c)
g2c <- ggplotGrob(p2c)
g3c <- ggplotGrob(p3c)
g4c <- ggplotGrob(p4c)

dev.off()

##Standardize panels

equalize_grobs <- function(grobs) {
  max_widths  <- do.call(grid::unit.pmax, lapply(grobs, `[[`, "widths"))
  max_heights <- do.call(grid::unit.pmax, lapply(grobs, `[[`, "heights"))
  
  lapply(grobs, function(g) {
    g$widths  <- max_widths
    g$heights <- max_heights
    g
  })
}

grobs <- equalize_grobs(list(g1, g2, g3, g4))
grobs_w <- equalize_grobs(list(g1w, g2w, g3w, g4w))
grobs_c <- equalize_grobs(list(g1c, g2c, g3c, g4c))

## Imagery
png("Output/maps/Flood_mapper_yolo_overlap26.png", 
    height = 6, width = 12, units = "in", res = 600, family = "serif", bg = "transparent")

grid.newpage()

grobs <- list(g1, g2, g3, g4)

# Centers of the four panels
x_centers <- c(0.24, 0.44, 0.64, 0.84)
panel_width <- 0.5


for (i in 1:4) {
  pushViewport(viewport(
    x = x_centers[i],
    y = 0.5,
    width = panel_width,
    height = 1,
    just = c("center", "center")
  ))
  
  grid.draw(grobs[[i]])
  popViewport()
}


dev.off()

## Inundation
png("Output/maps/Flood_mapper_yolo_overlap26_water.png", 
    height = 6, width = 12, units = "in", res = 600, family = "serif", bg = "transparent")

grid.newpage()

grobs <- list(g1w, g2w, g3w, g4w)

# Centers of the four panels
x_centers <- c(0.24, 0.44, 0.64, 0.84)
panel_width <- 0.5


for (i in 1:4) {
  pushViewport(viewport(
    x = x_centers[i],
    y = 0.5,
    width = panel_width,
    height = 1,
    just = c("center", "center")
  ))
  
  grid.draw(grobs[[i]])
  popViewport()
}


dev.off()

## classes
png("Output/maps/Flood_mapper_yolo_overlap26_class.png", 
    height = 6, width = 12, units = "in", res = 600, family = "serif", bg = "transparent")

grid.newpage()

grobs <- list(g1c, g2c, g3c, g4c)

# Centers of the four panels
x_centers <- c(0.24, 0.44, 0.64, 0.84)
panel_width <- 0.5


for (i in 1:4) {
  pushViewport(viewport(
    x = x_centers[i],
    y = 0.5,
    width = panel_width,
    height = 1,
    just = c("center", "center")
  ))
  
  grid.draw(grobs[[i]])
  popViewport()
}

dev.off()