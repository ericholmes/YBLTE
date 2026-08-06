
### YBLTE Zooplankton Workflow 2026

library(tidyverse)
library(readxl)
library(sf)
library(lubridate)
library(vegan)
library(reshape2)
library(scales)
library(plotly)
library(ggrepel)

# set colors

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
                                         "TEW" = "#ADE11E",
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
                                         "TEW" = "#ADE11E",
                                         "TER" = "#FDE725FF"))

# 1. Load data ----

zoop26       <- read_excel("Data/tabular/YBLTE_2026_zoop_QC.xlsx", sheet = 2)
zoop26sites  <- read_excel("Data/tabular/YBLTE_sites.xlsx", sheet = 1)
zooplookup   <- read.csv("Data/tabular/YBLTE_zooplookuptable_042026.csv")

# 2. Metadata attachment ----

zoop26 <- zoop26 %>%
  left_join(
    zoop26sites %>% select(Site_id, Region, Sitetype),
    by = c("Site" = "Site_id")
  ) |> filter(!(Site %in% c("KLWW", "LP", "WDSYB")))

# 3. Clean fields: species, lifestage, splife, date ----

zoop26 <- zoop26 %>%
  mutate(
    Species   = tolower(trimws(Species)),
    LifeStage = tolower(trimws(LifeStage)),
    splife    = trimws(str_c(Species, LifeStage, sep = "_")),
    Date      = as.Date(Date)
  )

# 4. NC handling and aliquot-based sample estimator ----

zoop26 <- zoop26 %>%
  mutate(
    abundance_num     = suppressWarnings(as.numeric(abundance)),
    subsample_fraction = Volumesubsampled_ml / TotalVolume_ml,
    sample.est         = abundance_num / subsample_fraction,
    sample.est         = ifelse(abundance == "NC", NA, sample.est)
  )

# 5. Flowmeter distance (Global Oceanics) ----

zoop26 <- zoop26 %>%
  mutate(
    Rotations = as.integer(FlowMeterEnd) - as.integer(FlowMeterBegin),
    Distance  = ifelse(is.na(Rotations), 20,
                       (Rotations * 26873) / 999999)
  )

ggplot(zoop26, aes(x = Rotations)) + geom_histogram()
ggplot(zoop26, aes(x = Distance)) + geom_histogram()

# 6. CPUE function usinDistance# 6. CPUE function using your mean(sample.est) logic ----

calc_cpue_density <- function(df) {
  
  df %>%
    group_by(Site, Date, splife) %>%
    summarise(Species   = first(Species),
              Region    = first(Region),
              Sitetype  = first(Sitetype),
              Ringsize  = first(RingSize_cm),
              mean.est  = mean(sample.est, na.rm = TRUE),
              Distance  = first(Distance),
              .groups   = "drop") %>%
    mutate(Volume_Sampled = pi * (((Ringsize / 2) * 0.01)^2) * Distance,
           Density        = mean.est / Volume_Sampled)
}

##Pooled density calculation

calc_cpue_density_pooled <- function(df) {
  
  df2 <- df %>%
    mutate(
      abundance_num = case_when(
        abundance == "NC" ~ NA_real_,
        TRUE ~ suppressWarnings(as.numeric(abundance))
      ),
      subsample_fraction = Volumesubsampled_ml / TotalVolume_ml
    )
  
  aliquot_info <- df2 %>%
    group_by(Site, Date) %>%
    summarise(
      n_aliquots = n_distinct(SplitFraction),
      subsample_fraction_single = first(subsample_fraction),
      denom_all = n_aliquots * subsample_fraction_single,   # pooled denominator for non-NC
      denom_nc  = subsample_fraction_single,                 # denominator for NC cases
      .groups = "drop"
    )
  
  df2 <- df2 %>%
    left_join(aliquot_info, by = c("Site", "Date"))
  
  df2 %>%
    group_by(Site, Date, splife) %>%
    summarise(
      Rotations = first(Rotations),
      Ringsize  = first(RingSize_cm),
      Species   = first(Species),
      Region    = first(Region),
      Sitetype  = first(Sitetype),
      numerator = sum(abundance_num, na.rm = TRUE),
      denominator = if (any(is.na(abundance_num)))
        first(denom_nc)
      else
        first(denom_all),
      TotalCount = numerator / denominator,
      .groups = "drop"
    ) %>%
    mutate(Distance = ifelse(is.na(Rotations), 20,
                        (Rotations * 26873) / 999999),
      Volume_Sampled = pi * (((Ringsize / 2) * 0.01)^2) * Distance,
      Density = TotalCount / Volume_Sampled)
}

zooplongmean <- calc_cpue_density(zoop26)
zooplong <- calc_cpue_density_pooled(zoop26)

# dput(zoop26[zoop26$Site == "STTD" & zoop26$Date == as.Date("2025-11-13"),])
view(zooplong[zooplong$Site == "STTD" & zooplong$Date == as.Date("2025-11-13"),])
view(zooplongmean[zooplongmean$Site == "STTD" & zooplong$Date == as.Date("2025-11-13"),])

# 7. Group taxa using lookup ----

zooplong <- zooplong %>%
  left_join(
    zooplookup %>% select(Taxa_identified, Category, Group_family, Group_zoop),
    by = c("Species" = "Taxa_identified")
  ) %>%
  rename(group = Group_zoop)

zooplong$group <- ifelse(zooplong$group %in% c("Harpacticoida", "Amphipoda", "Copepoda"), "Rare", 
                         zooplong$group)

zoopgroupsum <- zooplong %>%
  group_by(group) %>%
  summarise(total_density = sum(Density, na.rm = TRUE))

ggplot(zoopgroupsum, aes(x = reorder(group, -total_density), y = total_density)) + 
  geom_bar(stat = "identity")

# 8. Water-year day calculation ----

zooplong <- zooplong %>%
  mutate(
    Year   = year(Date),
    WY     = ifelse(month(Date) >= 10, Year + 1, Year),
    wyjday = as.numeric(Date - as.Date(paste0(ifelse(month(Date)>=10,Year,Year-1), "-10-01")) + 1)
  )


# WEEKLY AGGREGATION
zoop_weekly <- zooplong %>%
  mutate(week_start = floor_date(Date, "week")) %>%
  group_by(Site, week_start, Region) %>%
  summarise(totezoop = sum(Density, na.rm = TRUE), .groups = "drop")

## time series by site and total zoop ----
ts_by_site <- ggplot(zoop_weekly, aes(x = week_start, y = totezoop, color = Region)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  theme_bw() +
  facet_grid(Site ~ ., scales = "free_y") +
  labs(
    x = "Week",
    y = "Zooplankton Density (m^-3)",
    title = "Weekly Zooplankton Density by Site",
    color = "Region"
  ) +
  theme(
    strip.text.y = element_text(size = 10, face = "bold"),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(size = 14, face = "bold")
  )
ts_by_site

## time series by site with groups ----
zoop_weekly_group <- zooplong %>%
  mutate(week_start = floor_date(Date, "week")) %>%
  group_by(Site, week_start, group) %>%
  summarise(totezoop = sum(Density, na.rm = TRUE), .groups="drop")
dput(unique(zoop_weekly_group[,"group"]))

zoop_weekly_group$group <- factor(
  zoop_weekly_group$group,
  levels = c(
    "Calanoida",
    "Cyclopoida",
    "Small cladocera",
    "Large cladocera",
    "Rotifera",
    "Ostracoda",
    "Insecta",
    "Rare"
  )
)

zoop_weekly_group$Sitefac <- factor(zoop_weekly_group$Site, 
                                    levels = c("FWBN", "FW1", "KNG3", "CNW", "CCSYB", "RD22", "YBLR4", "SB4", 
                                               "AL0", "LIS", "TER", "TEW", "STTD"))

pastel_bold_pal <- c(
  "Calanoida"        = "#4F82C8",  # bold pastel blue
  "Cyclopoida"       = "#7FB3F0",  # lighter blue
  "Small cladocera"  = "#7ECF82",  # light green
  "Large cladocera"  = "#4FAF50",  # deeper green (same hue family)
  "Small cladocera"  = "#8BCC8C",  # light green
  "Rotifera"         = "#B38FD3",  # medium pastel-purple
  "Ostracoda"        = "#E5C58B",  # sand / buff
  "Insecta"          = "#F4A261"  # warm pastel orange
)

weekly_bar_by_site <- ggplot(zoop_weekly_group[!(zoop_weekly_group$group %in% c("Rare", "Copepoda")),],
                            aes(x = week_start, y = totezoop, fill = group)) +
  geom_bar(stat = "identity", width = 7) +
  facet_grid(Sitefac ~ ., scales = "free_y") +
  theme_bw() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%1") +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_fill_manual(values = pastel_bold_pal) +
  labs(
    x = NULL,
    y = "Zooplankton Density (m^-3)",
    fill = "Group",
    title = "Weekly Zooplankton Density by Site"
  ) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

png("Output/Figures/Zoop_weekly%02d.png", height = 9, width = 6.5, units = "in", res = 1000, family = "serif")
weekly_bar_by_site
weekly_bar_by_site + scale_y_sqrt()
dev.off()

# 9. NMDS 1: GROUPED TAXA (Category)----

# cast table
zoopgroup <- zooplong %>%
  group_by(Site, Date, wyjday, Year, group, Region) %>%
  summarise(sumtot = sum(Density, na.rm = TRUE), .groups="drop")

zoopcast_group <- dcast(
  zoopgroup,
  formula = Site + Date + wyjday + Year + Region ~ group,
  value.var = "sumtot",
  fun.aggregate = sum,
  fill = 0
)

# NMDS prep
com_group <- decostand(zoopcast_group[, -(1:5)], method = "range")

nmds_group <- metaMDS(com_group, autotransform = FALSE)

dist_raw <- vegdist(zoopcast_group[, -(1:5)], method="bray")
pcoa_raw <- cmdscale(dist_raw, eig=TRUE)

# Extract site & species scores
nmds_group_sites <- as.data.frame(scores(nmds_group, display = "sites"))
nmds_group_sites$Site   <- zoopcast_group$Site
nmds_group_sites$wyjday <- zoopcast_group$wyjday
nmds_group_sites$Region <- zoopcast_group$Region

nmds_group_species <- as.data.frame(scores(nmds_group, display = "species"))
nmds_group_species$group <- rownames(nmds_group_species)

png("Output/Figures/Zoop_nmds_groups%02d.png", height = 6, width = 6.5, units = "in", res = 1000, family = "serif")

ggplot() + 
  geom_point(data = nmds_group_sites, aes(x = NMDS1, y = NMDS2, color = Site)) +
  geom_point(data = nmds_group_species, aes(x = NMDS1, y=NMDS2), size = .8) +
  # geom_path(data = nmds_group_sites, aes(x = NMDS1, y = NMDS2, color = Site, group = Site)) +
  geom_text_repel(data=nmds_group_species,aes(x=NMDS1,y=NMDS2,label=group), alpha=0.9, size = 3, force = .1) +
  scale_color_viridis_d()+ scale_fill_viridis_d() + labs() +
  theme_bw(base_family = "serif") + #theme(legend.position = "none") +
  stat_ellipse(data = nmds_group_sites[nmds_group_sites$Site %in% c("FWBN", "CCSYB", "RD22"),], 
               aes(x = NMDS1, y = NMDS2, fill = Site, group = Site), geom = "polygon", alpha = .1) +
  stat_ellipse(data = nmds_group_sites[nmds_group_sites$Site %in% c("FWBN", "CCSYB", "RD22"),], 
               aes(x = NMDS1, y = NMDS2, color = Site, group = Site), alpha = .5)

ggplot() + 
  geom_point(data = nmds_group_sites[nmds_group_sites$Site %in% c("FWBN", "CCSYB", "CNW", "TEW", "SB4"),], 
             aes(x = NMDS1, y = NMDS2, color = Site, shape = Site)) +
  geom_point(data = nmds_group_species, aes(x = NMDS1, y=NMDS2), size = .8) +
  geom_path(data = nmds_group_sites[nmds_group_sites$Site %in% c("FWBN", "CCSYB", "CNW", "TEW", "SB4"),], 
            aes(x = NMDS1, y = NMDS2, color = Site, group = Site)) +
  geom_text_repel(data=nmds_group_species,aes(x=NMDS1,y=NMDS2,label=group), alpha=0.9, size = 3, force = .1) +
  animCol + animFill +
  theme_bw(base_family = "serif") + #theme(legend.position = "none") +
  stat_ellipse(data = nmds_group_sites[nmds_group_sites$Site %in% c("FWBN", "CCSYB", "CNW", "TEW", "SB4"),], 
               aes(x = NMDS1, y = NMDS2, fill = Site, group = Site), geom = "polygon", alpha = .1) +
  stat_ellipse(data = nmds_group_sites[nmds_group_sites$Site %in% c("FWBN", "CCSYB", "CNW", "TEW", "SB4"),], 
               aes(x = NMDS1, y = NMDS2, color = Site, group = Site), alpha = .5)

ggplot() + 
  geom_point(data = nmds_group_sites, aes(x = NMDS1, y = NMDS2, color = wyjday)) +
  geom_point(data = nmds_group_species, aes(x = NMDS1, y=NMDS2), size = .8) +
  geom_path(data = nmds_group_sites, aes(x = NMDS1, y = NMDS2, color = wyjday, group = wyjday)) +
  geom_text_repel(data=nmds_group_species,aes(x=NMDS1,y=NMDS2,label=group), alpha=0.9, size = 3, force = .1) +
  scale_color_viridis_c()+ scale_fill_viridis_c() + labs(title = "NMDS - 2016") +
  theme_bw(base_family = "serif") + #theme(legend.position = "none") +
  stat_ellipse(data = nmds_group_sites, 
               aes(x = NMDS1, y = NMDS2, fill = wyjday, group = wyjday), geom = "polygon", alpha = .1) +
  stat_ellipse(data = nmds_group_sites, 
               aes(x = NMDS1, y = NMDS2, color = wyjday, group = wyjday), alpha = .5)

dev.off()

# 10. NMDS 2: SPECIES-LEVEL RESOLUTION ----

top_species <- zooplong %>%
  group_by(Group_family) %>%
  summarise(total = sum(Density), .groups = "drop") %>%
  arrange(desc(total)) %>%
  slice_head(n = 25) %>%  # You can adjust this number
  pull(Group_family)

zoopsp <- zooplong %>%
  # filter(Group_family %in% top_species) %>%
  group_by(Site, Date, wyjday, Year, Group_family, Region) %>%
  summarise(sumtot = sum(Density, na.rm = TRUE), .groups="drop")

zoopcast_spec <- dcast(
  zoopsp,
  formula = Site + Date + wyjday + Year + Region ~ Group_family,
  value.var = "sumtot",
  fun.aggregate = sum,
  fill = 0
)

# NMDS prep
com_spec <- decostand(zoopcast_spec[, -(1:5)], method = "hellinger")

# nmds_spec <- metaMDS(zoopcast_spec[, -(1:5)], autotransform = FALSE)
nmds_spec <- metaMDS(com_spec[,], autotransform = FALSE)

# Extract site & species scores
nmds_spec_sites <- as.data.frame(scores(nmds_spec, display = "sites"))
nmds_spec_sites$Site   <- zoopcast_spec$Site
nmds_spec_sites$wyjday <- zoopcast_spec$wyjday
nmds_spec_sites$Region <- zoopcast_spec$Region

nmds_spec_species <- as.data.frame(scores(nmds_spec, display = "species"))
nmds_spec_species$Group_family <- rownames(nmds_spec_species)

ggplot() + 
  geom_point(data = nmds_spec_sites, aes(x = NMDS1, y = NMDS2, color = Site)) +
  geom_point(data = nmds_spec_species, aes(x = NMDS1, y=NMDS2), size = .8) +
  # geom_path(data = nmds_spec_sites, aes(x = NMDS1, y = NMDS2, color = Site, group = Site)) +
  geom_text_repel(data=nmds_spec_species,aes(x=NMDS1,y=NMDS2,label=Group_family), alpha=0.9, size = 3, force = .1) +
  scale_color_viridis_d()+ scale_fill_viridis_d() + labs(title = "NMDS - 2016") +
  theme_bw(base_family = "serif") + #theme(legend.position = "none") +
  stat_ellipse(data = nmds_spec_sites[nmds_spec_sites$Site %in% c("FWBN", "CCSYB", "RD22"),], 
               aes(x = NMDS1, y = NMDS2, fill = Site, group = Site), geom = "polygon", alpha = .1) +
  stat_ellipse(data = nmds_spec_sites[nmds_spec_sites$Site %in% c("FWBN", "CCSYB", "RD22"),], 
               aes(x = NMDS1, y = NMDS2, color = Site, group = Site), alpha = .5)

ggplot() + 
  geom_point(data = nmds_spec_sites[nmds_spec_sites$Site %in% c("FWBN", "CCSYB", "RD22"),], aes(x = NMDS1, y = NMDS2, color = Site)) +
  geom_point(data = nmds_spec_species, aes(x = NMDS1, y=NMDS2), size = .8) +
  geom_path(data = nmds_spec_sites[nmds_spec_sites$Site %in% c("FWBN", "CCSYB", "RD22"),], aes(x = NMDS1, y = NMDS2, color = Site, group = Site)) +
  geom_text_repel(data=nmds_spec_species,aes(x=NMDS1,y=NMDS2,label=Group_family), alpha=0.9, size = 3, force = .1) +
  scale_color_viridis_d()+ scale_fill_viridis_d() + labs(title = "NMDS - 2016") +
  theme_bw(base_family = "serif") + #theme(legend.position = "none") +
  stat_ellipse(data = nmds_spec_sites[nmds_spec_sites$Site %in% c("FWBN", "CCSYB", "RD22"),], 
               aes(x = NMDS1, y = NMDS2, fill = Site, group = Site), geom = "polygon", alpha = .1) +
  stat_ellipse(data = nmds_spec_sites[nmds_spec_sites$Site %in% c("FWBN", "CCSYB", "RD22"),], 
               aes(x = NMDS1, y = NMDS2, color = Site, group = Site), alpha = .5)

ggplot() + 
  geom_point(data = nmds_spec_sites, aes(x = NMDS1, y = NMDS2, color = wyjday)) +
  geom_point(data = nmds_spec_species, aes(x = NMDS1, y=NMDS2), size = .8) +
  geom_path(data = nmds_spec_sites, aes(x = NMDS1, y = NMDS2, color = wyjday, group = wyjday)) +
  geom_text_repel(data=nmds_spec_species,aes(x=NMDS1,y=NMDS2,label=Group_family), alpha=0.9, size = 3, force = .1) +
  scale_color_viridis_c()+ scale_fill_viridis_c() + labs(title = "NMDS - 2016") +
  theme_bw(base_family = "serif") + #theme(legend.position = "none") +
  stat_ellipse(data = nmds_spec_sites, 
               aes(x = NMDS1, y = NMDS2, fill = wyjday, group = wyjday), geom = "polygon", alpha = .1) +
  stat_ellipse(data = nmds_spec_sites, 
               aes(x = NMDS1, y = NMDS2, color = wyjday, group = wyjday), alpha = .5)

# 11. Plotly 3D visualization ----

vis3d <- function(nmdsdf){

  nmdsdf <- nmds_spec_sites
### Clean site names and define site order
  nmdsdf$site<- trimws(as.character(nmdsdf$Site))
  site_levels <- unique(nmdsdf$site)
  
  ### Palette data frame
  
  palnmdsdf <- data.frame(
    "sites" = c("AL0", "CCSYB", "CNW", "FW1", "FWBN", "KLWW", "KNG3", 
                "LIS", "LP", "RD22", "SB4", "STTD", "TER", "TEW", 
                "WDSYB", "YBLR4"),
    "color" = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
                "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", 
                "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5", 
                "#c49c94")
  )
  
  # Convert palette dataframe to named vector
  pal_named <- setNames(palnmdsdf$color, palnmdsdf$sites)
  
  ### add MARKERS by site (scalar color per trace)
  
  p <- plot_ly()
  
  for (s in site_levels) {
    
    df_site <- nmdsdf %>% filter(site == s)
    
    p <- p %>% add_trace(
      data   = df_site,
      x      = ~NMDS1,
      y      = ~NMDS2,
      z      = ~wyjday,
      type   = "scatter3d",
      mode   = "markers",
      marker = list(size = 4, opacity = 0.8),
      color  = pal_named[s],      # *** SCALAR COLOR ***
      name   = s,                 # legend entry
      legendgroup = s,
      showlegend = TRUE           # only markers show in legend
    )
  }
  
  ### ADD TRAJECTORY LINES BY SITE
  
  
  for (s in site_levels) {
    
    df_site <- nmdsdf %>% filter(site == s) %>%
      arrange(wyjday)
    
    p <- p %>% add_trace(
      data = df_site,
      x = ~NMDS1,
      y = ~NMDS2,
      z = ~wyjday,
      type = "scatter3d",
      mode = "lines",
      line = list(width = 3, color = pal_named[s]),   # same scalar color
      name = s,
      legendgroup = s,
      showlegend = FALSE          # no duplicate legend entries
    )
  }
  
  ### CYLINDER GENERATOR
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
    
    list(
      x = x,
      y = y,
      z = z,
      i = i - 1,
      j = j - 1,
      k = k - 1
    )
  }
  
  ### Compute cylinder stats (FWBN & CCSYB)
  
  cyl_sites <- c("FWBN", "CCSYB")
  
  cyl_stats <- nmdsdf %>%
    filter(site %in% cyl_sites) %>%
    group_by(site) %>%
    group_modify(~{
      pts  <- st_as_sf(.x, coords = c("NMDS1","NMDS2"), crs = NA)
      hull <- st_convex_hull(st_combine(pts))
      tibble(
        site = unique(.x$site),
        area = as.numeric(st_area(hull)),
        radius = sqrt(as.numeric(st_area(hull)) / pi),
        cx = st_coordinates(st_centroid(hull))[1],
        cy = st_coordinates(st_centroid(hull))[2]
      )
    })
  
  ### ADD CYLINDERS
  
  for (i in 1:nrow(cyl_stats)) {
    
    t <- cyl_stats[i, ]
    
    cyl <- make_cylinder(
      cx   = t$cx,
      cy   = t$cy,
      r    = t$radius,
      zmin = min(nmdsdf$wyjday, na.rm = TRUE),
      zmax = max(nmdsdf$wyjday, na.rm = TRUE)
    )
    
    p <- p %>% add_trace(
      x = cyl$x, y = cyl$y, z = cyl$z,
      i = cyl$i, j = cyl$j, k = cyl$k,
      type = "mesh3d",
      color = pal_named[t$site],   # same scalar color
      opacity = 0.25,
      name = t$site,
      legendgroup = t$site,
      showlegend = TRUE           # keep legend clean
    )
  }
  
  ### FINAL PLOT LAYOUT
  
  p <- p %>% layout(
    scene = list(
      xaxis = list(title = "NMDS1"),
      yaxis = list(title = "NMDS2"),
      zaxis = list(title = "WY Julian Day"),
      aspectmode = "manual",
      aspectratio = list(x = 1.2, y = 1.2, z = 0.8)
    )
  )
  
  p
}

vis3d(nmds_group_sites)

vis3d(nmds_spec_sites)


# 12. Indicator species analysis ----------------------------------------------


library(indicspecies)

# zoopcast_group or zoopcast_spec can be used for ISA
isa <- multipatt(zoopcast_spec[, -(1:5)], zoopcast_spec$Site, control = how(nperm=999))
summary(isa)

# 13. DTW analysis --------------------------------------------------------

library(dtw)

# total weekly zoop density across all groups
zoop_weekly_ts <- zooplong %>%
  mutate(week_start = floor_date(Date, "week")) %>%
  group_by(Site, week_start) %>%
  summarise(total_density = sum(Density, na.rm = TRUE), .groups = "drop")


library(tidyr)

ts_mat <- zoop_weekly_ts %>%
  pivot_wider(
    names_from = week_start,
    values_from = total_density,
    values_fill = 0
  ) %>%
  arrange(Site)

# extract matrix
ts_data <- as.matrix(ts_mat[,-1])
rownames(ts_data) <- ts_mat$Site

site_names <- rownames(ts_data)
n <- length(site_names)

dtw_dist <- matrix(0, n, n, dimnames=list(site_names, site_names))

for (i in 1:n) {
  for (j in 1:n) {
    alignment <- dtw(ts_data[i,], ts_data[j,])
    dtw_dist[i,j] <- alignment$distance
  }
}
hc_dtw <- hclust(as.dist(dtw_dist), method = "ward.D2")

plot(hc_dtw,
     main="Temporal DTW Clustering of Zooplankton Sites",
     xlab="Site",
     sub="")

clusters <- cutree(hc_dtw, k = 3)   # choose desired number
print(clusters)


library(pheatmap)

pheatmap(dtw_dist,
         clustering_method="ward.D2",
         main="DTW Temporal Distance Between Sites",
         fontsize=10)

zoop_weekly_ts$cluster <- clusters[zoop_weekly_ts$Site]

ggplot(zoop_weekly_ts, aes(x = week_start, y = total_density, color = Site)) +
  geom_line() +
  facet_wrap(~cluster, scales="free_y") +
  theme_bw() +
  labs(title = "Temporal Zooplankton Density Patterns by DTW Cluster",
       y = "Density (m^-3)")

library(dtwclust)

dba_centroid <- tsclust(ts_data,
                        type = "hierarchical",
                        k = 3,
                        distance = "dtw_basic",
                        centroid = "dba")



###########################################################
### FULL DTW NMDS USING GROUP-SPECIFIC TRAJECTORIES (ROBUST)
############################################################

library(tidyverse)
library(lubridate)
library(dtw)
library(vegan)
library(ggrepel)
zoopcast_group
### STEP 1 — Weekly time series by zoop group
zoop_weekly_group_ts <- zooplong %>%
  mutate(week_start = floor_date(Date, "week")) %>%
  group_by(Site, week_start, group) %>%
  summarise(totezoop = sum(Density, na.rm = TRUE), .groups = "drop")

### STEP 2 — Get site list
site_names <- unique(zoop_weekly_group_ts$Site)
group_names <- unique(zoop_weekly_group_ts$group)

dtw_list <- list()

### STEP 3 — Build DTW matrix per group
for(g in group_names){
  
  # rebuild FULL matrix for this group with ALL sites included
  ts_sub <- zoop_weekly_group_ts %>%
    filter(group == g) %>%
    select(Site, week_start, totezoop) %>%
    pivot_wider(names_from = week_start,
                values_from = totezoop,
                values_fill = 0) %>%
    right_join(tibble(Site = site_names), by = "Site") %>%
    arrange(Site) %>%
    select(-Site) %>%
    as.matrix()
  
  rownames(ts_sub) <- site_names
  
  # Z-score normalization per site (row)
  ts_norm <- t(scale(t(ts_sub), center = TRUE, scale = TRUE))
  
  # replace NaN or Inf (flat or zero-only sites)
  ts_norm[!is.finite(ts_norm)] <- 0
  
  # DTW-safe jitter for rows that become constant
  for(i in 1:nrow(ts_norm)) {
    if(all(abs(ts_norm[i,]) < 1e-9)) {
      ts_norm[i,] <- ts_norm[i,] + 1e-6
    }
  }
  
  
  
  # compute DTW distances
  n <- length(site_names)
  dist_mat <- matrix(0, n, n, dimnames=list(site_names, site_names))
  
  for(i in 1:n){
    for(j in 1:n){
      dist_mat[i,j] <- dtw(ts_norm[i,], ts_norm[j,])$distance
    }
  }
  
  dtw_list[[g]] <- dist_mat
}

### STEP 4 — Combine DTW distances across groups
dtw_combined <- Reduce("+", dtw_list) / length(dtw_list)

### STEP 5 — NMDS on DTW distances
nmds_dtw_group <- metaMDS(
  dtw_combined,
  distance = "none",
  autotransform = FALSE,
  trymax = 100
)

### STEP 6 — Extract NMDS scores
dtw_scores <- as.data.frame(scores(nmds_dtw_group, "sites"))
dtw_scores$Site <- rownames(dtw_scores)

dtw_scores <- dtw_scores %>%
  left_join(zoop26sites %>% select(Site_id, Region, Sitetype),
            by = c("Site" = "Site_id"))

### STEP 7 — Plot

ggplot(dtw_scores, aes(x = NMDS1, y = NMDS2, color = Site, label = Site)) +
  geom_point(size = 4) +
  geom_text_repel() +
  theme_bw() +
  # scale_color_manual(values = pal_named) +
  labs(
    title = "DTW-based NMDS using Group-Specific Zooplankton Trajectories",
    subtitle = "Temporal phenology similarity across sites (DTW)",
    x = "NMDS1", 
    y = "NMDS2",
    color = "Site"
  )


library(vegan)
library(tidyverse)
library(ggrepel)

# 1 — Build raw density matrix
#    zoopcast already contains group or species columns in wide format
density_cols <- names(zoopcast_group)[6:ncol(zoopcast_group)]
com_density <- zoopcast_group[, density_cols]

# 2 — Log-transform densities to stabilize magnitude effects
com_log <- log1p(com_density)     # log(1 + density)

# 3 — Bray-Curtis NMDS on log densities
nmds_log <- metaMDS(
  com_log,
  distance = "bray",
  autotransform = FALSE,
  trymax = 200
)

# 4 — Extract NMDS site scores
nmds_log_scores <- as.data.frame(scores(nmds_log, "sites"))
nmds_log_scores$Site <- zoopcast_group$Site
nmds_log_scores$Region <- zoopcast_group$Region

# compute total density (raw density)
nmds_log_scores$TotalDensity <- rowSums(com_density, na.rm = TRUE)

# rescale density for color intensity (0–1)
nmds_log_scores$DensityScaled <- scales::rescale(nmds_log_scores$TotalDensity)

# 5 — Plot
plotly::ggplotly(ggplot(nmds_log_scores,
       aes(x = NMDS1, y = NMDS2,
           label = Site,
           size = TotalDensity,        # magnitude signal
           color = DensityScaled)) +    # magnitude signal
  geom_point(alpha = 0.8) +
  geom_text_repel(size = 4) +
  scale_color_viridis_c(option = "magma") +
  scale_size(range = c(3, 14)) +
  theme_bw() +
  labs(
    title = "NMDS (Bray) using Log-Densities",
    subtitle = "Points scaled by total zooplankton density (Option 6)",
    x = "NMDS1",
    y = "NMDS2",
    size = "Total Density",
    color = "Scaled Density"
  ))

ggplot(nmds_log_scores,
       aes(x = NMDS1, y = NMDS2,
           label = Site,
           size = TotalDensity,        # magnitude signal
           color = DensityScaled)) +    # magnitude signal
  geom_point(alpha = 0.8) +
  geom_text_repel(size = 4) +
  scale_color_viridis_c(option = "magma") +
  scale_size(range = c(3, 14)) +
  theme_bw() +
  labs(
    title = "NMDS (Bray) using Log-Densities",
    subtitle = "Points scaled by total zooplankton density (Option 6)",
    x = "NMDS1",
    y = "NMDS2",
    size = "Total Density",
    color = "Scaled Density"
  )
