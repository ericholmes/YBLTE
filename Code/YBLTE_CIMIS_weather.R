library(tidyverse)
library(janitor)
library(lubridate)

# Load and clean CIMIS data ----
cimis <- read.csv("Data/tabular/CIMIS/Cimis_Davis_1983-2026.csv") %>%
  clean_names()

cimis$date <- as.Date(cimis$date, format = "%m/%d/%Y")

# Filter to 2026 water year ----
cimis_wy <- cimis %>%
  filter(date >= as.Date("2025-10-01"))

# Select variables + tule fog classifier ----
cimis_wy <- cimis_wy %>%
  transmute(
    date,
    solar = sol_rad_ly_day,
    precip = precip_in,
    wind = avg_wind_speed_mph,
    temp = avg_air_temp_f
  ) %>%
  mutate(
    tule_fog = solar < 150 & wind < 6 & temp < 55
  )

# Identify continuous fog periods ----
cimis_wy <- cimis_wy %>%
  mutate(fog_group = cumsum(tule_fog != lag(tule_fog, default = FALSE)))

fog_ranges <- cimis_wy %>%
  filter(tule_fog) %>%
  group_by(fog_group) %>%
  summarize(start = min(date), end = max(date), .groups = "drop")

# Prepare data for plotting ----
cimis_long <- cimis_wy %>%
  transmute(
    date,
    `Solar Irr. (Ly/day)` = solar,
    `Precip. (in)` = precip,
    `Wind Speed (mph)` = wind,
    `Air Temp. (°F)` = temp
  ) %>%
  pivot_longer(cols = -date, names_to = "Variable", values_to = "Value")

# Plot with fog shading ----

png(paste0("Output/Figures/YBLTE_CIMIS_2026_%02d.png"),
    width = 6.5, height = 6, units = "in", res = 1000, family = "serif")

ggplot() +
  geom_rect(data = fog_ranges,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    fill = "gray60", alpha = 0.5) +
  geom_col(data = subset(cimis_long, Variable == "Precip. (in)"),
    aes(x = date, y = Value),
    fill = "steelblue", alpha = 0.9) +
  geom_line(data = subset(cimis_long, Variable != "Precip. (in)"),
    aes(x = date, y = Value),
    color = "black", linewidth = 0.6) +
  facet_grid(Variable ~ ., scales = "free_y") +
  scale_x_date(date_labels = "%b-1", breaks = "1 month") +
  theme_bw(base_size = 13) +
  theme(strip.text = element_text(size = 11),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank()) +
  labs(title = "CIMIS Weather Conditions – 2026 Water Year (Davis, CA)",
    x = NULL)

dev.off()
