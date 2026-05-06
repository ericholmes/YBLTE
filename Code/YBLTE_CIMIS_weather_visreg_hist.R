library(tidyverse)
library(janitor)
library(lubridate)
library(scales)
library(mgcv)
# Load and clean CIMIS data ----
cimis <- read.csv("Data/tabular/CIMIS/Cimis_Davis_1983-2026.csv") %>%
  clean_names()

cimis$date <- as.Date(cimis$date, format = "%m/%d/%Y")
str(cimis)

# ============================================================
# 1. Prepare CIMIS data
# ============================================================

cimis <- cimis %>%
  mutate(
    date = as_date(date),
    year = year(date),
    wy = if_else(month(date) >= 10, year(date) + 1, year(date)),
    dowy = as.numeric(date - ymd(paste0(wy - 1, "-10-01"))) + 1
  )

# ============================================================
# 2. Build long-term daily climatology (by DOWY)
# ============================================================

clim <- cimis %>%
  group_by(dowy) %>%
  summarise(
    clim_avg_f = mean(avg_air_temp_f, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# 3. Compute anomalies
# ============================================================

cimis_anom <- cimis %>%
  left_join(clim, by = "dowy") %>%
  mutate(anom_f = avg_air_temp_f - clim_avg_f)

# ============================================================
# 4. Restrict to Oct 1 → May 1 (DOWY 1–213)
# ============================================================

cimis_anom <- cimis_anom %>%
  filter(dowy >= 1, dowy <= 213)

# ============================================================
# 5. Exclude incomplete years
# ============================================================

min_days <- 150

valid_years <- cimis_anom %>%
  count(wy) %>%
  filter(n >= min_days) %>%
  pull(wy)

cimis_hist <- cimis_anom %>%
  filter(wy %in% valid_years)

# ============================================================
# 6. Historical LOESS — ANOMALIES
# ============================================================

hist_smooths_anom <- cimis_hist %>%
  group_by(wy) %>%
  group_modify(~{
    dat <- .x
    lo <- loess(anom_f ~ dowy, data = dat, span = 0.2)
    tibble(
      wy = unique(dat$wy),
      dowy = 1:213,
      fit = predict(lo, newdata = tibble(dowy = 1:213))
    )
  }) %>%
  group_by(wy) %>%
  mutate(
    se = abs(fit - mean(fit, na.rm = TRUE)),
    se_scaled = (se - min(se)) / (max(se) - min(se)),
    weight = 1 - se_scaled,
    alpha = scales::rescale(weight, to = c(0.02, 0.25))
  )

# ============================================================
# 7. Historical LOESS — ACTUAL TEMPERATURES
# ============================================================

hist_smooths_temp <- cimis_hist %>%
  group_by(wy) %>%
  group_modify(~{
    dat <- .x
    lo <- loess(avg_air_temp_f ~ dowy, data = dat, span = 0.2)
    tibble(
      wy = unique(dat$wy),
      dowy = 1:213,
      fit = predict(lo, newdata = tibble(dowy = 1:213))
    )
  }) %>%
  group_by(wy) %>%
  mutate(
    se = abs(fit - mean(fit, na.rm = TRUE)),
    se_scaled = (se - min(se)) / (max(se) - min(se)),
    weight = 1 - se_scaled,
    alpha = scales::rescale(weight, to = c(0.02, 0.25))
  )

# ============================================================
# 8. Current water year
# ============================================================

current_wy <- 2026

cimis_wy <- cimis_anom %>%
  filter(wy == current_wy) %>%
  arrange(date) %>%
  mutate(x = as.numeric(date))

# ============================================================
# 9. LOESS — CURRENT WY ANOMALIES
# ============================================================

loess_fit_anom <- loess(anom_f ~ x, data = cimis_wy, span = 0.15)

pred_anom <- tibble(
  date = seq(min(cimis_wy$date), max(cimis_wy$date), by = "1 day")
) %>%
  mutate(
    x = as.numeric(date),
    dowy = as.numeric(date - ymd(paste0(current_wy - 1, "-10-01"))) + 1,
    fit = predict(loess_fit_anom, newdata = tibble(x = x))
  )

# Residual-based uncertainty
cimis_wy <- cimis_wy %>%
  mutate(
    fit_loess_anom = predict(loess_fit_anom),
    resid_anom = anom_f - fit_loess_anom
  )

resid_loess_anom <- loess(abs(resid_anom) ~ x, data = cimis_wy, span = 0.3)

pred_anom$se <- predict(resid_loess_anom, newdata = pred_anom)

se_range <- range(pred_anom$se, na.rm = TRUE)

pred_anom <- pred_anom %>%
  mutate(
    se_scaled = (se - se_range[1]) / diff(se_range),
    weight = 1 - se_scaled,
    alpha = scales::rescale(weight, to = c(0.3, 1))
  )

# ============================================================
# 10. LOESS — CURRENT WY ACTUAL TEMPERATURES
# ============================================================

loess_fit_temp <- loess(avg_air_temp_f ~ x, data = cimis_wy, span = 0.15)

pred_temp <- tibble(
  date = seq(min(cimis_wy$date), max(cimis_wy$date), by = "1 day")
) %>%
  mutate(
    x = as.numeric(date),
    dowy = as.numeric(date - ymd(paste0(current_wy - 1, "-10-01"))) + 1,
    fit = predict(loess_fit_temp, newdata = tibble(x = x))
  )

# Residual-based uncertainty
cimis_wy <- cimis_wy %>%
  mutate(
    fit_loess_temp = predict(loess_fit_temp),
    resid_temp = avg_air_temp_f - fit_loess_temp
  )

resid_loess_temp <- loess(abs(resid_temp) ~ x, data = cimis_wy, span = 0.3)

pred_temp$se <- predict(resid_loess_temp, newdata = pred_temp)

se_range_temp <- range(pred_temp$se, na.rm = TRUE)

pred_temp <- pred_temp %>%
  mutate(
    se_scaled = (se - se_range_temp[1]) / diff(se_range_temp),
    weight = 1 - se_scaled,
    alpha = scales::rescale(weight, to = c(0.3, 1))
  )

# ============================================================
# 11. Fog + heat windows (in DOWY)
# ============================================================

wy_start_date <- ymd("2025-10-01")   # start of WY 2026

fog_start  <- as.numeric(ymd("2025-11-20") - wy_start_date) + 1
fog_end    <- as.numeric(ymd("2025-12-10") - wy_start_date) + 1

heat_start <- as.numeric(ymd("2026-03-10") - wy_start_date) + 1
heat_end   <- as.numeric(ymd("2026-03-25") - wy_start_date) + 1

# ============================================================
# 12. PLOT 1 — Temperature Anomalies
# ============================================================

p1 <- ggplot() +
  geom_line(
    data = hist_smooths_anom,
    aes(dowy, fit, group = wy),
    alpha = .1,
    color = "grey40",
    linewidth = 0.6
  ) +
  scale_alpha_identity() +
  annotate("rect", xmin = fog_start, xmax = fog_end,
           ymin = -Inf, ymax = Inf, fill = "lightblue", alpha = 0.15) +
  annotate("rect", xmin = heat_start, xmax = heat_end,
           ymin = -Inf, ymax = Inf, fill = "tomato", alpha = 0.10) +
  geom_line(
    data = pred_anom,
    aes(dowy, fit),
    color = "firebrick",
    linewidth = 1.4
  ) + scale_x_continuous(
    breaks = c(1, 32, 62, 93, 124, 155, 184, 214),
    labels = c("Oct", "Nov","Dec", "Jan", "Feb", "Mar", "Apr", "May"),
    limits = c(1, 214)) +
  labs(
    title = "Temperature Anomalies — LOESS",
    x = "Day of Water Year",
    y = "Temperature anomaly (°F)"
  ) +
  theme_bw()

# ============================================================
# 13. PLOT 2 — Actual Temperatures (WITH HISTORICAL BACKDROP)
# ============================================================

p2 <- ggplot() +
  geom_line(
    data = hist_smooths_temp,
    aes(dowy, fit, group = wy), alpha = .1,
    color = "grey40",
    linewidth = 0.6
  ) +
  scale_alpha_identity() +
  annotate("rect", xmin = fog_start, xmax = fog_end,
           ymin = -Inf, ymax = Inf, fill = "lightblue", alpha = 0.15) +
  annotate("rect", xmin = heat_start, xmax = heat_end,
           ymin = -Inf, ymax = Inf, fill = "tomato", alpha = 0.10) +
  geom_line(
    data = pred_temp,
    aes(dowy, fit),
    color = "steelblue4",
    linewidth = 1.4
  ) +  scale_x_continuous(
    breaks = c(1, 32, 62, 93, 124, 155, 184, 214),
    labels = c("Oct", "Nov","Dec", "Jan", "Feb", "Mar", "Apr", "May"),
    limits = c(1, 214)
  ) +
  labs(
    title = "Daily average air temperature — LOESS",
    x = "Day of Water Year",
    y = "Temperature (°F)"
  ) +
  theme_bw()

p1

png("Output/Figures/CIMIS_visreg_wy%03d.png",
    height = 5.5, width = 6.5, units = "in", res = 300)
p1
p2
dev.off()

