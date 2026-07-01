library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggtern)
library(purrr)
library(readxl)
library(patchwork)

# ---------------------------------------------------------
# 0. Load data
# ---------------------------------------------------------

wq_raw <- read_excel("Data/tabular/YBLTE_point_wq.xlsx") %>%
  filter(Sample_Type == "zoop")

# north → south site order (mixing sites only)
site_order <- c(
  "FW1",
  "KNG3", "CNW", "RD22",
  "YBLR4", "SB4",
  "AL0", "LIS", "STTD", "TEW", "TER"
)

endmember_sites <- c("FWBN", "CCSYB", "KLWW")

# ---------------------------------------------------------
# 1. Prepare data
# ---------------------------------------------------------

wq <- wq_raw %>%
  mutate(
    datetime = as_datetime(Date),
    week = floor_date(datetime, "week"),
    SPC = SPC_uscm,
    FDOM = suppressWarnings(as.numeric(fdom_qsu))
  ) %>%
  filter(!is.na(SPC), !is.na(FDOM))

# ---------------------------------------------------------
# 2. Weekly endmembers
# ---------------------------------------------------------

endm <- wq %>%
  filter(Site %in% endmember_sites) %>%
  group_by(week, Site) %>%
  summarize(SPC = mean(SPC), FDOM = mean(FDOM), .groups = "drop") %>%
  pivot_wider(
    names_from = Site,
    values_from = c(SPC, FDOM),
    names_sep = "_"
  )

# ---------------------------------------------------------
# 3. Weekly mixing sites
# ---------------------------------------------------------

mix <- wq %>%
  filter(!Site %in% endmember_sites) %>%
  group_by(week, Site) %>%
  summarize(SPC = mean(SPC), FDOM = mean(FDOM), .groups = "drop")

# ---------------------------------------------------------
# 4. Join mixing sites with endmembers
# ---------------------------------------------------------

emma_df <- mix %>%
  left_join(endm, by = "week")

# ---------------------------------------------------------
# 5. PCA (EMMA step)
# ---------------------------------------------------------

pca <- prcomp(emma_df %>% select(SPC, FDOM), scale. = TRUE)

emma_df <- emma_df %>%
  mutate(
    PC1 = pca$x[,1],
    PC2 = pca$x[,2]
  )

# ---------------------------------------------------------
# 6. Project endmembers into PC space
# ---------------------------------------------------------

project_pc <- function(spc, fdom) {
  predict(pca, newdata = data.frame(SPC = spc, FDOM = fdom))
}

endm_pc <- endm %>%
  mutate(
    PC_FWBN  = project_pc(SPC_FWBN,  FDOM_FWBN),
    PC_CCSYB = project_pc(SPC_CCSYB, FDOM_CCSYB),
    PC_KLWW  = project_pc(SPC_KLWW,  FDOM_KLWW)
  ) %>%
  mutate(
    PC1_FWBN  = PC_FWBN[,1],  PC2_FWBN  = PC_FWBN[,2],
    PC1_CCSYB = PC_CCSYB[,1], PC2_CCSYB = PC_CCSYB[,2],
    PC1_KLWW  = PC_KLWW[,1],  PC2_KLWW  = PC_KLWW[,2]
  ) %>%
  select(-PC_FWBN, -PC_CCSYB, -PC_KLWW)

# ---------------------------------------------------------
# 7. EMMA solver
# ---------------------------------------------------------

solve_emma <- function(px, py, ex1, ey1, ex2, ey2, ex3, ey3) {
  A <- matrix(c(
    ex1, ex2, ex3,
    ey1, ey2, ey3,
    1,   1,   1
  ), 3, 3, byrow = TRUE)
  
  b <- c(px, py, 1)
  
  if (any(is.na(c(A, b)))) return(c(NA, NA, NA))
  
  out <- tryCatch(solve(A, b), error = function(e) c(NA, NA, NA))
  as.numeric(out)
}

# ---------------------------------------------------------
# 8. Compute EMMA fractions
# ---------------------------------------------------------

emma_fractions <- emma_df %>%
  rowwise() %>%
  mutate(
    frac = list(
      solve_emma(
        PC1, PC2,
        endm_pc$PC1_FWBN[match(week, endm_pc$week)],
        endm_pc$PC2_FWBN[match(week, endm_pc$week)],
        endm_pc$PC1_CCSYB[match(week, endm_pc$week)],
        endm_pc$PC2_CCSYB[match(week, endm_pc$week)],
        endm_pc$PC1_KLWW[match(week, endm_pc$week)],
        endm_pc$PC2_KLWW[match(week, endm_pc$week)]
      )
    ),
    f_FWBN_raw  = frac[1],
    f_CCSYB_raw = frac[2],
    f_KLWW_raw  = frac[3]
  ) %>%
  ungroup()

# ---------------------------------------------------------
# 9. Diagnostics + clamping + renormalization
# ---------------------------------------------------------

emma_clean <- emma_fractions %>%
  mutate(
    # raw sum
    sum_raw = f_FWBN_raw + f_CCSYB_raw + f_KLWW_raw,
    
    # clamp
    f_FWBN  = pmax(pmin(f_FWBN_raw,  1), 0),
    f_CCSYB = pmax(pmin(f_CCSYB_raw, 1), 0),
    f_KLWW  = pmax(pmin(f_KLWW_raw,  1), 0),
    
    sum_clamp = f_FWBN + f_CCSYB + f_KLWW,
    
    # renormalize for plotting
    f_FWBN_plot  = f_FWBN  / sum_clamp,
    f_CCSYB_plot = f_CCSYB / sum_clamp,
    f_KLWW_plot  = f_KLWW  / sum_clamp,
    
    # diagnostics
    neg_flag = (f_FWBN_raw < 0 | f_CCSYB_raw < 0 | f_KLWW_raw < 0),
    deviation = abs(sum_raw - 1),
    unexplained = pmax(0, 1 - sum_raw),
    
    unexplained_flag = case_when(
      neg_flag ~ "Extrapolated",
      deviation > 0.25 ~ "Large mismatch",
      deviation > 0.10 ~ "Moderate mismatch",
      TRUE ~ "Well explained"
    )
  )

# ---------------------------------------------------------
# 10. Percent-source long format
# ---------------------------------------------------------

emma_long <- emma_clean %>%
  select(week, Site, f_FWBN_plot, f_CCSYB_plot, f_KLWW_plot) %>%
  pivot_longer(
    cols = starts_with("f_"),
    names_to = "source",
    values_to = "fraction"
  ) %>%
  mutate(
    source = recode(source,
                    f_FWBN_plot  = "FWBN",
                    f_CCSYB_plot = "CCSYB",
                    f_KLWW_plot  = "KLWW"
    ),
    Site = factor(Site, levels = site_order)
  )

# ---------------------------------------------------------
# 11. Percent-source time series (north → south)
# ---------------------------------------------------------

(emmaplot <- ggplot(emma_long[is.na(emma_long$Site) == F,], aes(week, fraction, fill = source)) +
  geom_col(alpha=0.8) +
  facet_grid(Site ~ .) +
  scale_fill_manual(values = c(FWBN = "#B63679FF", CCSYB = "#FB8861FF", KLWW = "#482878FF")) +
  labs(
    title = "EMMA Percent Source Contribution",
    x = NULL,
    y = "Fraction",
    fill = "Water source"
  ) + scale_x_date(date_breaks = "1 month", date_labels = "%b-1",
                  limits = c(as.Date("2025-12-21"), as.Date("2026-04-19"))) +
  theme_bw())
  # theme(
  #   strip.placement = "outside",
  #   strip.background = element_rect(fill = "grey90"),
  #   axis.text.x = element_text(angle = 45, hjust = 1)
  # )
dates <- unique(data.frame(emma_long[is.na(emma_long$Site) == F,"week"]))

# ---------------------------------------------------------
# 12. Add flow
# ---------------------------------------------------------
source("Code/YBLTE_useful_functions.R")

cdec_stations <- c("RCS", "FRE", "CCY")

# sensor is param name, sensor_num is for access
sensor_codes <- data.frame(sensor = c("chla", "ec", "discharge_cfs", "fdom", 
                                      "wtemp_f", "domgl","ph", "turb_fnu",
                                      "stage_ft"), 
                           sensor_num = c(28, 100, 20, 266, 
                                          25, 61, 62, 221,
                                          1))
startdate <- "2025-10-1"
enddate <- "2026-05-31"

cdec <- data.frame()
for(station in cdec_stations){
  for(param in c(20)){
    print(paste("downloading:", station, param))
    try(cdec <- rbind(cdec, downloadCDEC(site_no = station, parameterCd = param, startDT = startdate , endDT = enddate)))
  }
}

cdec$Param_val <- as.numeric(cdec$Param_val)

cdecmerge <- merge(cdec, sensor_codes, by.x = "parameterCd", by.y = "sensor_num")

# Pivoting from long to wide
cdec_wide <- cdecmerge %>% select(-parameterCd) %>% 
  pivot_wider(names_from = sensor, values_from = Param_val)
cdec_wide$Date <- as.Date(cdec_wide$Datetime)
cdec_wide <- cdec_wide %>% filter((Datetime>as.POSIXct("2025-12-21"))&(Datetime<as.POSIXct("2026-04-19")))
trib_map <- c(
  "FRE" = "FWBN",   # Feather River
  "CCY" = "CCSYB",  # Cache Creek
  "PTC" = "KLWW",   # Putah Creek
  "RCS" = "KLWW"    # Knights Landing / Colusa Basin Drain
)
flow_daily <- cdec_wide %>%
  filter(Site_no %in% names(trib_map)) %>%
  mutate(
    trib = trib_map[Site_no],
    discharge_cfs = pmax(discharge_cfs, 0)  # no negative flows
  ) %>%
  group_by(Date, trib) %>%
  summarize(flow = median(discharge_cfs, na.rm = TRUE), .groups = "drop")
flow_in_time <- cdec_wide %>% filter(Site_no %in% c("RCS", "FRE", "CCY", "PTC")) %>%drop_na(discharge_cfs)

flow_zero <- flow_in_time
flow_zero[flow_zero$discharge_cfs<0, 'discharge_cfs'] <- 0

flow_perc <- flow_zero %>% group_by(Date, Site_no) %>% 
  summarize(median_flow = median(discharge_cfs)) %>% 
  group_by(Date) %>% 
  mutate(sumflow = sum(median_flow), percflow = 100*median_flow/sumflow)

# percent flow plot, stacked bar plot (daily increments)
(contpercflowplot <- ggplot(data = flow_perc, aes(x = Date, y = percflow, group = Site_no, fill = Site_no)) +
    geom_bar(stat = "identity", alpha = 0.8, width = 1) + 
    scale_fill_manual(values = c(FRE = "#B63679FF",CCY = "#FB8861FF", RCS = "#482878FF")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b-1") +
    labs(x = NULL, y = "Percent Flow", fill = "Water source") + theme_bw())

flow_in_time <- cdec_wide %>% filter(Site_no %in% c("RCS", "FRE", "CCY")) %>%drop_na(discharge_cfs)

flow_zero <- flow_in_time
flow_zero[flow_zero$discharge_cfs<0, 'discharge_cfs'] <- 0

flow_perc <- flow_zero %>% group_by(Date, Site_no) %>% 
  summarize(median_flow = median(discharge_cfs)) %>% 
  group_by(Date) %>% 
  mutate(sumflow = sum(median_flow), percflow = 100*median_flow/sumflow)

# percent flow plot, stacked bar plot (daily increments)
(contpercflowplot <- ggplot(data = flow_perc, aes(x = Date, y = percflow, group = Site_no, fill = Site_no)) +
    geom_bar(stat = "identity", alpha = 0.8, width = 1)  + 
    scale_fill_manual(values = c(FRE = "#B63679FF",CCY = "#FB8861FF", RCS = "#482878FF")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b-1") +
    labs(x = NULL, y = "Percent Flow", fill = "Water source") + 
    theme_bw()+ theme(axis.text.x = element_blank()))

(flow_plot <- ggplot(cdec_wide[cdec_wide$Site_no %in% c("CCY", "RCS", "FRE") & 
                                is.na(cdec_wide$discharge_cfs) == F,], aes(x = Datetime, y = discharge_cfs, color = Site_no)) + 
  geom_ribbon(data = cdec_wide[,],
              aes(ymax = discharge_cfs, ymin = 0, fill = Site_no), alpha = .1) +
  geom_line(alpha = .8, linewidth = .8) + scale_x_datetime(date_breaks = "1 month", date_labels = "%b-1") +
  geom_line(data = cdec_wide[cdec_wide$Site_no %in% c("LIS") & 
                               is.na(cdec_wide$discharge_cfs) == F,], alpha = .2) + 
  scale_color_manual(values = c(FRE = "#B63679FF",CCY = "#FB8861FF",RCS = "#482878FF")) +
  scale_fill_manual(values = c(FRE = "#B63679FF",CCY = "#FB8861FF", RCS = "#482878FF")) +
  theme_bw() + labs(y = "Discharge (cfs)", color = "Water source", fill = "Water source", x = NULL) + 
    theme(axis.text.x = element_blank()) +
  coord_cartesian(clip = "off", xlim = c(as.POSIXct("2025-12-21"), as.POSIXct("2026-04-19")),
                  ylim = c(0, max(cdec_wide[cdec_wide$Site_no %in% c("CCY", "RCS", "PTC"), "discharge_cfs"], na.rm = T))))

final_plot <- flow_plot  /
  contpercflowplot /
  emmaplot +
  plot_layout(heights = c(1, 1, 5))

png(paste("Output/Figures/YBLTE_EMMA_%02d.png", sep = ""), 
    height = 10, width = 8, unit = "in", res = 1000)

final_plot

dev.off()
