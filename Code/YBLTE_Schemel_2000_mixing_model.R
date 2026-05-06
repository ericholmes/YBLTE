library(tidyverse)
library(lubridate)
library(ggplot2)

#----------------------------------------------------------
# Load all tables
#----------------------------------------------------------
t2 <- read_csv("Data/tabular/Schemel_2000_data/Schemel_2000_Table2.csv")
t3 <- read_csv("Data/tabular/Schemel_2000_data/Schemel_2000_Table3.csv")
t4 <- read_csv("Data/tabular/Schemel_2000_data/Schemel_2000_Table4.csv")
t5 <- read_csv("Data/tabular/Schemel_2000_data/Schemel_2000_Table5.csv")

#----------------------------------------------------------
# Standardize key columns
#----------------------------------------------------------
#----------------------------------------------------------
# Fix site name inconsistencies BEFORE joining
#----------------------------------------------------------

fix_site_names <- function(df, site_col) {
  site_col <- rlang::ensym(site_col)
  
  df %>%
    mutate(
      {{site_col}} := case_when(
        {{site_col}} %in% c("LH-1", "LH1") ~ "LH1",
        {{site_col}} %in% c("PS-1", "PS1") ~ "PS1",
        {{site_col}} %in% c("PS-2", "PS2") ~ "PS2",
        {{site_col}} %in% c("PS-3", "PS3") ~ "PS3",
        {{site_col}} %in% c("BAND A", "BANDA") ~ "BANDA",
        {{site_col}} %in% c("BAND B", "BANDB") ~ "BANDB",
        {{site_col}} %in% c("BAND C", "BANDC") ~ "BANDC",
        {{site_col}} %in% c("BAND D", "BANDD") ~ "BANDD",
        {{site_col}} %in% c("RCE-16", "RCE16") ~ "RCE16",
        TRUE ~ as.character({{site_col}})
      )
    )
}

# Fix names first
# Fix names first
t2 <- fix_site_names(t2, Sample_ID)
t3 <- fix_site_names(t3, Station_ID)
t4 <- fix_site_names(t4, Site)
t5 <- fix_site_names(t5, Site)


t2_clean <- t2 %>% rename(Site = Sample_ID) %>% mutate(Date = ymd(Date))
t3_clean <- t3 %>% rename(Site = Station_ID) %>% mutate(Date = ymd(Date))
t4_clean <- t4 %>% mutate(Date = ymd(Date))
t5_clean <- t5 %>% mutate(Date = ymd(Date))

chem_master <- t2_clean %>%
  full_join(t3_clean, by = c("Block_Code", "Block_Name", "Band", "Site", "Date")) %>%
  full_join(t4_clean, by = c("Block_Code", "Block_Name", "Band", "Site", "Date")) %>%
  full_join(t5_clean, by = c("Block_Code", "Block_Name", "Band", "Site", "Date"))

library(tidyverse)
library(lubridate)
library(ggplot2)

#----------------------------------------------------------
# START WITH chem_master (already created earlier)
#----------------------------------------------------------

df <- chem_master %>% mutate(Date = ymd(Date))

#----------------------------------------------------------
# 1. Identify numeric chemistry columns
#----------------------------------------------------------

numeric_cols <- df %>%
  select(where(is.numeric)) %>%
  names()

#----------------------------------------------------------
# 2. Compute missingness per analyte
#----------------------------------------------------------

analyte_missing <- df %>%
  summarise(across(all_of(numeric_cols), ~mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "analyte", values_to = "missing_fraction")

#----------------------------------------------------------
# 3. Choose analytes with < 40% missingness
#    (empirically best for Schemel 2000)
#----------------------------------------------------------

good_analytes <- analyte_missing %>%
  filter(missing_fraction < 0.40) %>%
  pull(analyte)

cat("Selected analytes for PCA:\n")
print(good_analytes)

#----------------------------------------------------------
# 4. Count non-missing analytes per row
#----------------------------------------------------------

df_row_counts <- df %>%
  mutate(non_missing = rowSums(!is.na(select(., all_of(good_analytes)))))

#----------------------------------------------------------
# 5. Automatically choose optimal threshold
#    Strategy: maximize (# rows retained * # analytes retained)
#----------------------------------------------------------

thresholds <- tibble(
  threshold = 1:length(good_analytes),
  rows_retained = map_int(threshold, ~sum(df_row_counts$non_missing >= .x)),
  score = threshold * rows_retained
)

best_threshold <- thresholds %>%
  filter(score == max(score)) %>%
  pull(threshold)

cat("\nOptimal threshold =", best_threshold, "non-missing analytes per row\n")

#----------------------------------------------------------
# 6. Build PCA-ready dataset
#----------------------------------------------------------

df_pca_ready <- df_row_counts %>%
  filter(non_missing >= best_threshold) %>%
  select(Site, Date, all_of(good_analytes)) %>%
  drop_na()

cat("\nRows in PCA dataset:", nrow(df_pca_ready), "\n")

#----------------------------------------------------------
# 7. Run PCA
#----------------------------------------------------------

pca <- prcomp(df_pca_ready %>% select(-Site, -Date), scale. = TRUE)

scores <- as_tibble(pca$x) %>%
  bind_cols(Site = df_pca_ready$Site,
            Date = df_pca_ready$Date)

loadings <- as_tibble(pca$rotation, rownames = "Variable")

# Scale loadings for plotting
loading_scale <- 3
loadings <- loadings %>%
  mutate(
    PC1 = PC1 * loading_scale,
    PC2 = PC2 * loading_scale
  )

#----------------------------------------------------------
# 8. Compute convex hulls
#----------------------------------------------------------

hulls <- scores %>%
  group_by(Site) %>%
  slice(chull(PC1, PC2))

#----------------------------------------------------------
# 9. PCA plot with convex hulls + loading vectors
#----------------------------------------------------------

ggplot() +
  geom_polygon(
    data = hulls,
    aes(x = PC1, y = PC2, fill = Site),
    alpha = 0.2, color = NA
  ) +
  geom_point(
    data = scores[scores$Site %in% c("BANDA", "BANDB", "BANDC", "BANDD"),],
    aes(x = PC1, y = PC2), color = "black",
    size = 3
  ) +
  geom_point(
    data = scores[scores$Site %in% c("FRW", "CC113", "WS102", "PCD"),],
    aes(x = PC1, y = PC2), color = "black",
    size = 4
  ) +
  geom_point(
    data = scores,
    aes(x = PC1, y = PC2, color = Site),
    size = 2
  ) +
  geom_segment(
    data = loadings,
    aes(x = 0, y = 0, xend = PC1, yend = PC2),
    arrow = arrow(length = unit(0.25, "cm")),
    color = "black"
  ) +
  geom_text(
    data = loadings,
    aes(x = PC1, y = PC2, label = Variable),
    vjust = -0.5,
    size = 4
  ) +
  labs(
    title = "PCA of Schemel 2000 Chemistry (Optimized for Completeness)",
    x = "PC1",
    y = "PC2"
  ) +
  theme_minimal()


# Extract only numeric analytes used in PCA
corr_data <- df_pca_ready %>%
  select(all_of(good_analytes))

# Compute correlation matrix
corr_matrix <- cor(corr_data, use = "pairwise.complete.obs")

# Melt for ggplot
corr_melt <- reshape2::melt(corr_matrix, varnames = c("Var1", "Var2"), value.name = "Correlation")

#----------------------------------------------------------
# Correlation heatmap
#----------------------------------------------------------

ggplot(corr_melt, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#2166ac",
    mid = "white",
    high = "#b2182b",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlation"
  ) +
  labs(
    title = "Correlation Heatmap of Optimized Chemistry Analytes",
    x = "Analyte",
    y = "Analyte"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

library(tidyverse)
library(ggplot2)
library(lubridate)

#----------------------------------------------------------
# Use chem_master (already created earlier)
#----------------------------------------------------------

df <- chem_master %>% mutate(Date = ymd(Date))

#----------------------------------------------------------
# Define representative analytes
#----------------------------------------------------------

rep_analytes <- c(
  "Specific_Conductance",
  "DOC",
  "SPM",
  # "DIN",
  "DiSi",
  "Ca"
)

#----------------------------------------------------------
# Filter to rows with complete data for these analytes
#----------------------------------------------------------

df_reduced <- df %>%
  select(Site, Date, all_of(rep_analytes)) %>%
  drop_na()

cat("Rows retained for PCA:", nrow(df_reduced), "\n")

#----------------------------------------------------------
# Run PCA
#----------------------------------------------------------

pca <- prcomp(df_reduced %>% select(-Site, -Date), scale. = TRUE)

scores <- as_tibble(pca$x) %>%
  bind_cols(Site = df_reduced$Site)

loadings <- as_tibble(pca$rotation, rownames = "Variable")

# Scale loadings for plotting
loading_scale <- 3
loadings <- loadings %>%
  mutate(
    PC1 = PC1 * loading_scale,
    PC2 = PC2 * loading_scale
  )

#----------------------------------------------------------
# Compute convex hulls for each site
#----------------------------------------------------------

hulls <- scores %>%
  group_by(Site) %>%
  slice(chull(PC1, PC2))

hulls_mo <- scores %>%
  group_by(Site) %>%
  slice(chull(PC1, PC2))

#----------------------------------------------------------
# PCA biplot with convex hulls + loading vectors
#----------------------------------------------------------

ggplot() +
  geom_polygon(
    data = hulls[hulls$Site %in% c("CC113", "PCD", "WS102", "CBD99E"),],
    aes(x = PC1, y = PC2, fill = Site),
    alpha = 0.2, color = NA
  ) +
  geom_point(
    data = scores,
    aes(x = PC1, y = PC2, color = Site, shape = Site),
    size = 2
  ) +
  geom_segment(
    data = loadings,
    aes(x = 0, y = 0, xend = PC1, yend = PC2),
    arrow = arrow(length = unit(0.25, "cm")),
    color = "black"
  ) +
  geom_text(
    data = loadings,
    aes(x = PC1, y = PC2, label = Variable),
    vjust = -0.5,
    size = 4
  ) +
  labs(
    title = "PCA of Schemel 2000 Chemistry",
    x = "PC1",
    y = "PC2"
  ) +
  theme_minimal() + scale_shape_manual(values = 1:12)



# EMMA model --------------------------------------------------------------

# -------------------------------------------------
# 1. Basic setup
# -------------------------------------------------

df <- chem_master %>%
  mutate(Date = ymd(Date))

# Static endmembers
endmember_sites <- c(
  "CBD99E",   # Colusa Drain / Ridgecut
  "CC113",    # Cache Creek
  "FRW",      # Fremont Weir
  "PCD"       # Putah Creek
)
unique(chem_master$Site)
# Yolo Bypass interior sites (EDIT THESE to your actual toe drain / tule canal IDs)
yolo_sites <- c("STTD",      "TD80" ,     "TD5")

# Tracers to use
tracers <- c("Specific_Conductance", "Ca", "DiSi", "DOC", "SPM")

df_clean <- df %>%
  select(Site, Date, all_of(tracers)) %>%
  drop_na()

# -------------------------------------------------
# 2. Static endmember chemistry (mean over all dates)
# -------------------------------------------------

endm_static <- df_clean %>%
  filter(Site %in% endmember_sites) %>%
  group_by(Site) %>%
  summarise(across(all_of(tracers), mean, na.rm = TRUE), .groups = "drop")

# -------------------------------------------------
# 3. Yolo samples
# -------------------------------------------------

mix <- df_clean %>%
  filter(Site %in% yolo_sites)

# -------------------------------------------------
# 4. PCA on all samples (endmembers + Yolo)
# -------------------------------------------------

df_pca <- bind_rows(
  endm_static %>% mutate(type = "endmember"),
  mix          %>% mutate(type = "mix")
)

pca <- prcomp(df_pca %>% select(all_of(tracers)), scale. = TRUE)

scores <- as_tibble(pca$x) %>%
  bind_cols(df_pca %>% select(Site, Date, type))

# -------------------------------------------------
# 5. Extract static PC scores for each endmember
# -------------------------------------------------

endm_pc <- scores %>%
  filter(type == "endmember") %>%
  group_by(Site) %>%
  summarise(
    PC1 = mean(PC1, na.rm = TRUE),
    PC2 = mean(PC2, na.rm = TRUE),
    PC3 = mean(PC3, na.rm = TRUE),
    .groups = "drop"
  )

FRW_pc <- endm_pc %>% filter(Site == "FRW")   %>% select(PC1, PC2, PC3) %>% as.numeric()
CC_pc  <- endm_pc %>% filter(Site == "CC113") %>% select(PC1, PC2, PC3) %>% as.numeric()
CBD_pc <- endm_pc %>% filter(Site == "CBD99E")%>% select(PC1, PC2, PC3) %>% as.numeric()
PCD_pc <- endm_pc %>% filter(Site == "PCD")   %>% select(PC1, PC2, PC3) %>% as.numeric()

# -------------------------------------------------
# 6. Mixing space for Yolo samples (PC1–PC3)
# -------------------------------------------------

mix_pc <- scores %>%
  filter(type == "mix") %>%
  select(Site, Date, PC1, PC2, PC3)

# -------------------------------------------------
# 7. 4‑source mixing solver (static endmembers in PC space)
# -------------------------------------------------

solve_4source <- function(mix, e1, e2, e3, e4) {
  A <- matrix(c(
    e1[1], e2[1], e3[1], e4[1],
    e1[2], e2[2], e3[2], e4[2],
    e1[3], e2[3], e3[3], e4[3],
    1,     1,     1,     1
  ), 4, 4, byrow = TRUE)
  
  b <- c(mix[1], mix[2], mix[3], 1)
  
  out <- tryCatch(solve(A, b), error = function(e) rep(NA, 4))
  as.numeric(out)
}

mix_fractions <- mix_pc %>%
  rowwise() %>%
  mutate(
    frac = list(
      solve_4source(
        c(PC1, PC2, PC3),
        FRW_pc,
        CC_pc,
        CBD_pc,
        PCD_pc
      )
    ),
    f_FRW = frac[1],
    f_CC  = frac[2],
    f_CBD = frac[3],
    f_PCD = frac[4]
  ) %>%
  ungroup() %>%
  mutate(
    across(c(f_FRW, f_CC, f_CBD, f_PCD), ~pmax(pmin(.x, 1), 0)),
    sum_clamp = f_FRW + f_CC + f_CBD + f_PCD,
    f_FRW_plot = f_FRW / sum_clamp,
    f_CC_plot  = f_CC  / sum_clamp,
    f_CBD_plot = f_CBD / sum_clamp,
    f_PCD_plot = f_PCD / sum_clamp,
    unexplained = 1 - (f_FRW + f_CC + f_CBD + f_PCD)
  )

# -------------------------------------------------
# 8. Stacked bar plot of static‑endmember fractions
# -------------------------------------------------

mix_long <- mix_fractions %>%
  select(Date, Site, f_FRW_plot, f_CC_plot, f_CBD_plot, f_PCD_plot) %>%
  pivot_longer(
    cols = starts_with("f_"),
    names_to = "source",
    values_to = "fraction"
  ) %>%
  mutate(
    source = recode(source,
                    f_FRW_plot = "Fremont Weir",
                    f_CC_plot  = "Cache Creek",
                    f_CBD_plot = "Colusa Drain",
                    f_PCD_plot = "Putah Creek"
    )
  )
mix_long$Sitefac <- factor(mix_long$Site, levels = c("TD5", "TD80", "STTD"))

ggplot(mix_long, aes(Date, fraction, fill = source)) +
  geom_col(width = 6) +
  facet_grid(Sitefac ~ .) +
  scale_fill_manual(values = c(
    "Fremont Weir" = "skyblue",
    "Cache Creek"  = "forestgreen",
    "Colusa Drain" = "goldenrod",
    "Putah Creek"  = "skyblue3"
  )) +
  labs(
    title = "Schemel 2000 EMMA",
    x = "Date",
    y = "Fraction"
  ) +
  theme_bw()

mix_long %>%
  arrange(Date) %>%   # important for area plots
  ggplot(aes(Date, fraction, fill = source)) +
  geom_area(alpha = 0.85, color = NA) +
  facet_grid(Sitefac ~ .) +
  scale_fill_manual(values = c(
    "Fremont Weir" = "skyblue",
    "Cache Creek"  = "forestgreen",
    "Colusa Drain" = "goldenrod",
    "Putah Creek"  = "skyblue3"
  )) +
  labs(
    title = "Schemel 2000 EMMA – Stacked Area Representation",
    x = "Date",
    y = "Fraction"
  ) +
  theme_bw()
