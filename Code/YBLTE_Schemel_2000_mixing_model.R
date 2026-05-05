library(tidyverse)
library(lubridate)
library(ggplot2)

library(tidyverse)
library(lubridate)

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
  "DIN",
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
    title = "PCA of Schemel 2000 Chemistry (Reduced Analyte Set)",
    x = "PC1",
    y = "PC2"
  ) +
  theme_minimal() + scale_shape_manual(values = 1:12)
