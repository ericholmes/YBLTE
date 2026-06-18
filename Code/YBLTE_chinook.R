## Fish sampling update 2026
library(readxl)
library(tidyverse)

# # Folder containing your Excel files
# path <- "Data/tabular/YBFMP_take_tables"
# 
# # Get all .xlsx files
# files <- list.files(path, pattern = "\\.xlsx$", full.names = TRUE)
# dat1 <- read_excel(files[1]) %>% janitor::clean_names() %>% 
#   mutate(weight = as.numeric(weight))
# 
# dat2 <- read_excel(files[2]) %>% janitor::clean_names() %>% 
#   mutate(weight = as.numeric(weight))
# 
# chinook <- rbind(dat1, dat2) %>% filter(is.na(date) == F)

chinook <- readxl::read_excel("Data/tabular/YBLTE_fish_specimens.xlsx") %>% janitor::clean_names()

chinook$month <- as.integer(format(chinook$sample_date, format = "%m"))
chinook$day <- as.integer(format(chinook$sample_date, format = "%j"))

chinook$wyday <- chinook$sample_date - as.POSIXct("2025-10-01", tz = "UTC")

chinook$take <- ifelse(chinook$direct_take, "D",
                       ifelse(chinook$indirect_take, "I", F))

chinook$bridge <- ifelse(chinook$station_code %in% c("CNW", "KNG3"), 'T', 'F')

chinook <- chinook[is.na(chinook$fork_length) == F,]

# table -------------------------------------------------------------------

chinply <- chinook %>% group_by(sample_date, station_code, take) %>%  summarize(count = length(sample_date))
# write.csv(chinook, "Output/Chinook.csv", row.names = F)

chinsizeply <- chinook %>% group_by(sample_date, station_code) %>%  summarize(count = length(sample_date), meanfl = mean(fork_length), meanwt = mean(weight))
# plotting ----------------------------------------------------------------

# Water-year day sequence (adjust range as needed)
mydata <- data.frame(wyday = 60:200)

# LAD functions rewritten in terms of wyday
mydata$func1 <- sapply(mydata$wyday, function(x) exp(3.516464 + 0.006574 * (x + 26 - 91)))   # was x+26
mydata$func2 <- sapply(mydata$wyday, function(x) exp(3.516464 + 0.006574 * (x + 71 - 91)))   # was x+71
mydata$func3 <- sapply(mydata$wyday, function(x) exp(3.516464 + 0.006574 * (x + 160 - 91)))   # was x+160


set.seed(123)
jitter_width <- 0.3

# Jitter on water-year day, not calendar day
chinook$wydayjitter <- chinook$wyday + runif(nrow(chinook), -jitter_width, jitter_width)

png("Output/Figures/Chinook_take_wild_LAD_wy%03d.png",
    height = 5.5, width = 6.5, units = "in", res = 300)

ggplot(chinook, aes(x = wyday)) + theme_bw() +
  geom_ribbon(data = mydata, aes(ymin = func2, ymax = func1),
              fill = RColorBrewer::brewer.pal(8, "Dark2")[3], alpha = .2) +
  geom_ribbon(data = mydata, aes(ymin = min(chinook$fork_length), ymax = func1),
              fill = RColorBrewer::brewer.pal(8, "Dark2")[1], alpha = .2) +
  geom_ribbon(data = mydata, aes(ymin = func2, ymax = func3),
              fill = RColorBrewer::brewer.pal(8, "Dark2")[4], alpha = .2) +
  geom_ribbon(data = mydata, aes(ymin = func3, ymax = 200),
              fill = RColorBrewer::brewer.pal(8, "Dark2")[2], alpha = .2) +
  coord_cartesian(ylim = range(chinook$fork_length)) +
  # Water-year x-axis: adjust breaks/labels to your season
  scale_x_continuous(
    breaks = c(61, 92, 123, 151, 182),
    labels = c("Dec", "Jan", "Feb", "Mar", "Apr"),
    limits = c(61, max(chinook$wyday, na.rm = TRUE) + 15)
  ) +
  # LAD lines in wyday space
  stat_function(fun = function(x) exp(3.516464 + 0.006574 * (x + 26 - 91))) +  # func3
  stat_function(fun = function(x) exp(3.516464 + 0.006574 * (x + 71 - 91))) +  # func2
  stat_function(fun = function(x) exp(3.516464 + 0.006574 * (x + 160 - 91))) +  # func1
  annotate("text",
           x = c(81, 80, 76, 67),
           y = c(35, 39, 51, 85),
           label = c("Fall", "Spring", "Winter", "Late-fall"),
           color = RColorBrewer::brewer.pal(8, "Dark2")[c(1, 3, 4, 2)],
           angle = c(20, 29, 34, 46)) +
  theme(legend.position = "bottom", text = element_text(family = "serif")) +
  geom_point(data = chinook,#[chinook$take %in% c("D", "Y"),],
             aes(x = wydayjitter, y = fork_length, color = take, 
                 shape = bridge), size = 2, stroke = 2) +
  # geom_point(data = chinook[chinook$take %in% "I",],
  #            aes(x = wydayjitter, y = fork_length, color = take), size = 3) +
  geom_point(aes(x = wydayjitter, y = fork_length)) +
  scale_color_manual(values = c('D' = "orange3", 'I' = "red"), 
                     labels = c("Direct", "Indirect"), name = "Take") +
  scale_shape_manual(values = c('F' = 19, 'T' = 4), 
                     labels = c("Non-bridge", "Bridge Group"), name = NULL) +
  labs(x = NULL, y = "Fork Length (mm)", title = "Fisher LAD class for YBLTE Fish")

dev.off()
