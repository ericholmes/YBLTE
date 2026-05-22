# Summarizing fish data to help processing

# load libraries
library(tidyverse)
library(openxlsx)

# read in data for fish and sites
  # fish data was sorted in excel for direct and indirect take
sites <- readxl::read_excel("Data/tabular/YBLTE_sites.xlsx")
fish <- readxl::read_excel("Data/tabular/YBLTE_fish_specimens.xlsx")

# from site data filter for just the site ids and types
sites <- sites %>% subset(select=c("Site_id", "Sitetype"))

# change SB3 to SB4 in sites, just to get the site type
sites[sites$Site_id=="SB4","Site_id"] <- "SB3"

# merge sites to fish to get the site types per fish
fish <- left_join(fish, sites, by = join_by(StationCode == Site_id))

# get counts for record keeping - total and per habitat/date
total <- dim(fish)[1]
habitat_count <- fish %>% group_by(Sitetype) %>% summarize(count = n())
date_count <- fish %>% group_by(SampleDate) %>% summarize(count = n())

# save results back to new sheet in fish excel
updated_excel <- loadWorkbook("Data/tabular/YBLTE_fish_specimens.xlsx")

writeData(updated_excel, sheet = "Juv_CHN_take_2026", fish, colNames = T)

for(sheet in c("Total Fish", "Counts by Habitat", "Counts by Date")){
  if(!(sheet %in% names(updated_excel))){
    addWorksheet(updated_excel, sheetName = sheet)
  }
}

writeData(updated_excel, sheet = "Total Fish", total, colNames = F)
writeData(updated_excel, sheet = "Counts by Habitat", habitat_count, colNames = T)
writeData(updated_excel, sheet = "Counts by Date", date_count, colNames = T)

saveWorkbook(updated_excel,"Data/tabular/YBLTE_fish_specimens.xlsx", overwrite = T)
