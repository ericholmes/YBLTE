## NDFS 2024 
## MiniDOT data processing script
## Eric Holmes 2024-08-21

## Load Libraries ----------------------------------------------------------

library(tidyverse)
library(ggridges)

## Load data ---------------------------------------------------------------

saveOutput = F

##list folders corresponding to DOBO serial numbers
folders <- list.files("Data/tabular/MiniDOTs")

##load miniDOT serial lookup table
dobolookup <- read.csv("Data/tabular/YBLTE_miniDOTlookup.csv")

## Create empty data frame to compile all dobo data
dobodat <- data.frame()

### Loop through all DOBO folders ----
for(folder in folders){
  print(folder)
  ## List daily files from each folder associated with a specific serial number
  files <- list.files(paste0("Data/tabular/MiniDOTs/", folder), full.names = T)
  ##Loop through individual files within folders
  for(file in files){
    ## Pull DOBO serial number from first row of each file
    serial <- read.table(file, nrows = 1, sep = ",")
    ## Load daily data file (comma separated text file)
    tempdat <- read.table(file, skip = 2, header = T, sep = ",", 
                          col.names = c("time_sec", "bat_volts", "temp_c", "do_mgl", "q"))
    ##Create column with the station name
    tempdat$station <- dobolookup[dobolookup$serial == serial$V1, "station"]
    ## Convert time in seconds to POSIX date time format
    tempdat$datetime <- as.POSIXct(tempdat$time_sec, origin = "1970-01-01", tz = "")
    attr(tempdat$datetime, "tzone") <- "America/Los_Angeles"
    ##Combine individual data files
    dobodat <- rbind(dobodat, tempdat[, c("station", "datetime", "temp_c", "do_mgl")])
    ##Clean up workspace after each file
    rm(serial, tempdat)
  }
}

## Clean DOBO data ---------------------------------------------------------

dobodat %>% group_by(station) %>% summarize(mindate = min(datetime))

dobodat <- dobodat[dobodat$datetime > as.POSIXct("2025-09-26 13:00:00"),]

# dobodat <- dobodat[!(dobodat$station == "BL5" & dobodat$datetime < as.POSIXct("2024-07-3 13:00:00")),]

## Plot DOBO data ----------------------------------------------------------
dobodat$date <- as.Date(dobodat$datetime)

dobodaily <- dobodat %>% filter(temp_c < 30) %>% group_by(station, date) %>% 
  summarize(mintemp = min(temp_c), meantemp = mean(temp_c), maxtemp = max(temp_c),
            mindo = min(do_mgl), meando = mean(do_mgl), maxdo = max(do_mgl)) %>% data.frame()

dbmelt <- reshape2::melt(dobodaily, id.vars = c("station", "date"))

### Temperature ----
ggplot(dobodat, aes(x = datetime, y = temp_c)) + geom_line(aes(color = station)) + theme_bw()

ggplot(dobodat, aes(x = datetime, y = temp_c)) + geom_line() + theme_bw() + 
  facet_wrap(station~.)

ggplot(dobodaily, aes(x = maxtemp, fill = station)) + geom_density(alpha = .2) + 
  facet_wrap(station ~ .) + theme_bw()

dbmelt$stationfac <- factor(dbmelt$station, 
                            levels = c("RVB", "DWSN",  "DWSS", "LIB", "BL5", "SHAS", "SHAN", "SSE"))

ggplot(dbmelt[dbmelt$variable %in% c("mintemp", "meantemp", "maxtemp"),], 
       aes(x = value, y = stationfac, fill = stat(x))) + geom_density_ridges_gradient(show.legend = F) + 
  scale_fill_viridis_c(option = "B", direction = -1) +
  facet_grid(. ~ variable, scales = "free") + theme_bw()

ggplot(dbmelt[dbmelt$variable %in% c("mindo", "meando", "maxdo"),], 
       aes(x = value, y = stationfac, fill = stat(x))) + geom_density_ridges_gradient(show.legend = F) + 
  scale_fill_viridis_c(option = "B", direction = 1) +
  facet_grid(. ~ variable, scales = "fixed") + theme_bw()

#### temp range ----

### DO ----

ggplot(dobodat, aes(x = datetime, y = do_mgl)) + geom_line(aes(color = station)) + theme_bw()

ggplot(dobodat, aes(x = datetime, y = do_mgl)) + geom_line() + theme_bw() + 
  facet_wrap(station~.)

# Continuous ribbon smoothing ---------------------------------------------

dobodaily$datetime <- as.POSIXct(dobodaily$date)

# Create empty caegories to be filled with smoothed daily
dobodat$mindomod <- NA; dobodat$maxdomod <- NA; dobodat$meandomod <- NA
dobodat$mintempmod <- NA; dobodat$maxtempmod <- NA; dobodat$meantempmod <- NA
spanwidth = .2
# Calculate LOESS smoothed min, max and mean from daily dobodata for each site and year combo
for(i in unique(dobodaily$station)){
  
  print(i)
  
  mindomod <- loess(mindo ~ as.numeric(datetime), dobodaily[dobodaily$station %in% i, ], span = spanwidth)
  maxdomod <- loess(maxdo ~ as.numeric(datetime), dobodaily[dobodaily$station %in% i, ], span = spanwidth)
  meandomod <- loess(meando ~ as.numeric(datetime), dobodaily[dobodaily$station %in% i, ], span = spanwidth)
  
  dobodat$mindomod <- ifelse(dobodat$station == i, predict(mindomod, dobodat$datetime), dobodat$mindomod)
  dobodat$maxdomod <- ifelse(dobodat$station == i, predict(maxdomod, dobodat$datetime), dobodat$maxdomod)
  dobodat$meandomod <- ifelse(dobodat$station == i, predict(meandomod, dobodat$datetime), dobodat$meandomod)
  
  mintempmod <- loess(mintemp ~ as.numeric(datetime), dobodaily[dobodaily$station %in% i, ], span = spanwidth)
  maxtempmod <- loess(maxtemp ~ as.numeric(datetime), dobodaily[dobodaily$station %in% i, ], span = spanwidth)
  meantempmod <- loess(meantemp ~ as.numeric(datetime), dobodaily[dobodaily$station %in% i, ], span = spanwidth)
  
  dobodat$mintempmod <- ifelse(dobodat$station == i, predict(mintempmod, dobodat$datetime), dobodat$mintempmod)
  dobodat$maxtempmod <- ifelse(dobodat$station == i, predict(maxtempmod, dobodat$datetime), dobodat$maxtempmod)
  dobodat$meantempmod <- ifelse(dobodat$station == i, predict(meantempmod, dobodat$datetime), dobodat$meantempmod)
  
  rm(mindomod, maxdomod, meandomod,
     mintempmod, maxtempmod, meantempmod)
}

if(saveOutput == T){png(paste("output/LEBLS_mindots_%03d.png", sep = ""), 
                        height = 5, width = 8, unit = "in", res = 1000)}

# Continuous temp and DO plotting
(domglpan <- ggplot(dobodat, aes(x = datetime, y = meandomod, fill = station, group = station)) +
    # scale_fill_manual(values =c("TOE" = "#FFAA00", "LIB" = "#33A02C")) +
    # scale_color_manual(values =c("TOE" = "#FFAA00", "LIB" = "#33A02C")) +
    geom_ribbon(aes(ymin = mindomod, ymax = maxdomod), alpha = .2,linetype = 0) + 
    geom_line(data = dobodat, aes(x = datetime, y = do_mgl, color = station), alpha = .4) + 
    geom_line(color = "black", linetype = 2) +
    theme_bw() +
    labs(x = NULL, y = "Dissolved oxygen (mg/L)") + 
    facet_wrap(station ~ ., scales = "fixed") + theme(legend.position = "none"))

(tempcpan <- ggplot(dobodat, aes(x = datetime, y = meantempmod, fill = station, group = station)) +
    # scale_fill_manual(values =c("TOE" = "#FFAA00", "LIB" = "#33A02C")) +
    # scale_color_manual(values =c("TOE" = "#FFAA00", "LIB" = "#33A02C")) +
    geom_ribbon(aes(ymin = mintempmod, ymax = maxtempmod), alpha = .2,linetype = 0) + 
    geom_line(data = dobodat, aes(x = datetime, y = temp_c, color = station), alpha = .4) + 
    geom_line(color = "black", linetype = 2) +
    theme_bw() +
    labs(x = NULL, y = "Temperature (C)") + 
    facet_wrap(station ~ ., scales = "fixed") + theme(legend.position = "none"))

if(saveOutput == T){dev.off()}
# Calculate metrics centered around sampling events -----------------------
# save(dobodat, file = "Data/NDFS2024_DOBOdat.Rdata")
# unique(ndmerge$wdl_sam_collection_date)
# dobodat$date
# sitedates <- ndmerge %>% group_by(wdl_sam_collection_date, station_name) %>% summarize() %>% data.frame()
# 
# mets <- data.frame()
# for(site in sitedates$station_name){
#   dates <- as.Date(unique(ndmerge[ndmerge$station_name == site, "wdl_sam_collection_date"]))
#   print(site)
#   print(dates)
#   for(date in dates){
#     dobotemp <- dobodat[dobodat$station == site & dobodat$date > date - 7 & dobodat$date < date + 7 ,]
#     mets <- rbind(mets, data.frame(station_name = site, wdl_sam_collection_date = as.Date(date), 
#                mediantemp = median(dobotemp$temp_c, na.rm = T),
#                mediando = median(dobotemp$do_mgl, na.rm = T), maxdo = max(dobotemp$do_mgl, na.rm = T),
#                N = length(dobotemp$temp_c)))
#   }
# }


