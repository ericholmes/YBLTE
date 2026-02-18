## YBLTE 2025-26
## Water quality logger data processing script
## Eric Holmes - Kristina Nguyen - Last Edited 2/10/26

# editing notes
  # no sight of flow data for TER and I80 (exists or hidden?)
    # flow data --> discharge and stage lvl (cdec I80(no discharge sensor), none for TER (use TOE or LIS?))

## Load Libraries ----------------------------------------------------------

library(tidyverse)
library(ggridges)

## Load data ---------------------------------------------------------------
saveOutput = F

## List folders corresponding to DOBO serial numbers
folders <- list.files("Data/tabular/MiniDOTs")

## Load miniDOT serial lookup table
dobolookup <- read.csv("Data/tabular/YBLTE_miniDOTlookup.csv")
  # miniDOT lookup has the station, serial, and start/end (not used, easier to change in code)

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
    ##Create column with the station name (gets station from serial with lookup)
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

## Create empty data frame to compile all conductivity data, different because time not guaranteed to line up
conddat <- data.frame()

## Load EC data
ec <- list.files("Data/tabular/EC_loggers", full.names = T)
ec <- as.list(ec[grepl(".csv", ec)])
for(f in ec){
  
  cond_temp <- read.csv(f, skip= 1, header = T)
  cond_temp <- cond_temp[,2:4]
  colnames(cond_temp) <- c("datetime", "raw_cond", "temp")
  
  # get station from file name
  cond_temp$station <- str_sub(f, -7, -5)
  
  # convert time type
  cond_temp$datetime <- as.POSIXct(cond_temp$datetime, tz="America/Los_Angeles", 
                                   format="%m/%d/%y %I:%M:%S %p")
  # cond_temp$datetime <- format(cond_temp$datetime, "%Y-%m-%d %H:%M:%S")
  
  # convert temperature from F to C
  cond_temp$temp <- (cond_temp$temp-32)*5/9
  
  # calculate SpC from raw conductivity and temperature
  cond_temp$spc <- cond_temp$raw_cond/(1+0.02*(cond_temp$temp-25))
  
  conddat <- rbind(conddat, cond_temp[,c("station","datetime","spc")])
  }

## Clean DOBO data ---------------------------------------------------------
# Filter to current water year 
dobodat <- dobodat[dobodat$datetime > as.POSIXct("2025-10-01"),]
conddat <- conddat[conddat$datetime > as.POSIXct("2025-10-01"),]

# Drop NA
dobodat <- drop_na(dobodat)
conddat <- drop_na(conddat)

# Drop bad conductivity values (guessing under 150); edit values further later (better and for other params)
conddat <- filter(conddat, spc>150)

# Get earliest dates for each station
# dobodat %>% group_by(station)%>% summarize(mindate = min(datetime)))

# Filter dates, imported from other script so not used
# dobodat <- dobodat[dobodat$datetime > as.POSIXct("2025-09-26 13:00:00"),]
# dobodat <- dobodat[!(dobodat$station == "BL5" & dobodat$datetime < as.POSIXct("2024-07-3 13:00:00")),]

# Convert date type
dobodat$date <- as.Date(dobodat$datetime)
conddat$date <- as.Date(conddat$datetime)

# Get daily min, max, mean per variable
dobodaily <- dobodat %>% filter(temp_c < 30) %>% group_by(station, date) %>% 
  summarize(mintemp = min(temp_c), meantemp = mean(temp_c), maxtemp = max(temp_c),
            mindo = min(do_mgl), meando = mean(do_mgl), maxdo = max(do_mgl)) %>% data.frame()
conddaily <- conddat %>% 
  group_by(station, date) %>% 
  summarize(minc = min(spc), meanc = mean(spc), maxc = max(spc)) %>% data.frame()

# Reshape data frame for daily points
dbmelt <- reshape2::melt(dobodaily, id.vars = c("station", "date"))
cdmelt <- reshape2::melt(conddaily, id.vars = c("station", "date"))

# Station factors for plotting
dbmelt$stationfac <- factor(dbmelt$station, 
                            levels = c("TER", "SB4", "I80"))
cdmelt$stationfac <- factor(cdmelt$station,
                            levels = c("TEW", "TER", "SB4", "I80"))

## Plot DOBO+cond data ----------------------------------------------------------

### Temperature ----
ggplot(dobodat, aes(x = datetime, y = temp_c)) + geom_line(aes(color = station)) + theme_bw()

ggplot(dobodat, aes(x = datetime, y = temp_c)) + geom_line() + theme_bw() + 
  facet_wrap(station~.)

ggplot(dobodaily, aes(x = maxtemp, fill = station)) + geom_density(alpha = .2) + 
  facet_wrap(station ~ .) + theme_bw()

ggplot(dbmelt[dbmelt$variable %in% c("mintemp", "meantemp", "maxtemp"),], 
       aes(x = value, y = stationfac, fill = stat(x))) + geom_density_ridges_gradient(show.legend = F) + 
  scale_fill_viridis_c(option = "B", direction = -1) +
  facet_grid(. ~ variable, scales = "free") + theme_bw()

### DO ----

ggplot(dobodat, aes(x = datetime, y = do_mgl)) + geom_line(aes(color = station)) + theme_bw()

ggplot(dobodat, aes(x = datetime, y = do_mgl)) + geom_line() + theme_bw() + 
  facet_wrap(station~.)

ggplot(dobodaily, aes(x = maxdo, fill = station)) + geom_density(alpha = .2) + 
  facet_wrap(station ~ .) + theme_bw()

ggplot(dbmelt[dbmelt$variable %in% c("mindo", "meando", "maxdo"),], 
       aes(x = value, y = stationfac, fill = stat(x))) + geom_density_ridges_gradient(show.legend = F) + 
  scale_fill_viridis_c(option = "B", direction = 1) +
  facet_grid(. ~ variable, scales = "fixed") + theme_bw()

### Conductivity ----

ggplot(conddat, aes(x = datetime, y = spc, group = station)) + geom_line(aes(color = station)) + theme_bw()

ggplot(conddat, aes(x = datetime, y = spc, group = station)) + geom_line() + theme_bw() +
  facet_wrap(station~.)

ggplot(conddaily, aes(x = maxc, fill = station)) + geom_density(alpha = .2) + 
  facet_wrap(station ~ .) + theme_bw()

ggplot(cdmelt[cdmelt$variable %in% c("minc", "meanc", "maxc"),],
       aes(x = value, y = stationfac, fill = stat(x))) + geom_density_ridges_gradient(show.legend = F) +
  scale_fill_viridis_c(option = "B", direction = 1) +
  facet_grid(. ~ variable, scales = "fixed") + theme_bw()

# Continuous ribbon smoothing ---------------------------------------------

# Convert date type
dobodaily$datetime <- as.POSIXct(dobodaily$date)
conddaily$datetime <- as.POSIXct(conddaily$date)

# Create empty categories to be filled with smoothed daily
dobodat$mintempmod <- NA; dobodat$maxtempmod <- NA; dobodat$meantempmod <- NA
conddat$mincmod <- NA; conddat$maxcmod <- NA; conddat$meancmod <- NA
dobodat$mindomod <- NA; dobodat$maxdomod <- NA; dobodat$meandomod <- NA
spanwidth = .2
# Calculate LOESS smoothed min, max and mean from daily dobodata for each site and year combo
for(i in unique(dobodaily$station)){
  
  print(i)
  # Temperature
  
  mintempmod <- loess(mintemp ~ as.numeric(datetime), dobodaily[dobodaily$station %in% i, ], span = spanwidth)
  maxtempmod <- loess(maxtemp ~ as.numeric(datetime), dobodaily[dobodaily$station %in% i, ], span = spanwidth)
  meantempmod <- loess(meantemp ~ as.numeric(datetime), dobodaily[dobodaily$station %in% i, ], span = spanwidth)
  
  dobodat$mintempmod <- ifelse(dobodat$station == i, predict(mintempmod, dobodat$datetime), dobodat$mintempmod)
  dobodat$maxtempmod <- ifelse(dobodat$station == i, predict(maxtempmod, dobodat$datetime), dobodat$maxtempmod)
  dobodat$meantempmod <- ifelse(dobodat$station == i, predict(meantempmod, dobodat$datetime), dobodat$meantempmod)
  
  # DO
  
  mindomod <- loess(mindo ~ as.numeric(datetime), dobodaily[dobodaily$station %in% i, ], span = spanwidth)
  maxdomod <- loess(maxdo ~ as.numeric(datetime), dobodaily[dobodaily$station %in% i, ], span = spanwidth)
  meandomod <- loess(meando ~ as.numeric(datetime), dobodaily[dobodaily$station %in% i, ], span = spanwidth)
  
  dobodat$mindomod <- ifelse(dobodat$station == i, predict(mindomod, dobodat$datetime), dobodat$mindomod)
  dobodat$maxdomod <- ifelse(dobodat$station == i, predict(maxdomod, dobodat$datetime), dobodat$maxdomod)
  dobodat$meandomod <- ifelse(dobodat$station == i, predict(meandomod, dobodat$datetime), dobodat$meandomod)
  
  # Clear workspace
  
  rm(mintempmod, maxtempmod, meantempmod,
     mindomod, maxdomod, meandomod)
}

for(i in unique(conddaily$station)){
  
  print(i)
  
  # Conductivity
  
  mincmod <- loess(minc ~ as.numeric(datetime), conddaily[conddaily$station %in% i, ], span = spanwidth)
  maxcmod <- loess(maxc ~ as.numeric(datetime), conddaily[conddaily$station %in% i, ], span = spanwidth)
  meancmod <- loess(meanc ~ as.numeric(datetime), conddaily[conddaily$station %in% i, ], span = spanwidth)

  conddat$mincmod <- ifelse(conddat$station == i, predict(mincmod, conddat$datetime), conddat$mincmod)
  conddat$maxcmod <- ifelse(conddat$station == i, predict(maxcmod, conddat$datetime), conddat$maxcmod)
  conddat$meancmod <- ifelse(conddat$station == i, predict(meancmod, conddat$datetime), conddat$meancmod)
  
  # Clear workspace
  
  rm(mincmod, maxcmod, meancmod)
}

# Continuous temp, conductivity, and DO plotting

(tempcpan <- ggplot(dobodat, aes(x = datetime, y = meantempmod, fill = station, group = station)) +
    # scale_fill_manual(values =c("TOE" = "#FFAA00", "LIB" = "#33A02C")) +
    # scale_color_manual(values =c("TOE" = "#FFAA00", "LIB" = "#33A02C")) +
    geom_ribbon(aes(ymin = mintempmod, ymax = maxtempmod), alpha = .2, linetype = 0) + 
    geom_line(data = dobodat, aes(x = datetime, y = temp_c, color = station), alpha = .4) + 
    geom_line(color = "black", linetype = 2) +
    theme_bw() +
    labs(x = NULL, y = "Temperature (C)")
    # +facet_wrap(station ~ ., scales = "fixed") + theme(legend.position = "none")
  )

(domglpan <- ggplot(dobodat, aes(x = datetime, y = meandomod, fill = station, group = station)) +
    # scale_fill_manual(values =c("TOE" = "#FFAA00", "LIB" = "#33A02C")) +
    # scale_color_manual(values =c("TOE" = "#FFAA00", "LIB" = "#33A02C")) +
    geom_ribbon(aes(ymin = mindomod, ymax = maxdomod), alpha = .2, linetype = 0) + 
    geom_line(data = dobodat, aes(x = datetime, y = do_mgl, color = station), alpha = .4) + 
    geom_line(color = "black", linetype = 2) +
    theme_bw() +
    labs(x = NULL, y = "Dissolved Oxygen (mg/L)")
  # +facet_wrap(station ~ ., scales = "fixed") + theme(legend.position = "none")
)

# Edit with conductivity data; min etc c mods are NA
(cpan <- ggplot(conddat, aes(x = datetime, y = meancmod, fill = station, group = station)) +
    # scale_fill_manual(values =c("TOE" = "#FFAA00", "LIB" = "#33A02C")) +
    # scale_color_manual(values =c("TOE" = "#FFAA00", "LIB" = "#33A02C")) +
    geom_ribbon(aes(ymin = mincmod, ymax = maxcmod), alpha = .2, linetype = 0) +
    geom_line(data = conddat, aes(x = datetime, y = spc, color = station), alpha = .4) +
    geom_line(color = "black", linetype = 2) +
    theme_bw() +
    labs(x = NULL, y = "Specific Conductivity (μS/cm)")
  # +facet_wrap(station ~ ., scales = "fixed") + theme(legend.position = "none")
)

# Combine plots and save
if(saveOutput == T){png(paste("Output/Figures/YBLTE_mindots_01.png", sep = ""), 
                        height = 8, width = 8, unit = "in", res = 1000)}
# if(saveOutput == T){png(paste("Output/Figures/YBLTE_mindots_facet_01.png", sep = ""), 
#                         height = 8, width = 10, unit = "in", res = 1000)}

cowplot::plot_grid(domglpan, tempcpan, cpan, ncol=1)

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


