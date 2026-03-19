## YBLTE 2025-26
## Water quality logger data processing script

## note - for adding new data, just need to update sites and filter good date range

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
dobodat_raw <- data.frame()

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
    dobodat_raw <- rbind(dobodat_raw, tempdat[, c("station", "datetime", "temp_c", "do_mgl")])
    ##Clean up workspace after each file
    rm(serial, tempdat)
  }
}

## Create empty data frame to compile all conductivity data, different because time not guaranteed to line up
conddat_raw <- data.frame()

## Load EC data
ec <- list.files("Data/tabular/EC_loggers", full.names = F, pattern = ".csv")
# ec <- as.list(ec[grepl(".csv", ec)])
for(f in ec){
  print(f)
  print(sub(".*_(.*)\\.csv", "\\1", f))
  cond_temp <- read.csv(paste0("Data/tabular/EC_loggers/", f), skip= 1, header = T)
  cond_temp <- cond_temp[,2:4]
  colnames(cond_temp) <- c("datetime", "raw_cond", "temp")
  
  # get station from file name
  # cond_temp$station <- str_sub(f, -7, -5)
  cond_temp$station <- sub(".*_(.*)\\.csv", "\\1", f)
  # convert time type
  cond_temp$datetime <- as.POSIXct(cond_temp$datetime, tz="America/Los_Angeles", 
                                   format="%m/%d/%y %I:%M:%S %p")
  # cond_temp$datetime <- format(cond_temp$datetime, "%Y-%m-%d %H:%M:%S")
  
  # convert temperature from F to C
  cond_temp$temp <- (cond_temp$temp-32)*5/9
  
  # calculate SpC from raw conductivity and temperature
  cond_temp$spc <- cond_temp$raw_cond/(1+0.02*(cond_temp$temp-25))
  
  conddat_raw <- rbind(conddat_raw, cond_temp[,c("station","datetime","spc")])
  }

# Time frame for plots
startdate <- as.POSIXct("2025-10-01")
enddate <- Sys.Date()

## Load flow data from file then clean
load("Data/cdec_flow_data.Rdata")
# Focus on tributary discharge and current water year
cdec$Param_val <- as.numeric(cdec$Param_val)
flow <- cdec %>% filter(Site_no %in% c("RCS", "FRE", "CCY", "PTC")
                        & parameterCd == 20
                        & Datetime > startdate)

## Clean DOBO data ---------------------------------------------------------
# Filter to current water year 
dobodat <- dobodat_raw[dobodat_raw$datetime > startdate,]
conddat <- conddat_raw[conddat_raw$datetime > startdate & is.na(conddat_raw$datetime) == F,]

# Manually selecting date ranges per sensor/station based on large jumps at beginning/end in plot
  # setting values as NA bc ignored in plot and won't drop any values in other columns
dobodat[dobodat$station=="SB4"&
          dobodat$datetime>as.POSIXct("2026-01-27"),c("temp_c", "do_mgl")] <- NA
dobodat[dobodat$station=="TER"&(dobodat$datetime<as.POSIXct("2025-11-19")|
          dobodat$datetime>as.POSIXct("2026-01-22")),c("temp_c", "do_mgl")] <- NA

dobodat[dobodat$station=="I80"&(dobodat$datetime<as.POSIXct("2025-10-30 08:57:00")|
          dobodat$datetime>as.POSIXct("2025-12-16 12:32:00")),"do_mgl"] <- NA

conddat[conddat$station=="SB4"&(conddat$datetime<as.POSIXct("2025-12-27 01:30:00")|
          conddat$datetime>as.POSIXct("2026-01-27 22:00:00")),"spc"] <- NA
conddat[conddat$station=="TER"&(conddat$datetime<as.POSIXct("2025-11-18 14:30:00")|
          conddat$datetime>as.POSIXct("2026-01-24 10:30:00")),"spc"] <- NA
conddat[conddat$station=="TEW"&
          conddat$datetime<as.POSIXct("2025-11-04 14:30:00"), "spc"] <- NA

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
  summarize(mintemp = min(temp_c, na.rm = T), meantemp = mean(temp_c, na.rm = T), maxtemp = max(temp_c, na.rm = T),
            mindo = min(do_mgl, na.rm = T), meando = mean(do_mgl, na.rm = T), maxdo = max(do_mgl, na.rm = T)) %>% 
  data.frame()

conddaily <- conddat %>% 
  group_by(station, date) %>% 
  summarize(minc = min(spc, na.rm = T), meanc = mean(spc, na.rm = T), maxc = max(spc, na.rm = T)) %>% 
  data.frame()

# Reshape data frame for daily points
dbmelt <- reshape2::melt(dobodaily, id.vars = c("station", "date"))
cdmelt <- reshape2::melt(conddaily, id.vars = c("station", "date"))
cdmelt <- cdmelt[!is.na(cdmelt$value) & is.finite(cdmelt$value),]
# full list of DOBO stations
# c("TER", "TEW", "I80", "SB4", "CNW", "KNG3")

# Station factors for plotting
dbmelt$stationfac <- factor(dbmelt$station, 
                            levels = c("TEW", "TER", "SB4", "I80", "KNG3"))
cdmelt$stationfac <- factor(cdmelt$station,
                            levels = c("TEW", "TER", "SB4", "I80", "KNG3"))
# save data for transfer
# save(dobodat, file="minidot.Rdata")
# save(conddat, file="eclog.Rdata")

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
       aes(x = value, y = stationfac, fill = stat(x))) + 
  geom_density_ridges_gradient(show.legend = F) + 
  scale_fill_viridis_c(option = "B", direction = 1) +
  facet_grid(. ~ variable, scales = "fixed") + theme_bw()

### Conductivity ----

ggplot(conddat, aes(x = datetime, y = spc, group = station)) + geom_line(aes(color = station)) + theme_bw()

ggplot(conddat, aes(x = datetime, y = spc, group = station)) + geom_line() + theme_bw() +
  facet_wrap(station~.)

ggplot(conddaily, aes(x = maxc, fill = station)) + geom_density(alpha = .2) + 
  facet_wrap(station ~ .) + theme_bw()
unique(cdmelt$variable)

ggplot(cdmelt, aes(x = value, y = stationfac, fill = stat(x))) + 
  geom_density_ridges_gradient(show.legend = F) +
  scale_fill_viridis_c(option = "B", direction = 1) +
  facet_grid(. ~ variable, scales = "fixed") + theme_bw()

# Plot flow of tributaries ------------------------------------------------
flowplt <- ggplot(flow, aes(x = Datetime, y = Param_val, color = Site_no)) +
  geom_line() + theme_bw() + xlim(startdate, enddate) +
  labs(x = NULL, y = "Discharge (cfs)", color = "station")

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
  
  mincmod <- loess(minc ~ as.numeric(datetime), conddaily[conddaily$station %in% i & is.finite(conddaily$minc), ], span = spanwidth)
  maxcmod <- loess(maxc ~ as.numeric(datetime), conddaily[conddaily$station %in% i & is.finite(conddaily$maxc), ], span = spanwidth)
  meancmod <- loess(meanc ~ as.numeric(datetime), conddaily[conddaily$station %in% i & is.finite(conddaily$meanc), ], span = spanwidth)

  conddat$mincmod <- ifelse(conddat$station == i, predict(mincmod, conddat$datetime), conddat$mincmod)
  conddat$maxcmod <- ifelse(conddat$station == i, predict(maxcmod, conddat$datetime), conddat$maxcmod)
  conddat$meancmod <- ifelse(conddat$station == i, predict(meancmod, conddat$datetime), conddat$meancmod)
  
  # Clear workspace
  
  rm(mincmod, maxcmod, meancmod)
}

# Continuous temp, conductivity, and DO plotting
  # use facet wrap if too many stations is too noisy

# manually scale colors to match across EC and DOBO
  # add colors once all stations are in
# plot_col <- scale_color_manual(c("TER" = , "TEW" = , "I80" = , 
#                                  "SB4" = , "CNW" = , "KNG3" = ))
# plot_fil <- scale_fill_manual(c("TER" = , "TEW" = , "I80" = , 
#                                 "SB4" = , "CNW" = , "KNG3" = ))

(tempcpan <- ggplot(dobodat, aes(x = datetime, y = meantempmod, fill = station, group = station)) +
    # scale_fill_manual(values =c("TOE" = "#FFAA00", "LIB" = "#33A02C")) +
    # scale_color_manual(values =c("TOE" = "#FFAA00", "LIB" = "#33A02C")) +
    geom_ribbon(aes(ymin = mintempmod, ymax = maxtempmod), alpha = .2, linetype = 0) + 
    geom_line(data = dobodat, aes(x = datetime, y = temp_c, color = station), alpha = .4) + 
    geom_line(color = "black", linetype = 2) +
    theme_bw() + xlim(startdate, enddate) +
    labs(x = NULL, y = "Temperature (C)")
    # +facet_wrap(station ~ ., scales = "fixed") + theme(legend.position = "none")
  )

(domglpan <- ggplot(dobodat, aes(x = datetime, y = meandomod, fill = station, group = station)) +
    # scale_fill_manual(values =c("TOE" = "#FFAA00", "LIB" = "#33A02C")) +
    # scale_color_manual(values =c("TOE" = "#FFAA00", "LIB" = "#33A02C")) +
    geom_ribbon(aes(ymin = mindomod, ymax = maxdomod), alpha = .2, linetype = 0) + 
    geom_line(data = dobodat, aes(x = datetime, y = do_mgl, color = station), alpha = .4) + 
    geom_line(color = "black", linetype = 2) +
    theme_bw() + xlim(startdate, enddate) +
    labs(x = NULL, y = "Dissolved Oxygen (mg/L)")
  # +facet_wrap(station ~ ., scales = "fixed") + theme(legend.position = "none")
)

# Edit with conductivity data; min etc c mods are NA
(cpan <- ggplot(conddat, aes(x = datetime, y = meancmod, fill = station, group = station)) +
    # scale_fill_manual(values =c("TOE" = "#FFAA00", "LIB" = "#33A02C")) +
    # scale_color_manual(values =c("TOE" = "#FFAA00", "LIB" = "#33A02C")) +
    # geom_ribbon(aes(ymin = mincmod, ymax = maxcmod), alpha = .2, linetype = 0) +
    geom_line(data = conddat, aes(x = datetime, y = spc, color = station), alpha = .4) +
    # geom_line(color = "black", linetype = 2) +
    theme_bw() + xlim(startdate, enddate) +
    labs(x = NULL, y = "Specific Conductivity (μS/cm)")
  # +facet_wrap(station ~ ., scales = "fixed") + theme(legend.position = "none")
)

# Combine plots and save
if(saveOutput == T){png(paste("Output/Figures/YBLTE_mindots_01.png", sep = ""), 
                        height = 8, width = 8, unit = "in", res = 1000)}
# if(saveOutput == T){png(paste("Output/Figures/YBLTE_mindots_facet_01.png", sep = ""), 
#                         height = 8, width = 10, unit = "in", res = 1000)}

cowplot::plot_grid(domglpan, tempcpan, cpan, flowplt, ncol=1, align="v")

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


