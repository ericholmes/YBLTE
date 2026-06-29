## YBLTE 2025-26
## Water quality logger data processing script

## Load Libraries ----------------------------------------------------------

library(tidyverse)
library(ggridges)
library(dygraphs)

## Load data ---------------------------------------------------------------
saveOutput = T

## List folders corresponding to DOBO serial numbers
folders <- list.files("Data/tabular/MiniDOTs")

## Load miniDOT serial lookup table
dobolookup <- read.csv("Data/tabular/YBLTE_miniDOTlookup.csv")
  # miniDOT lookup has the station, serial, deploy/retrieval, wet/dry

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

## Create empty data frame to compile all conductivity data, time does not line up with minidots
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
  
  # convert temperature from F to C
  cond_temp$temp <- (cond_temp$temp-32)*5/9
  
  # calculate SpC from raw conductivity and temperature
  # cond_temp$spc <- cond_temp$raw_cond/(1+0.02*(cond_temp$temp-25))
  cond_temp$spc <- cond_temp$raw_cond
  conddat_raw <- rbind(conddat_raw, cond_temp[,c("station","datetime","spc")])
  }

# Time frame for plots
startdate <- as.POSIXct("2025-11-01")
enddate <- as.POSIXct("2026-05-31")

## Load flow data from file then clean
load("Data/cdec_flow_data.Rdata")
# Focus on tributary discharge and current water year
cdec$Param_val <- as.numeric(cdec$Param_val)
flow <- cdec %>% filter(Site_no %in% c("RCS", "FRE", "CCY", "PTC")
                        & parameterCd == 20
                        & Datetime > startdate)

## Clean DOBO data ---------------------------------------------------------
# Drop NA
dobodat <- drop_na(dobodat_raw)
conddat <- drop_na(conddat_raw)

# Filter to current water year 
dobodat <- dobodat[(dobodat$datetime > startdate) & (dobodat$datetime < enddate),]
conddat <- conddat[(conddat$datetime > startdate) & (conddat$datetime < enddate),]

# Sort data
dobodat <- dobodat %>% arrange(station, datetime)
conddat <- conddat %>% arrange(station, datetime)

# Filter dates for bad data, pulled from csv where predetermined
qclookup <- read.csv("Data/tabular/YBLTE_logger_qc_lookup.csv")

qclookup$start <- as.POSIXct(qclookup$start, format="%m/%d/%Y %H:%M")
qclookup$end <- as.POSIXct(qclookup$end, format="%m/%d/%Y %H:%M")

for (s in unique(dobodat$station)){
  qc_filt <- (qclookup %>% filter((model == "MiniDOT") & (station == s)))
  dobodat <- dobodat %>% filter(!((station == s) &
    ((datetime < qc_filt$start) | (datetime > qc_filt$end))))
}

for (s in unique(conddat$station)){
  qc_filt <- qclookup %>% filter((model == "EC") & (station == s))
  conddat <- conddat %>% filter(!((station == s) & 
    ((datetime < qc_filt$start) | (datetime > qc_filt$end))))
}

# Extra manual filtering for outliers (minidot SB4 I80, ec KNG3 YBLR4 SB4 I80 TER)
dobodat <- dobodat %>% filter(!((station == "SB4") &
              ((datetime > as.POSIXct("01/27/2026 11:19", format="%m/%d/%Y %H:%M")) 
               & (datetime < as.POSIXct("02/17/2026 19:42", format="%m/%d/%Y %H:%M")))))
dobodat <- dobodat %>% filter(!((station == "I80") &
              ((datetime > as.POSIXct("12/16/2025 11:57", format="%m/%d/%Y %H:%M")) 
               & (datetime < as.POSIXct("12/16/2025 13:16", format="%m/%d/%Y %H:%M")))))

conddat <- conddat %>% filter(!((station == "KNG3") &
              (((datetime > as.POSIXct("01/24/2026 07:30", format="%m/%d/%Y %H:%M"))
               & (datetime < as.POSIXct("01/28/2026 05:30", format="%m/%d/%Y %H:%M")))
               | ((datetime > as.POSIXct("02/10/2026 17:00", format="%m/%d/%Y %H:%M"))
                  & (datetime < as.POSIXct("02/10/2026 18:00", format="%m/%d/%Y %H:%M")))
               | ((datetime > as.POSIXct("02/23/2026 04:00", format="%m/%d/%Y %H:%M"))
                  & (datetime < as.POSIXct("02/23/2026 05:00", format="%m/%d/%Y %H:%M")))
               | ((datetime > as.POSIXct("03/05/2026 16:30", format="%m/%d/%Y %H:%M"))
                  & (datetime < as.POSIXct("03/05/2026 17:30", format="%m/%d/%Y %H:%M"))))))
conddat <- conddat %>% filter(!((station == "YBLR4") &
              ((datetime > as.POSIXct("02/28/2026 03:00", format="%m/%d/%Y %H:%M"))
               & (datetime < as.POSIXct("02/28/2026 04:00", format="%m/%d/%Y %H:%M")))))
conddat <- conddat %>% filter(!((station == "SB4") &
              ((datetime > as.POSIXct("01/27/2026 22:00", format="%m/%d/%Y %H:%M"))
               & (datetime < as.POSIXct("02/17/2026 16:30", format="%m/%d/%Y %H:%M")))))
conddat <- conddat %>% filter(!((station == "I80") &
              (((datetime > as.POSIXct("02/15/2026 04:00", format="%m/%d/%Y %H:%M"))
               & (datetime < as.POSIXct("02/15/2026 05:00", format="%m/%d/%Y %H:%M")))
               | ((datetime > as.POSIXct("02/21/2026 09:30", format="%m/%d/%Y %H:%M"))
                  & (datetime < as.POSIXct("02/21/2026 10:30", format="%m/%d/%Y %H:%M"))))))
conddat <- conddat %>% filter(!((station == "TER") & 
              ((datetime > as.POSIXct("12/31/2025 22:00", format="%m/%d/%Y %H:%M"))
               & (datetime < as.POSIXct("12/31/2025 23:00", format="%m/%d/%Y %H:%M")))))

# Convert date type
dobodat$date <- as.Date(dobodat$datetime)
conddat$date <- as.Date(conddat$datetime)

# Station factors for plotting
dobodat$station <- factor(dobodat$station, 
                          levels = c("KNG3", "CNW", "YBLR4", "SB4", "I80", "TEW", "TER"))
conddat$station <- factor(conddat$station, 
                          levels = c("KNG3", "CNW", "YBLR4", "SB4", "I80", "TEW", "TER"))



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

# Station factor for daily points (reversed to match latitude order when stacked)
dbmelt$stationfac <- factor(dbmelt$station, 
                            levels = c("TER", "TEW", "I80", "SB4", "YBLR4", "CNW", "KNG3"))
cdmelt$stationfac <- factor(cdmelt$station,
                            levels = c("TER", "TEW", "I80", "SB4", "YBLR4", "CNW", "KNG3"))

## Plot DOBO+EC data ----------------------------------------------------------
# Note: SB4 has a large data gap (Jan 27-Feb 17), so many of the plots will plot SB4 twice
sb4_Dcondition <- (dobodat$station=="SB4")&(dobodat$datetime > as.POSIXct("02/01/2026", format="%m/%d/%Y"))
sb4_Ccondition <- (conddat$station=="SB4")&(conddat$datetime > as.POSIXct("02/01/2026", format="%m/%d/%Y"))

dobodat_full <- dobodat
dobo_sb4 <- dobodat[sb4_Dcondition,]
dobodat <- dobodat[!sb4_Dcondition,]

conddat_full <- conddat
cond_sb4 <- conddat[sb4_Ccondition,]
conddat <- conddat[!sb4_Ccondition,]

# Save datasets for plotting in dashboard
if(saveOutput == T){
  save(dobodat, dobo_sb4, dobodat_full, conddat, cond_sb4, conddat_full, file = "Data/YBLTE_logger.RData")
}

# Read in and clean wqp data
wqp <- readxl::read_excel("Data/tabular/YBLTE_point_wq.xlsx")
wqp <- wqp %>% filter(Site %in% unique(dobodat$station))

# Create dygraphs for cleaning data and interactive aspect
for(s in unique(dobodat_full$station)){
  dyD <- dygraph(dobodat_full %>% filter(station == s) %>% subset(select = c(datetime, temp_c, do_mgl)),
                main = paste("MiniDOT at", s), group = s, height = 300, width = "47%") %>%     
    dyAxis("y", label = "Temperature (C)") %>% 
    dyAxis("y2", label = "Dissolved Oxygen (mg/L)", independentTicks = T) %>% 
    dySeries("temp_c", label = "Temperature") %>% 
    dySeries("do_mgl", label = "DO", axis = "y2") %>% 
    dyRangeSelector(dateWindow = c(startdate, enddate))

  dyC <- dygraph(conddat_full %>% filter(station == s) %>% subset(select = c(datetime, spc)), 
                main = paste("EC at", s), group = s, height = 300, width = "47%") %>% 
    dyAxis("y", label = "Specific Conductivity (uS/cm)") %>% 
    dySeries("spc", label = "Conductivity") %>% 
    dyRangeSelector(dateWindow = c(startdate, enddate))
  # Set plots in same row and adjust size
  dy <- htmltools::browsable(htmltools::tags$div(style = "display: flex; flex-direction: row; gap: 20px;", dyD, dyC))
  print(dy)
}

# Compare logger data with wqp data
wqp_plts <- c()
for(s in unique(dobodat$station)){
  wqp_plts <- append(wqp_plts,ggplot() + geom_line(data = dobodat %>% filter(station == s), aes(x = datetime, y = temp_c)) +
                   geom_line(data = dobo_sb4 %>% filter(station == s), aes(x = datetime, y = temp_c)) +
                   geom_point(data = wqp %>% filter(Site == s), aes(x = Date, y = Temp), color = "blue") + theme_bw() +
                   facet_wrap(station~.))
}
for(s in unique(dobodat$station)){
  wqp_plts <- append(wqp_plts,ggplot() + geom_line(data = dobodat %>% filter(station == s), aes(x = datetime, y = do_mgl)) +
                   geom_line(data = dobo_sb4 %>% filter(station == s), aes(x = datetime, y = do_mgl)) +
                   geom_point(data = wqp %>% filter(Site == s), aes(x = Date, y = DO_mgl), color = "blue") + theme_bw() +
                   facet_wrap(station~.))
}
for(s in unique(conddat$station)){
  wqp_plts <- append(wqp_plts,ggplot() + geom_line(data = conddat %>% filter(station == s), aes(x = datetime, y = spc)) +
                   geom_line(data = cond_sb4 %>% filter(station == s), aes(x = datetime, y = spc)) +
                   geom_point(data = wqp %>% filter(Site == s), aes(x = Date, y = SPC_uscm), color = "blue") + theme_bw() +
                   facet_wrap(station~.))
}
# Plot all logger data against corresponding wqp data
cowplot::plot_grid(plotlist = wqp_plts[15:21]) # set to just get EC plots

# Fit wqp to EC logger data to get transformation per site
transLog <- data.frame(Site = character(), spc = numeric(), Date = Date())
for(s in unique(conddat$station)){
  # Filter for station
  siteWQP <- wqp %>% filter(Site == s)
  siteEC <- conddat %>% filter(station == s)
  # Fit logger data to point wq, get medians for each day
  ec_temp <- siteEC %>% group_by(date) %>% 
    summarize(spc = median(spc, na.rm = T))
  # Convert date type
  ec_temp$date <- as.POSIXct(ec_temp$date)
  # Merge wqp and logger data so points match for model
  temp <- merge(siteWQP, ec_temp, by.x = "Date", by.y = "date")
  # Fit logger data to wqp
  sitemodel <- lm(temp$spc~temp$SPC_uscm)
  # Transform logger values with model coefficients
  c <- sitemodel$coefficients
  trans <- data.frame(date = siteEC$datetime, spc = (siteEC$spc/c[2])-c[1]*c[2])
  trans$Site <- s
  # Add new values to data frame
  transLog <- rbind(transLog, trans)
}

# Plot logger, wqp, and transformed data (just EC plots)
trans_plts <- c()
for(s in unique(conddat$station)){
  trans_plts <- append(trans_plts, ggplot() + geom_line(data = conddat %>% filter(station == s), aes(x = datetime, y = spc)) +
                       geom_point(data = wqp %>% filter(Site == s), aes(x = Date, y = SPC_uscm), color = "blue") + 
                       geom_line(data = transLog %>% filter(Site == s), aes(x = date, y = spc), color = "red") + theme_bw() +
                       facet_wrap(station~.))
}
cowplot::plot_grid(plotlist = trans_plts)

wqp$station <- wqp$Site
### Temperature ----
ggplot(dobodat, aes(x = datetime, y = temp_c)) + geom_line(aes(color = station)) +
  geom_line(data = dobo_sb4, aes(x = datetime, y = temp_c, color = station)) + theme_bw()

ggplot(dobodat, aes(x = datetime, y = temp_c)) + geom_line() + 
  geom_line(data = dobo_sb4, aes(x = datetime, y = temp_c)) + theme_bw() + 
  facet_wrap(station~.)

ggplot(dobodaily, aes(x = maxtemp, fill = station)) + geom_density(alpha = .2)+ 
  facet_wrap(station ~ .) + theme_bw()

ggplot(dbmelt[dbmelt$variable %in% c("mintemp", "meantemp", "maxtemp"),], 
       aes(x = value, y = station, fill = stat(x))) + geom_density_ridges_gradient(show.legend = F) + 
  scale_fill_viridis_c(option = "B", direction = -1) +
  facet_grid(. ~ variable, scales = "free") + theme_bw()

### DO ----

ggplot(dobodat, aes(x = datetime, y = do_mgl)) + geom_line(aes(color = station)) +
  geom_line(data = dobo_sb4, aes(x = datetime, y = do_mgl, color = station)) + theme_bw()

ggplot(dobodat, aes(x = datetime, y = do_mgl)) + geom_line() +
  geom_line(data = dobo_sb4, aes(x = datetime, y = do_mgl)) + theme_bw() + 
  facet_wrap(station~.)

ggplot(dobodaily, aes(x = maxdo, fill = station)) + geom_density(alpha = .2) + 
  facet_wrap(station ~ .) + theme_bw()

ggplot(dbmelt[dbmelt$variable %in% c("mindo", "meando", "maxdo"),], 
       aes(x = value, y = stationfac, fill = stat(x))) + 
  geom_density_ridges_gradient(show.legend = F) + 
  scale_fill_viridis_c(option = "B", direction = 1) +
  facet_grid(. ~ variable, scales = "fixed") + theme_bw()

if(saveOutput == T){png(paste("Output/Figures/YBLTE_Temp+DO_panels_%02d.png", sep = ""), 
                        height = 6, width = 8, unit = "in", res = 1000)}

ggplot() + geom_line(data = dobodat, aes(x = datetime, y = temp_c, color = station), alpha = .5) +
  geom_line(data = dobo_sb4, aes(x = datetime, y = temp_c, color = station)) + 
  geom_point(data = wqp, aes(x = Date, y = Temp, color = Site)) + theme_bw() + 
  labs(x = NULL, y = "Temperature (C)") +
  facet_wrap(station~.)

ggplot() + geom_line(data = dobodat, aes(x = datetime, y = do_mgl, color = station), alpha = .5) +
  geom_line(data = dobo_sb4, aes(x = datetime, y = do_mgl, color = station)) + 
  geom_point(data = wqp, aes(x = Date, y = DO_mgl, color = Site)) + theme_bw() +
  labs(x = NULL, y = "Dissolved Oxygen (mg/L)") +
  facet_wrap(station~.)

if(saveOutput == T){dev.off()}

### Conductivity ----

ggplot(conddat, aes(x = datetime, y = spc, group = station)) + geom_line(aes(color = station)) +
  geom_line(data = cond_sb4, aes(x = datetime, y = spc, color = station)) + theme_bw()

ggplot(conddat, aes(x = datetime, y = spc, group = station)) + geom_line() +
  geom_line(data = cond_sb4, aes(x = datetime, y = spc)) + theme_bw() +
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

cowplot::plot_grid(domglpan + theme(axis.text.x = element_blank()), 
                   tempcpan + theme(axis.text.x = element_blank()), 
                   cpan + theme(axis.text.x = element_blank()), 
                   flowplt, ncol=1, align="v")

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


