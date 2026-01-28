# PDPs --------------------------------------------------------------------

# Install if needed:
# install.packages(c("gbm","dplyr","ggplot2","pdp","gridExtra"))

library(gbm)
library(tidyverse)
library(pdp)
library(gridExtra)
download_data = F
# ---- 1. Load and curate data ----
sensor_codes <- data.frame(sensor = c("chla", "ec", "discharge_cfs", "fdom", 
                                      "wtemp_f", "domgl","ph", "turb_fnu",
                                      "stage_ft"), 
                           sensor_num = c(28, 100, 20, 266, 
                                          25, 61, 62, 221,
                                          1))
startdate <- "2015-10-1"
enddate <- Sys.Date()

if(download_data == T){
  cdec <- data.frame()
  for(station in "LIS"){
    for(param in sensor_codes$sensor_num){
      print(paste("downloading:", station, param))
      try(cdec <- rbind(cdec, downloadCDEC(site_no = station, parameterCd = param, startDT = startdate , endDT = enddate)))
    }
  }
}else(load("Data/cdec_LIS_2015-2025_data.Rdata"))
# save(cdec, file = "Data/cdec_LIS_2015-2025_data.Rdata")

cdec$Param_val <- as.numeric(cdec$Param_val)

cdecmerge <- merge(cdec, sensor_codes, by.x = "parameterCd", by.y = "sensor_num")

#Substitute YBY flow data for LIS flow data
cdecmerge <- cdecmerge[cdecmerge$sensor != "discharge_cfs",]

load("Data/cdec_flow_data.Rdata")

yby <- cdec[cdec$Site_no == "YBY",]
yby$sensor <- "discharge_cfs"
yby$Param_val <- as.numeric(yby$Param_val)

cdecmerge <- rbind(cdecmerge, yby)

cdecmerge$date <- as.Date(cdecmerge$Datetime)
cdecmergeply <- cdecmerge %>% group_by(date, sensor) %>% 
  summarize(medval = median(Param_val, na.rm = T)) %>% data.frame()

ggplot(cdecmergeply, aes(x = date, y = medval)) + geom_line() + theme_bw() +
  facet_wrap(sensor ~ ., scales = "free_y")

# Pivoting from long to wide
cdec_wide <- cdecmergeply %>% pivot_wider(names_from = sensor, values_from = medval)
cdec_wide <- cdec_wide %>% filter(chla < 100,
                                  domgl > 0.01,
                                 ph > 1,
                                 turb_fnu < 300,
                                 wtemp_f < 130 & wtemp_f > 32) %>% select(-fdom)

cdec_wide$wtemp_c <- (cdec_wide$wtemp_f - 32) * 5 / 9

# cdec_wide$discharge_cfs <- log10(cdec_wide$discharge_cfs + 1000)

# Remove first row with NA lag
# data <- na.omit(data)

# ---- 2. Create lagged predictors ----
load(file = "Data/YBLTE_percent_flow.Rdata")

data <- cdec_wide %>%
  arrange(date) %>%
  mutate(
    discharge_cfs_lag1 = lag(discharge_cfs, 7),
    wtemp_c_lag1 = lag(wtemp_c, 7),
    domgl_lag1   = lag(domgl, 7),
    ec_lag1 = lag(ec, 7),
    # fdom_lag1 = lag(fdom, 7),
    ph_lag1   = lag(ph, 7),
    jday = as.numeric(format(date, format = "%j"))) %>% 
  merge(cdectribply[cdectribply$Trib == "Tribs", c("Date", "sumperc")], 
              by.x = "date", by.y = "Date", all.x = T) %>% 
  filter(jday %in% 1:150) %>% 
  #filter(!(jday %in% 200:330)) %>% 
  na.omit()

hist(data$discharge_cfs)
hist(cdecmerge[cdecmerge$sensor == "discharge_cfs", "Param_val"])
# 
# data <- merge(data, 
#                     cdectribply[cdectribply$Trib == "Tribs", c("Date", "sumperc")], 
#                     by.x = "date", by.y = "Date", all.x = T)

# ---- 3. Fit boosted regression tree ----
brt_model <- gbm(
  formula = chla ~ discharge_cfs + wtemp_c + domgl + ec + ph + sumperc +
    discharge_cfs_lag1 + wtemp_c_lag1 + domgl_lag1 + ec_lag1 + ph_lag1,
  data = data,
  distribution = "gaussian",
  n.trees = 2000,
  interaction.depth = 4,
  shrinkage = 0.01,
  cv.folds = 5,
  n.minobsinnode = 10,
  verbose = T
)

# ---- 4. Optimal number of trees ----
best_iter <- gbm.perf(brt_model, method = "cv")

# ---- 5. Predictions and performance ----
data$pred <- predict(brt_model, newdata = data, n.trees = best_iter)
rmse <- sqrt(mean((data$chla - data$pred)^2))
r2 <- cor(data$chla, data$pred)^2
cat("RMSE:", rmse, "\nR²:", r2, "\n")

# ---- 6. Variable importance ----
vi <- summary(brt_model, n.trees = best_iter)
print(vi)

# ---- 7. Observed vs predicted plot ----
ggplot(data, aes(x = chla, y = pred)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope=1, intercept=0, color="red") +
  labs(x="Observed chla", y="Predicted chla",
       title="Boosted Regression Tree Predictions") +
  theme_minimal()

# 
# ggplot(data, aes(x = discharge_cfs, y = ec)) + geom_point()
# 
# ggplot(data, aes(x = discharge_cfs, y = wtemp_c)) + geom_point()
# 

# ranked partial dependence plots ----
ggplot(data, aes(x = ec, y = chla, color = discharge_cfs)) + geom_point() + scale_color_viridis_c()
ggplot(data, aes(x = ec, y = chla, color = wtemp_c)) + geom_point() + scale_color_viridis_c()
ggplot(data, aes(x = ec, y = chla, color = jday)) + geom_point() + scale_color_viridis_c()
ggplot(data, aes(x = logflow, y = ec, color = log(chla))) + geom_point() + scale_color_viridis_c()
data$logflow <- log(data$discharge_cfs)

gbm1 <- dismo::gbm.step(
  gbm.x =  c(#"discharge_cfs", 
             "domgl", "ec", "ph",
  "turb_fnu", "wtemp_c",  "logflow",
  #"discharge_cfs_lag1", "ec_lag1",
 # "wtemp_c_lag1",
  # "domgl_lag1",  "ph_lag1",
  "jday"),
  gbm.y = "chla",
  data = data,
  family = "gaussian", tree.complexity = 4,
  learning.rate = .1, bag.fraction = .5, max.trees = 3000
)

psgbm1 <- data.frame()
for(variable in 1:length(gbm1[["contributions"]][["var"]])){
  var1 <- gbm1[["contributions"]][["var"]][variable]
  print(paste(variable, var1))
  temp <- plot(gbm1, var1, return.grid = T)
  temp$rank <- variable
  temp$contribution <- gbm1[["contributions"]][["rel.inf"]][variable]
  temp$Variable <- var1
  colnames(temp) <- c("Value", "Response", "Rank", "Contribution", "Variable")
  psgbm1 <- rbind(psgbm1, temp)
}

psgbm1$varfac <- factor(psgbm1$Variable, levels = unique(psgbm1$Variable))

# varnamelookup <- data.frame(varname = c("Copepoda", "Insecta", 
#                                         "Large_cladocera", "Ostracoda", "Rare","Rotifera", "Small_cladocera", 
#                                         "CHL", "DOC", "rangeDO", "rangetemp", "SPC"),
#                             labels = c("Copepoda~log(orgs.~m^-3)", "Insecta~log(orgs.~m^-3)",
#                                        "Lg.~cladocera~log(orgs.~m^-3)", "Ostracoda~log(orgs.~m^-3)", "Rare~taxa~log(orgs.~m^-3)",
#                                        "Rotifera~log(orgs.~m^-3)", "Sm.~cladocera~log(orgs.~m^-3)",
#                                        "Chl-alpha~(mu*g~L^-1)", "DOC~(mu*g~L^-1)",
#                                        "DO~MDR~(mg~L^-1)","Wtemp~MDR~(degree*C)", "SPC~(mu*S~cm^-1)"))
# 
# 
# psgbm1$varfac <- factor(psgbm1$Variable, levels = gbm1[["contributions"]][["var"]],
#                         labels = varnamelookup[match(gbm1[["contributions"]][["var"]], varnamelookup$varname), "labels"])
# 
psgbm1sub <- psgbm1[duplicated(psgbm1$varfac) == F,]

png("Output/Figures/YBLTE_CHLmod_pd_plots_b_%02d.png",
    height = 8, width = 9, units = "in", res = 1000, family = "serif")
ggplot(psgbm1, aes(x = Value, y = Response)) + geom_line() +
  facet_wrap(varfac ~., scales = "free_x", labeller=label_parsed) + theme_bw() +
  labs(x = "Parameter value", y = expression(Chl-alpha~(mu*g~L^-1))) +
  ggpp::geom_text_npc(data = data.frame(varfac = psgbm1sub$varfac,
                                        label = round(psgbm1sub$Contribution, digits = 2),
                                        x = 0.05,
                                        y = 0.95),
                      mapping = aes(npcx = x, npcy = y, label = label),
                      size = 3.5)

dev.off()


