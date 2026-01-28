## Useful functions:

# From Juan's WDLImportR

ImportContinuousRaw <- function(stationNumber, parameters = NULL) {
  
  # HTTP request URL ----
  searchURL <- paste0("https://data.cnra.ca.gov/api/3/action/datastore_search?resource_id=cdb5dd35-c344-4969-8ab2-d0e2d6c00821&q=", stationNumber)
  
  # Call on the API, transform JSON data into data frame ----
  df_station_link <- searchURL %>%
    jsonlite::read_json() %>%
    purrr::pluck("result", "records") %>%
    tidyr::tibble() %>%
    tidyr::unnest_wider(1)
  
  # Filter data frame for "Daily Mean", and make sure user requested a valid station number ----
  if (is.null(parameters)) {
    filtered_df_station_link <- df_station_link %>%
      dplyr::filter(station_number == stationNumber,
                    output_interval == "Raw")
  } else {
    filtered_df_station_link <- df_station_link %>%
      dplyr::filter(station_number == stationNumber,
                    output_interval == "Raw",
                    parameter %in% parameters)
  }
  
  # Read in CSV links from data frame, format and join resultant data frames ----
  df_raw <- filtered_df_station_link$download_link %>%
    lapply(. %>% ImportContinuousCSV) %>%
    purrr::reduce(dplyr::full_join, by = 'Date') %>%
    # dplyr::mutate(Date = as.POSIXct(Date, format = "%m/%d/%Y %H:%M:%S")) %>%
    dplyr::rename_with(~ c("date", filtered_df_station_link$parameter)) %>%
    janitor::clean_names()
  
  # Return final generated data frame ----
  return(df_raw)
  
}

ImportContinuousCSV <- function(csv_link){
  
  # Read CSV link and format
  df_wdl <- utils::read.csv(csv_link, skip = 2) %>%
    dplyr::select(!c("Qual", "X"))
  
  # Return formatted data frame
  return(df_wdl)
  
}

## Simplified WDL download code

downloadWDLcont <- function(StationNumber, Parameter){
  read.csv(paste0("https://wdlstorageaccount.blob.core.windows.net/continuous/",
                  StationNumber, "/por/", StationNumber,"_", Parameter, "_raw.csv"))
}

## Download Functions for NWIS and CDEC
 
downloadNWIS <- function(site_no, parameterCd, startDT, endDT){
  temp <- read.table(paste0("https://waterservices.usgs.gov/nwis/iv/?sites=", site_no,
                            "&parameterCd=", parameterCd,
                            "&startDT=", startDT,
                            "T00:00:00-07:00&endDT=", endDT,
                            "T00:00:00-07:00&siteStatus=all&format=rdb"),
                     skip = 30, fill = TRUE, stringsAsFactors = F, 
                     col.names = c("Agency", "Site_no", "Date", "Time", "tz", "Param_val", "qc_code"))
  temp$parameterCd <- parameterCd
  temp$Datetime <- as.POSIXct(paste(temp$Date, temp$Time), format = "%Y-%m-%d %H:%M")
  return(temp)
}

downloadCDEC <- function(site_no, parameterCd, startDT, endDT){
  temp <- read.table(paste("http://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=", site_no, "&SensorNums=",
                           parameterCd, "&dur_code=", "E", "&Start=", startDT, "&End=", endDT, sep=""),
                     header=FALSE, sep=",", skip=1, stringsAsFactors = F)
  
  temp <- temp[,c(5,7)]
  colnames(temp) <- c("Datetime", "Param_val")
  temp$Site_no <- site_no
  temp$parameterCd <- parameterCd
  temp$Datetime <- as.POSIXct(temp$Datetime, format = "%Y%m%d %H%M")
  
  return(temp[,c("Site_no", "Datetime", "parameterCd", "Param_val")])
}
