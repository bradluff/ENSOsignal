#' Download the average daily discharge for a vector of gages from the USGS site.
#'
#' @param gages a vector of gages to download discharge data for.
#' @param discharge_directory The directory where the raw discharge files are located.
#' @param start_date The first date to download data for.
#' @param end_date The last date to download data for.
#' @param missing_days The number of missing days in the discharge record that is acceptable.



discharge_download <- function(gages, discharge_directory, start_date = "1951-10-01", end_date = "2011-09-30", missing_days = 14){
  library(dataRetrieval)
  library(tidyr)
  setwd(discharge_directory)
  for (each in gages) {
    if (!file.exists(paste0(each,"_discharge.csv"))){
      parameterCodes <- c("00060")
      try({
        discharge <- readNWISdv(each, parameterCodes, startDate = start_date, endDate = end_date)
        if (length(discharge) == 5){
          colnames(discharge) <- c("USGS","Gage","Date","Discharge","Letters")
          discharge[discharge$Discharge < 0,4] <- NA
          discharge$Date <- as.Date(discharge$Date)
          discharge <- discharge[(discharge$Date >= start_date) & (discharge$Date <= end_date),]
          days_of_record <- as.numeric(difftime(as.Date(end_date), as.Date(start_date), units = "days"))
          if ((length(discharge$Discharge) >= (days_of_record-missing_days)) & (length(discharge) == 5)){
            fileName <- paste0(each, "_discharge.csv")
            write.table(discharge, fileName, row.names = FALSE, col.names = FALSE, sep = ",")
            print(paste0(each, " downloaded successfully"))
          } else {
            print(paste0(each," discharge record corrupt"))
          }
        } else {
          print(paste0(each," data in wrong format"))
        }
      }, silent = FALSE)
    } else {
      print(paste0(each," already downloaded"))
    }
  }
}

