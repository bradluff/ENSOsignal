#' Download the average daily discharge for a vector of gages from the USGS site.
#'
#' @param gages a vector of gages to download discharge data for.
#' @param discharge_directory The directory where the raw discharge files are located.
#' @param start_date The first date to download data for.
#' @param end_date The last date to download data for.
#' @param missing_days The number of missing days in the discharge record that is acceptable.



discharge_download <- function(gages, discharge_directory, start_date = "1951-10-01", end_date = "2011-09-30", missing_days = 14){

  # set the working directory to the discharge directory
  setwd(discharge_directory)

  # step through every gage
  for (each in gages) {

    # if the file has already been downloaded skip to next
    if (!file.exists(paste0(each,"_discharge.csv"))){

      # only need to download discharge
      parameterCodes <- c("00060")

      try({

        # download the discharge
        discharge <- dataRetrieval::readNWISdv(each, parameterCodes, startDate = start_date, endDate = end_date)

        # check if file in correct format
        if (length(discharge) == 5){

          # rename columns
          colnames(discharge) <- c("USGS","Gage","Date","Discharge","Letters")

          # if discharge is less than 0 set to NA
          discharge[discharge$Discharge < 0,4] <- NA

          # convert the date to date format
          discharge$Date <- as.Date(discharge$Date)

          # select records between the specified dates
          discharge <- discharge[(discharge$Date >= start_date) & (discharge$Date <= end_date),]

          # the number of rows expected in the dataframe
          days_of_record <- as.numeric(difftime(as.Date(end_date), as.Date(start_date), units = "days"))

          # if the data is correct
          if ((length(discharge$Discharge) >= (days_of_record-missing_days)) & (length(discharge) == 5)){

            # create the filename
            fileName <- paste0(each, "_discharge.csv")

            # save the discharge to a csv file in the disharge directory
            write.table(discharge, fileName, row.names = FALSE, col.names = FALSE, sep = ",")

            # print update to console
            print(paste0(each, " downloaded successfully"))

          } else {

            # fails the completeness check
            print(paste0(each," discharge record corrupt"))
          }

        } else {

          # fails the formatting check
          print(paste0(each," data in wrong format"))
        }

        # don't print errors
      }, silent = FALSE)

    } else {

      # the file already exists in the discharge directory
      print(paste0(each," already downloaded"))
    }
  }
}

