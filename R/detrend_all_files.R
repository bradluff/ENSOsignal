#' Detrend all of the discharge data for the project.
#'
#' @param discharge_directory The directory where the raw discharge files are located.
#' @param detrended_directory The directory where the detrended discharge files are located.
#' @param project_directory The directory used to save output files.
#'
#' @return a list of the gages used for the study



pre_process <- function(discharge_directory, detrended_directory, project_directory){

  # list of all files in discharge directory
  water_files <- list.files(discharge_directory, full.names = T)

  # Lists to append to while looping
  gage_list <- c()
  slope_list <- c()

  # detrend and calculate moving average
  for (every in water_files){

    # read in gage
    gage <- read.csv(every, header = F)

    # Get the Gage ID
    gage_id <- gage$V2[1]

    # Correct stripper zeros
    if (nchar(gage_id) == 7){
      gage_id <- paste0("0",as.character(gage_id))
    }

    # Check if gage is in contiguous US
    try({

      # date as date
      gage$V3 <- as.Date(gage$V3)

      # Replace -999999 and zero values with NA then omit
      gage[na.omit(gage$V4) <= 0,"V4"] <- NA
      gage[na.omit(gage$V4) == 0,"V4"] <- NA
      gage <- na.omit(gage)

      # Detrend the discharge data
      detrended <- detrend(gage$V3,gage$V4)

      # add to dataframe
      gage$Detrended <- detrended[[1]]

      # day 7 moving average
      gage$day7 <- moving_average(gage$Detrended,7)

      # day 15 moving average
      gage$day15 <- moving_average(gage$Detrended,15)

      # append gage to list
      gage_list <- append(gage_list, gage_id)

      # append slope to list
      slope_list <- append(slope_list, detrended[[2]][1])

      # save the file
      write.csv(gage, paste0(detrended_directory, gage_id, ".csv"), row.names = F)
    })
  }
  # Create the slope df
  slope_df <- as.data.frame(gage_list)

  # add the slopes
  slope_df$slope <- slope_list

  # write out the csv
  write.csv(slope_df, paste0(project_directory, "slope_df.csv"), row.names = F)

  # return a list of the gages used
  return(gage_list)
}

