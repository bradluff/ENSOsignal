#' Calculate the annual discharge, the average daily discharge, the 7 day minimum discharge, and the 15 day maximum discharge.
#'
#' @param start_year The first year of data to be used.
#' @param end_year The last year of data to be used.
#' @param detrended_directory The directory where the detrended discharge files are located.
#' @param project_directory The directory used to save output files.
#' @export detrend



discharge_parameters <- function(start_year = 1951, end_year = 2011, detrended_directory, project_directory){
  # sequence years
  water_years <- seq(start_year+1, end_year, by = 1)
  # year starts
  year_start <- "-10-01"
  # year ends
  year_end <- "-09-30"
  # gather file names
  water_files <- list.files(detrended_directory, full.names = T)
  # dataframes
  mean_df <- as.data.frame(matrix(nrow = length(water_files), ncol = length(water_years)))
  sum_df <- as.data.frame(matrix(nrow = length(water_files), ncol = length(water_years)))
  max_df <- as.data.frame(matrix(nrow = length(water_files), ncol = length(water_years)))
  min_df <- as.data.frame(matrix(nrow = length(water_files), ncol = length(water_years)))
  maxdate_df <- as.data.frame(matrix(nrow = length(water_files), ncol = length(water_years)))
  mindate_df <- as.data.frame(matrix(nrow = length(water_files), ncol = length(water_years)))

  count <- 1
  gage_list <- c()
  # Loop through every file
  for (every in water_files){
    # Open file
    gage <- read.csv(every)
    # date as date
    gage$V3 <- as.Date(gage$V3)
    # lists to append to
    mean_list <- c()
    sum_list <- c()
    max_list <- c()
    min_list <- c()
    max_date <- c()
    min_date <- c()
    # loop through every water year
    for (year in water_years){
      # subset the data
      this_year <- gage[gage$V3 >= paste0(as.character(year-1),year_start) & gage$V3 <= paste0(as.character(year),year_end),]
      # append mean
      mean_list <- append(mean_list, mean(na.omit(this_year$Detrended)))
      # append sum
      sum_list <- append(sum_list, sum(na.omit(this_year$Detrended)))
      # append max15
      max_list <- append(max_list, max(na.omit(this_year$day15)))
      # append min7
      min_list <- append(min_list, min(na.omit(this_year$day7)))
      # append date of max event
      max_date <- append(max_date, this_year$V3[match(max(na.omit(this_year$day15)), this_year$day15)])
      # append date of min event
      min_date <- append(min_date, this_year$V3[match(min(na.omit(this_year$day7)), this_year$day7)])
    }
    gage_list <- append(gage_list, gage$V2[1])
    mean_df[count,] <- mean_list
    sum_df[count,] <- sum_list
    max_df[count,] <- max_list
    min_df[count,] <- min_list
    maxdate_df[count,] <- max_date
    mindate_df[count,] <- min_date
    count <- count + 1
    print(gage$V2[1])
  }

  for (column in colnames(maxdate_df)){
    maxdate_df[,column] <- as.character(format(as.Date(maxdate_df[,column], origin = "1970-01-01"), "%m-%d"))
  }
  for (column in colnames(mindate_df)){
    mindate_df[,column] <- as.character(format(as.Date(mindate_df[,column], origin = "1970-01-01"), "%m-%d"))
  }

  colnames(sum_df) <- water_years
  rownames(sum_df) <- gage_list
  colnames(mean_df) <- water_years
  rownames(mean_df) <- gage_list
  colnames(max_df) <- water_years
  rownames(max_df) <- gage_list
  colnames(min_df) <- water_years
  rownames(min_df) <- gage_list
  colnames(maxdate_df) <- water_years-1
  rownames(maxdate_df) <- gage_list
  colnames(mindate_df) <- water_years-1
  rownames(mindate_df) <- gage_list

  write.csv(sum_df, paste0(project_directory, "sum_df.csv"), row.names = T)
  write.csv(mean_df, paste0(project_directory, "mean_df.csv"), row.names = T)
  write.csv(max_df, paste0(project_directory, "max_df.csv"), row.names = T)
  write.csv(min_df, paste0(project_directory, "min_df.csv"), row.names = T)
  write.csv(maxdate_df, paste0(project_directory, "maxdate_df.csv"), row.names = T)
  write.csv(mindate_df, paste0(project_directory, "mindate_df.csv"), row.names = T)

}



