#' Make sure all gage IDs have leading zeroes.
#'
#' @param start_year The first year of data to be used.
#' @param end_year The last year of data to be used.
#' @param project_directory The directory used to save output files.
#' @param detrended_directory The directory where the detrended discharge files are located.
#' @export detrend



standardize_gages <- function(start_year = 1951, end_year = 2011, project_directory, detrended_directory){
  water_years <- seq(start_year+1, end_year, by = 1)
  # dataframes
  sum_df <- read.csv(paste0(project_directory, "sum_df.csv"), row.names = 1)
  mean_df <- read.csv(paste0(project_directory, "mean_df.csv"), row.names = 1)
  max_df <- read.csv(paste0(project_directory, "max_df.csv"), row.names = 1)
  min_df <- read.csv(paste0(project_directory, "min_df.csv"), row.names = 1)
  sum_z <- as.data.frame(matrix(nrow = length(list.files(detrended_directory)), ncol = length(water_years)))
  mean_z <- as.data.frame(matrix(nrow = length(list.files(detrended_directory)), ncol = length(water_years)))
  max_z <- as.data.frame(matrix(nrow = length(list.files(detrended_directory)), ncol = length(water_years)))
  min_z <- as.data.frame(matrix(nrow = length(list.files(detrended_directory)), ncol = length(water_years)))
  # Z score all values
  for (i in 1:length(sum_z$V1)){
    sum_z[i,] <- scale(as.numeric(sum_df[i,]))
  }
  for (i in 1:length(mean_z$V1)){
    mean_z[i,] <- scale(as.numeric(mean_df[i,]))
  }
  for (i in 1:length(max_z$V1)){
    max_z[i,] <- scale(as.numeric(max_df[i,]))
  }
  for (i in 1:length(min_z$V1)){
    min_z[i,] <- scale(as.numeric(min_df[i,]))
  }
  # Rename the columns
  colnames(sum_z) <- as.character(water_years-1)
  colnames(mean_z) <- as.character(water_years-1)
  colnames(max_z) <- as.character(water_years-1)
  colnames(min_z) <- as.character(water_years-1)
  # write out z-score tables
  write.csv(sum_z, paste0(project_directory, "sum_z.csv"), row.names = F)
  write.csv(mean_z, paste0(project_directory, "mean_z.csv"), row.names = F)
  write.csv(max_z, paste0(project_directory, "max_z.csv"), row.names = F)
  write.csv(min_z, paste0(project_directory, "min_z.csv"), row.names = F)
}
