#' Correlate the z-scored values to the historic oni-intensity.
#'
#' @param gage_list The gages used in vector form.
#' @param project_directory The directory used to save output files.
#' @return A dataframe of correlated values using the sum, min, max, and mean z-scored values.
#' @export detrend



correlate <- function(gage_list, project_directory){
  oni_intensity <- c(0.608333333,0.35,0.375,-0.733333333,-0.925,0.008333333,1.291666667,0.416666667,-0.075,0.133333333,-0.2,-0.033333333,
                     0.625,-0.333333333,1.266666667,-0.2,-0.3,0.716666667,0.416666667,-0.916666667,-0.35,1.008333333,-1.4,-0.633333333,-1.058333333,
                     0.5,0.341666667,-0.066666667,0.408333333,-0.166666667,0.091666667,1.55,-0.466666667,-0.675,-0.333333333,0.925,0.633333333,
                     -1.225,0.016666667,0.375,1.108333333,0.225,0.208333333,0.566666667,-0.65,-0.083333333,1.633333333,-1.216666667,
                     -1.183333333,-0.491666667,0,0.641666667,0.275,0.525,-0.358333333,0.291666667,-1.108333333,-0.366666667,0.75,-1.15)
  water_years <- seq(1952, 2011, by = 1)
  sum_z <- read.csv(paste0(project_directory, "sum_z.csv"))
  min_z <- read.csv(paste0(project_directory, "min_z.csv"))
  max_z <- read.csv(paste0(project_directory, "max_z.csv"))
  mean_z <- read.csv(paste0(project_directory, "mean_z.csv"))
  colnames(sum_z) <- as.character(water_years-1)
  colnames(min_z) <- as.character(water_years-1)
  colnames(max_z) <- as.character(water_years-1)
  colnames(mean_z) <- as.character(water_years-1)
  sum_cor <- c()
  for (row_i in 1:length(sum_z$`1951`)){
    sum_cor <- append(sum_cor, cor(as.numeric(sum_z[row_i,]), oni_intensity))
  }
  mean_cor <- c()
  for (row_i in 1:length(mean_z$`1951`)){
    mean_cor <- append(mean_cor, cor(as.numeric(mean_z[row_i,]), oni_intensity))
  }
  min_cor <- c()
  for (row_i in 1:length(min_z$`1951`)){
    min_cor <- append(min_cor, cor(as.numeric(min_z[row_i,]), oni_intensity))
  }
  max_cor <- c()
  for (row_i in 1:length(max_z$`1951`)){
    max_cor <- append(max_cor, cor(as.numeric(max_z[row_i,]), oni_intensity))
  }
  hist(sum_cor, xlab = "Correlation", main = "Correlation of Sum Discharge to ONI")
  hist(mean_cor, xlab = "Correlation", main = "Correlation of Mean Discharge to ONI")
  hist(min_cor, xlab = "Correlation", main = "Correlation of 7-day Minimum Discharge to ONI")
  hist(max_cor, xlab = "Correlation", main = "Correlation of 15-day Maximum Discharge to ONI")
  correlation_df <- as.data.frame(cbind(gage_list,sum_cor,mean_cor,min_cor,max_cor))
  # Name columns
  colnames(correlation_df) <- c("gage","sum","mean","min","max")
  # write out effects df
  write.csv(correlation_df, paste0(project_directory, "correlation_df.csv"), row.names = F)
  return(correlation_df)
}
