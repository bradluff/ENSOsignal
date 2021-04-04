#' Correlate the z-scored values to the historic oni-intensity.
#'
#' @param gage_list The gages used in vector form.
#' @param project_directory The directory used to save output files.
#' @return A dataframe of correlated values using the sum, min, max, and mean z-scored values.
#' @export detrend



correlate <- function(gage_list, project_directory){
  oni_intensity <- c(0.041666667,0.4,0.525,0,-0.75,-0.858333333,0.475,1.083333333,0.25,0.025,0.058333333,-0.191666667,
                     0.25,0.183333333,0.225,0.9,-0.258333333,-0.133333333,0.733333333,0.091666667,-0.933333333,0.191666667,
                     0.341666667,-1.183333333,-0.833333333,-0.65,0.516666667,0.125,0.075,0.383333333,-0.241666667,0.441666667,
                     1.233333333,-0.5,-0.733333333,-0.108333333,1.216666667,-0.066666667,-0.991666667,0.175,
                     0.441666667,0.983333333,0.258333333,0.258333333,0.325,-0.608333333,0.466666667,0.9,-1.233333333,
                     -1.033333333,-0.391666667,0.258333333,0.466666667,0.375,0.35,-0.3,-0.025,-1,-0.175,0.258333333)
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
