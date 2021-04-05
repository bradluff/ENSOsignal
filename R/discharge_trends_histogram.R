#' Histogram of the slopes of each gage's trend in discharge.
#'
#' @param slope_df Dataframe of the slopes associated with each gage's trend in discharge.



plot_slopes <- function(slope_df){

  # extract all slopes greater than -1?
  slope_list <- slope_df[slope_df$slope > -1,2]

  # histogram of the slopes
  hist(slope_list, xlab = "Slope", main = "Slopes of 60 year climate record for gages")
}
