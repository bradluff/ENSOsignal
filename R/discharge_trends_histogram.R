#' Histogram of the slopes of each gage's trend in discharge.
#'
#' @param slope_df Dataframe of the slopes associated with each gage's trend in discharge.
#' @export detrend



plot_slopes <- function(slope_df){
  slope_list <- slope_df[slope_df$slope>-1,2]
  hist(slope_list, xlab = "Slope", main = "Slopes of 60 year climate record for gages")
}
