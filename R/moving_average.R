#' Calculate the moving average of a set of numbers.
#'
#' @param dat A vector of data to create a moving average for.
#' @param avg_length The number of values to be averaged.
#' @export detrend



moving_average <- function(dat, avg_length){
  num <- avg_length%/%2
  end_values <- rep(NA, length(dat))
  for (every in 1:length(dat)){
    try(end_values[every] <- mean(dat[(every-num):(every+num)]), silent = T)
  }
  return(end_values)
}

