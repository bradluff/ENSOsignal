#' Calculate the moving average of a set of numbers.
#'
#' @param dat A vector of data to create a moving average for.
#' @param avg_length The number of values to be averaged.



moving_average <- function(dat, avg_length){

  # divide the average by 2 to the closest integer
  num <- avg_length%/%2

  # empty vector
  end_values <- rep(NA, length(dat))

  # for every value find the moving average
  for (every in 1:length(dat)){
    try(end_values[every] <- mean(dat[(every-num):(every+num)]), silent = T)
  }

  # return the list of moving averages
  return(end_values)
}

