#' Detrend
#'
#' Perform a linear regression on the discharge to remove trends.
#'
#' @param x The date vector or column for the discharge data.
#' @param y The discharge values to be detrended.
#' @return A list of the detrended data and the linear regression model.



# the detrend function
detrend <- function(x,y){

  # linear regression model
  detrend_lm <- lm(y~x)

  # extract the mean
  mean_y <- mean(na.omit(y))

  # use the linear model and the mean to create a detrended dataset
  detrended <- y+(mean_y-((1:length(y)*detrend_lm[[1]][2])+detrend_lm[[1]][1]))

  # return the detrended data
  return(list(detrended, detrend_lm[[1]][2]))
}

