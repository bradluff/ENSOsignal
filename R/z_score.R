#' Z-score
#'
#' Z-score a value.
#'
#' @param value the value to z-score.
#' @param values the rest of the dataset to z-score against.
#'
#' @return the z-scored value
#' @export detrend



# the z-scoring function
z_score <- function(value, values){
  # suggest stats
  if (requireNamespace("stats", quietly = TRUE)) {
    # z-score a value and return it
    return(z_value <- (value-mean(values))/stats::sd(values))
  }
}
