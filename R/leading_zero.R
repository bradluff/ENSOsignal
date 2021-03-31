#' Make sure all gage IDs have leading zeroes.
#'
#' @param gage_list A vector of the gages used.
#' @export detrend



leading_zero <- function(gage_list){
  new_list <- c()
  for (every in gage_list){
    if (nchar(every) == 7){
      every1 <- paste0("0",as.character(every))
    } else {
      every1 <- every
    }
    new_list <- append(new_list, every1)
  }
  return(new_list)
}
