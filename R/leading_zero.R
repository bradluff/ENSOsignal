#' Make sure all gage IDs have leading zeroes.
#'
#' @param gage_list A vector of the gages used.



leading_zero <- function(gage_list){

  # empty vector
  new_list <- c()

  # loop through every gage
  for (every in gage_list){

    # if length is 7 add a leading 0
    if (nchar(every) == 7){
      every1 <- paste0("0",as.character(every))
    } else {
      every1 <- every
    }

    # add the value changed or unchanged to the list
    new_list <- append(new_list, every1)
  }

  # return the list of adjust gage names
  return(new_list)
}
