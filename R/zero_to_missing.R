#' Missing to zero and vice versa
#'
#' \code{zero_to_missing} turns 0 values into NA values in a data frame or vector
#' \code{missing_to_zero} turns NA values into 0 values in a data frame or vector
#'
#' @param x what inputs to function
#'
#' @examples
#' zero_to_missing(mtcars)
#' 
#' @export
zero_to_missing <- function(x){
  x[x == 0] <- NA
  return(x)
}

#' @export
missing_to_zero <- function (v)
{
  v[is.na(v) == TRUE] <- 0
  return(v)
}
