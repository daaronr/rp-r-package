#' Calculate the SE of a bin
#'
#' \code{se_bin} calculates the SE of bla bla.
#'
#' @param x A vector that bla bla.
#' 
#' @examples
#' # Here is an example
#' x <- c(0:10, 50)
#' se_bin(x)
#'
#' @export
se_bin <- function(x) {
  x = as.numeric(x)
  n = length(x)
  m = mean(x, na.rm=TRUE)
  se = sqrt((m*(1-m))/n)
  
  return(se)
}
