#' Calculate the SE of a bin
#'
#' \code{se_bin} calculates the SE of input vector
#'
#' @param x A vector of binary values that is numeric or can be coerced to be numeric.
#'
#' @examples
#' # Here is an example
#' x <- sample(x = c(0, 1),  size = 10, replace = TRUE)
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
