#' Replaces missing values with medians for column (ignoring NA's in computing median)
#'
#' @param x column of numeric values
#'
#' @examples
#' x <- c(0:10, 100, rep(NA,10), 1000, NA)
#' impute.med(x)
#'
#' @export

impute.med <- function(x) {
    base::replace(x, is.na(x), median(x, na.rm = TRUE))
}

