#' Some shortcut functions for mean, median, and sd,  removing missing values
#'
#' \code{mn} calculates the mean, removing missings
#' \code{med} calculates the mean, removing missings
#' \code{sdev} calculates the standard deviation, removing missings
#'
#' @param x A numeric vector
#'
#' @examples
#' 
#' x <- c(0:10, 100, rep(NA,10))
#' mn(x)
#' med(x)
#' sdev(x)
#' @export

mn <- function(x) {
    base::mean(x, na.rm=TRUE)
}

##' @export
med <- function(x) {
    stats::median(x, na.rm=TRUE)
}

##' @export
sdev <- function(x) {
    stats::sd(x, na.rm=TRUE)
}



