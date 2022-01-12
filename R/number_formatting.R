#' Number formatting options, mainly useful for inline code
#'
#' \code{op} default to 3-digit 'f' formatting with a comma, avoiding scientific notation
#'
#' @param x is something that produces a number
#' @examples
#'  we see that `r op(share_future_if_email*100)`% appear in a future EA survey.


#' @export
op <- function(x, d=3){
    format(x, format="f", big.mark=",", digits=d,
           scientific=FALSE)}

#' \code{ops} limits to 2 digits after decimal point
#' @examples
#'  the median percentage of income donated was `r ops(med_don_share_imp_bc*100)`% for `r year_n-1`.

#' @export

ops <- function(x, d=3, ns=2){
    format(x, format="f", big.mark=",", digits=d, nsmall=ns, scientific = FALSE)
options(scipen=999)
}

