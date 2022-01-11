#' Shortcut tool for altering a function 
#'  
#'
#' @param FUN An already defined function
#' 
#' @examples
#' # Here is an example
#' .median <- hijack(stats::median, na.rm = TRUE)
#'
#' @details Taken from  https://www.r-bloggers.com/hijacking-r-functions-changing-default-arguments/

#' 
#' @export

hijack <- function (FUN, ...) {
  .FUN <- FUN
  args <- list(...)
  invisible(lapply(seq_along(args), function(i) {
    formals(.FUN)[[names(args)[i]]] <<- args[[i]]
  }))
  .FUN
}

