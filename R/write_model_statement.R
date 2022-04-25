#' Create and write a model statement to a file
#'
#' This function takes a lavaan model object, converts it to a model 
#' statement, and saves the result to a text file. This text file can be used
#' to calculate dynamic fit indices via https://www.dynamicfit.app/connect/.
#'
#' @export
write_model_statement <- function(x, file) {
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop(
      "Package \"lavaan\" must be installed to use this function.",
      call. = FALSE
    )
  }
  
  x %>%
    lavaan::standardizedsolution() %>%
    dplyr::select(lhs, op, rhs, est.std) %>%
    dplyr::filter(lhs != rhs) %>%
    tidyr::unite(col = "parameter", est.std, rhs, sep = " * ") %>%
    dplyr::mutate(
      op = dplyr::if_else(dplyr::lag(lhs, default = "") == lhs, "+", op),
      lhs = dplyr::if_else(dplyr::lag(lhs, default = "") == lhs, "", lhs),
      parameter = dplyr::if_else(
        stringr::str_detect(dplyr::lead(op, default = "~"), "~"), 
        paste0(parameter, "\n"), parameter)
    ) %>% 
    tidyr::unite(col = "x", lhs, op, parameter, sep = " ") %>%
    unlist() %>%
    paste(collapse = "") %>%
    write(file = file)
}
