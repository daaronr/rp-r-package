#' Case insensitive string_detect
#'
#' @export

#' @examples
#' num_savdon <- sum(str_det(eas_20$donate_later, "Yes"))

str_det <- function(string, pattern, negate = FALSE) {
  str_detect(string, regex(pattern, ignore_case = T))
}

