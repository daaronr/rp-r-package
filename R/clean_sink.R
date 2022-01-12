#' Puts together a bunch of common 'recipe' steps used in ML and other contexts
#'
#' @details Not used in EAS code
#'

#' @export


clean_sink <- function(df) {
  require("dplyr")
  step_meanimpute(all_numeric(), -all_outcomes()) %>%
      step_knnimpute(all_nominal()) %>%
    step_center(all_numeric(), -all_outcomes()) %>%
    step_scale(all_numeric(),
    -all_outcomes()) %>%
    step_other(all_nominal())
}
