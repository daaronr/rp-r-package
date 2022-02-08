#' A set of functions to 'adorn' and format janitor::tabyl() table objects
#'
#' \code{tabylstuff} is a function that puts totals for row and column, formats rows with 1 digit percentages, adds "N" counts, makes it a kable with a caption, and styles the kable
#' \code{tabylstuff_nocol} is like tabylstuff but without column totals
#' \code{adornme} adds a title and does some other things differently, but I'm not sure what. This may be mostly redundant
#' \code{adornme_not} is adornme without a title
#'
#' @param df is a 'tabyl' style tibble
#' @param cap is a caption text (quoted string)
#' @param digits (in adornme and adornme_not) lets you specify the digits in the percentage
#' @param adorn  (in adornme and adornme_not) lets you specify whether row or column will be made into percentages ... not sure this actually makes sense!
#'
#'
#' @details XXX This function does nothing. It relies on no packages
#'
#' @examples
#'  XXX nothing(mtcars)

#' @export
tabylstuff <- function(df, cap = "") {
  adorn_totals(df, c("row", "col")) %>% adorn_percentages("row") %>%
    adorn_pct_formatting(digits = 1) %>% adorn_ns() %>% kable(caption = cap) %>%
    kable_styling(latex_options = "scale_down")
}

#' @export
tabylstuff_nocol <- function(df, cap=""){
  adorn_totals(df, c("row")) %>%
    adorn_percentages("row") %>%
    adorn_pct_formatting(digits = 1) %>%
    adorn_ns() %>%
    kable(caption=cap) %>%
    kable_styling(latex_options = "scale_down")
}

#' @export
adornme <- function(atabyl, adorn = "row", digits = 2, cap = "",
                    title = "") {
  atabyl %>% adorn_totals("row") %>% #
    adorn_percentages(adorn) %>% adorn_pct_formatting(digits = digits) %>%
    adorn_ns() %>% adorn_title(title, placement = "top") %>%
    kable(caption = cap) %>% kable_styling()
}

#' @export
adornme_not <- function(atabyl, adorn = "row", digits = 2, cap = "") {
  atabyl %>% adorn_totals("row") %>%
    adorn_percentages(adorn) %>% adorn_pct_formatting(digits = digits) %>%
    adorn_ns() %>%
    kable(caption = cap) %>% kable_styling()
}


