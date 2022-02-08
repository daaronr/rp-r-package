#' simple tables and tabsum ####
#'
#' \code{tabyl_ow}  makes a one-way 'tabyl' style table, arranged in descending order of frequency, not showing missing levels, and making it actually percents
#' \code{tabyl_ow_plus} does the same as tabyl_ow, but also kables and styles it
#' \code{tabylme} is a function that makes a two-way 'tabyl' style table, adding row percentages, and kabling and styling it

#'
#' @param var is the column to tabulate
#' @param caption can contain text
#'
#'
#' @details You need the janitor package, dplyr, and kableExtra
#'
#' @examples
#'  tabyl_ow_plus(iris, Species, caption = "iris species or something")

#' @export

tabyl_ow <- function(df, var) {
    df %>%
    tabyl({{var}}, show_missing_levels = FALSE) %>%
        mutate(percent = 100*percent) %>%
        dplyr::arrange(desc(n))
}

#' @export

tabyl_ow_plus <- function(df, var, caption=NULL, title_case = FALSE) {
  df <- {{df}} %>%
    tabyl({{var}}, show_missing_levels = FALSE) %>%
    mutate(percent = 100*percent) %>%
    dplyr::arrange(desc(n)) %>%
    adorn_totals()

  if (title_case == TRUE){
    df <- df %>% rename_with(snakecase::to_title_case)
  }
  df %>%
    kable(caption = caption, padding=0) %>%
    kable_styling()
}


#' @export

tabylme <- function(df = ADSX, rowvar = TreatFirstAsk, colvar = treat_second_ask,
  adorn = "row") {
    {{df}} %>%
  tabyl({{rowvar}}, {{colvar}}) %>% adorn_percentages({{adorn}}) %>%
    adorn_pct_formatting(digits = 2) %>% adorn_ns() %>% kable() %>%
    kable_styling()
}

