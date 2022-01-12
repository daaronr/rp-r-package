#' Grouped summaries, listing of unique groups
#'
#' \code{grp_n} counts nonmissing observations of each column by group
#'
#' @examples
#'  grp_n(mtcars, cyl)
#' @export

grp_n <- function (df, groupvar) {
df %>%
  group_by({{groupvar}}) %>%
  summarise(across(.cols = everything(),
                   .fns = list(n = ~ sum(!is.na(.x)))
  )
  )
}

#' \code{grp_uniq} counts nonmissing *distinct* observations of each column by group
#'
#' @examples
#'  grp_n(mtcars, cyl)


#' @export
grp_uniq <- function (df, groupvar) {
df %>%
  group_by({{groupvar}}) %>%
  summarise(across(.cols = everything(),
                   .fns = list(uniq = ~ n_distinct(.x))
    )
  )
}
