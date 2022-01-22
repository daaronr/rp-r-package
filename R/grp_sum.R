#' Grouped summary table of 2 variables, mainly for plotting bivariate means by groups

#' \code{grp_sum} creates a table of summary statistics on two variables, grouped by a third variable.  
#'
#' @param XXX describe an input to function
#'
#'
#' @details 
#'
#' @examples
#'  XXX nothing(mtcars)

#' @export


grp_sum <- function(df, xvar, yvar, groupvar) {
  df %>%
    dplyr::select({{xvar}}, {{yvar}}, {{groupvar}}) %>%
    group_by({{groupvar}}) %>%
    drop_na({{xvar}}, {{yvar}}, {{groupvar}}) %>%
    summarise(
      mn_y = mean({{yvar}}),
      mn_x = mean({{xvar}}),
      med_y = median({{yvar}}),
      med_x = median({{xvar}}),
      se_y = sd({{yvar}}, na.rm=TRUE)/sqrt(length({{yvar}})),
      se_x = sd({{xvar}}, na.rm=TRUE)/sqrt(length({{xvar}}))
    ) %>%
    group_by({{groupvar}}) %>%
    # Calculate confidence intervals
    mutate(
      lower = max(0, mn_y - 1.96*se_y),
      upper = mn_y + 1.96*se_y
    )
}
