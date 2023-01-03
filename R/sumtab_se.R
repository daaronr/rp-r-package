#' A styled summary table for one outcome by another grouping variable. A lovely set of statistics. Now eith se
#'
#'
#' @details uses kable and kable_styling
#'
#' @examples
#'  sumtab_se(movies_long, budget, genre)
#'  airquality %>% mutate(ifelse(Ozonesumtab(airquality, Ozone, Month)

#' @export


sumtab_se <- function(df, depvar, treatvar, caption = "", digits=3, label = TRUE) {
  require(dplyr, kableExtra)
  df %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na({{depvar}})) %>%
    group_by({{treatvar}}) %>%
    dplyr::summarize(N = n(),
                     `share > 0` = sum({{depvar}} >0)/n(),
                     #share_10 = sum({{depvar}}== 10)/n(),
                     Mean = round(mean({{depvar}}, na.rm = T), 2),
                     SE = sd({{depvar}}, na.rm=T)/sqrt(sum(!is.na({{depvar}}))),
                     Median = round(median({{depvar}}, na.rm = T),2),
                     P80 = round(quantile({{depvar}}, 0.8, na.rm = T), 2),
                     # P99 = round(quantile({{depvar}}, 0.99, na.rm = T), 2),
                     Std.dev. = glue::glue("(", {round(sd({{depvar}}, na.rm = T), 2) }, ")")) %>%
    kable(caption = caption, digits=digits, label = label) %>%
    kable_styling()
}
