#' sumtab but without removing NAs, no kable and adding a few rows
#'
#' @examples
#'  sumtab0(movies_long, budget, genre)

#' @export

sumtab0 <- function(df, depvar, treatvar, caption = "", digits=3, label = TRUE) {
  require(dplyr, kableExtra)
  df %>%
    dplyr::ungroup() %>%
    group_by({{treatvar}}) %>%
    dplyr::summarize(N = n(),
                     `share > 0` = sum({{depvar}} >0)/n(),
                     `share missing` = sum(is.na({{depvar}})) / n(),
                     #share_10 = sum({{depvar}}== 10)/n(),
                     Mean = round(mean({{depvar}}, na.rm = T), 2),
                     Median = round(median({{depvar}}, na.rm = T),2),
                     P80 = round(quantile({{depvar}}, 0.8, na.rm = T), 2),
                     P99 = round(quantile({{depvar}}, 0.99, na.rm = T), 2),
                     Std.dev. = glue::glue("(", {round(sd({{depvar}}, na.rm = T), 2) }, ")"))
   # kable(caption = caption, digits=digits, label = label) %>%
    #kable_styling()
}
