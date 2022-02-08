#' Converts 'one-hot' Dummies to factors
#'
#' \code{convert_one_hot_back} is a function that...
#'
#' @param XXX describe an input to function
#'
#'
#' @details I think I got it from https://stackoverflow.com/questions/64230674/how-to-turn-one-hot-encoded-variables-to-a-single-factor-in-r
#'
#' @examples
#'   dat <- data.frame(season = as.factor(sample(1:4, 100, replace = TRUE))) 
#'   dat2 <- model.matrix(~ season, dat)
#'   convert_one_hot_back(as_tibble(dat2), "season", c("season2", "season3", "season4")) %>% head()

#' @export

convert_one_hot_back <- function(df, prefix, vars, keep_dummies=FALSE) {
  df_dum <- df %>% dplyr::select(starts_with(prefix))
  df_dum[[prefix]] <- NA
  for (var in vars) {
    idx <- df_dum[[var]] == 1
    df_dum[[prefix]][idx] <- gsub(prefix,'', var)
  }
  if (!keep_dummies) {
    df_dum <- df_dum %>% select(prefix)
  }
  return(cbind(df %>% dplyr::select(-vars), df_dum ))
}
