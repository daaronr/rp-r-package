#' Fit models using columns specifying formulas and required dataframes;
#'
#' @details fit_lm as default modeling thing, but fit_glm, etc can also work
#'
#' @examples
#' # Create data frame for modeling
#' linear_models <- make_model_df(rhs_vars_list, outcome_vars_list, dfs)
#' Fit linear models
#' linear_models <- linear_models %>%
#'   mutate(
#'     lm_fit = fit_models(
#'     linear_models, "formulas", "dfs", fun = fit_lm)
#'   )
#'
#' @export

fit_models <- function(df, formulas_col, dfs_col, fun = fit_lm, family = NULL){

  assertthat::assert_that(is.character(formulas_col) & is.character(dfs_col),
                          msg = "Formulas and dfs column should be specified as strings")

  # Linear models don't take family as an argument
  if (is.null(family)){
    results <- map2(.x = pull(df, !!formulas_col),
                    .y = pull(df, !!dfs_col), ~ fun(.x, .y))
  }

  else{

  results <- map2(.x = pull(df, !!formulas_col),
                  .y = pull(df, !!dfs_col), ~ fun(.x, .y, family = family))
  }
  return(results)
}


