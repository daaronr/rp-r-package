#' make a dataframe for modeling
#' \code{make_model_df} makes a dataframe with outcome variable, model formula and associated dataframe

#'
#'
#'
#' @details  (uses make_formula (which we just moved to the rethinkpriorities package)
#'
#' @examples
#' qp_models <- make_model_df(qp_rhs_vars, qp_outcome_vars, qp_dfs) # Create dataframe for models

#' @export





make_model_df <- function(indep_vars, outcome_vars, dfs){

  # Make all associated formulas
  formulas <- mapply(function(x, y) make_formula(x, y), x = outcome_vars, y = indep_vars)

  tibble(outcome = as.character(outcome_vars),
         formulas = formulas,
         dfs = dfs)
}
