#' Making formulas
#
#' \code{m_f} simply collapses lists into a formula notation and makes it a formula
#' \code{make_formula} does something similar but in a more sophisticated way. using the 'reformulate' function. We use this in predictive modeling
#' \code{make_formula_df} attatches thi formula to a tibble (multiple) data frames?
#' \code{wdwd} is a function that...
#'
#'
#' @details Requires stats package
#'
#' @examples
#' t0_satis_vars <- c("age_approx", "d_male", "d_student", "not_just_white",  "d_live_usa", "years_involved_norm")
#' sat_outcome <- "as.factor(satisfaction_ideal)"
#' eas_20 %>%  MASS::polr(m_f(sat_outcome, t0_satis_vars), data = ., Hess=TRUE)
#'
#' @examples
#' control_vars <- c("ln_years_involved", "year_f", "age"
#' showup_f <- make_formula("showup", control_vars)
#'
#' @export

# @David: Repeating + renaming for clarity
make_formula <- function(lhs, rhs) {
    stats::reformulate(response = lhs, termlabels = rhs,
              env = globalenv()) # Formula is defined in the global environment
}


#' @export
# Shorter but less clear version -- remove?
m_f <- function(lhs, rhs) {
    stats::as.formula(paste(lhs," ~ ", paste(rhs, collapse= "+")))
}

#' @export
make_formula_df <- function(outcome_vars, indep_vars, dfs){
  # Make all associated formulas
  formulas <- mapply(function(x, y) make_formula(x, y), x = outcome_vars, y = indep_vars)

  tibble(outcome = as.character(outcome_vars),
         formulas = formulas,
         dfs = dfs)
}

