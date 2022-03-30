#'  Fit linear (regression) model and return fit (not tidied, just regular)

#'
#' \code{fit_glm} is like fit_lm but for a glm I guess ...  a function that fits a general linear regression model and returns a 'model frame', and can keep the model matrix, etc. 
 
#'
#' @param keep_data whether to return the model frame?
#' @param keep_model_matrix whether to keep the model matrix
#' @param qr whether to keep the 'QR decomposition', a factorization of the (?) model matrix
#'
#'
#'
#' @examples
#'  linear_models <- linear_models %>%
#' mutate(
#'  lm_fit = fit_models(
#'    linear_models, "formulas", "dfs", fun = fit_lm)
#')

#' @export



fit_glm <- function(formula, df, family = quasipoisson,
  replacement_names=NULL,
  keep_data = TRUE,
  y = TRUE,
  qr = TRUE,
  keep_model_matrix=FALSE){
  
  fit <- glm(formula, data = df,
    model = keep_data,
    x = keep_model_matrix,
    family = family,
    y = y)
  
  return(fit)
}