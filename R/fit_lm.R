#'  Fit linear (regression) model and return fit (not tidied, just regular)

#'
#' \code{fit_lm} is a function that fits a linear regression model and returns a 'model frame', and can keep the model matrix, etc. 
 
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
#'  lm_fit = fit_moels(
#'    linear_models, "formulas", "dfs", fun = fit_lm)
#')

#' @export

fit_lm <- function(formula, df,
  keep_data = TRUE, #return 'model frame'
  y = TRUE,
  qr = TRUE,
  keep_model_matrix=FALSE){
  
  fit <- lm(
    formula, data = df,
    model = keep_data, #whether to return 'model frame'
    x = keep_model_matrix, #whether to keep the model matrix
    y = y, #whether to keep the response
    qr = qr #whether to keep the 'QR decomposition', a factorization of the (?) model matrix
  )
  
  return(fit)
}