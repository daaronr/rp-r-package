#'  Make a model into a tidy object 
#'
#' \code{extract_coefficients} is a function that extracts (and rename) coefficients and CIs (from a list/tibble including columns 'term' and ??)
#'
#' @param XXX describe an input to function
#'
#'
#' @details Need to add more details here. Note that robust sandwich se's are defaults

#'

#' @export


extract_coefficients <- function(fit, replacement_names = NULL, model_name = NULL,
  robust_SE = TRUE, exponentiate=FALSE){
  
  # Included here as broom methods often don't include options for exponentiate
  expo <- function(data) {
    data <- mutate_at(data, vars(estimate), exp)
    
    if ("conf.low" %in% colnames(data)) {
      data <- dplyr::mutate_at(data, vars(conf.low, conf.high), exp)
    }
    
    data
  }
  
  if (robust_SE){
    # Calculate robust standard errors using default ("best") HC3
    tidy_fit <- lmtest::coeftest(fit, vcov = sandwich::vcovHC(fit)) %>%
      broom::tidy(conf.int=TRUE)
    
  }
  else{
    tidy_fit <- fit %>%
      broom::tidy(conf.int=TRUE)
  }
  
  if (exponentiate){
    tidy_fit <- expo(tidy_fit)
  }
  
  # Rename terms if names were specified
  if (!is.null(replacement_names)) {
    tidy_fit <- tidy_fit %>%
      mutate(term = str_replace_all(term, replacement_names))
  }
  if (!is.null(model_name)) {
    tidy_fit <- tidy_fit %>%
      mutate(model_names = model_name)
  }
  
  return(tidy_fit)
  
}