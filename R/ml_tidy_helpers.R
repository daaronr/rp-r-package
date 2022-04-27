#' Machine learning Tidymodels helpers
#'
#' \code{get_var_importance} is a function to get model fit and make a variable importance plot, I guess
#' \code{scale_var} is just a code shortcut for scaling
#' \code{norm_vi} prepares content for importance plots
#' \code{plot_vi} creqtes varln importance plots
#'
#' @param fit is a tune_results object, I think
#'
#' @details  \code{get_var_importance}  needs workflowsets and vip packages, part of the tidymodels ecosystem
#'
#' @examples
#'  best_models$fit[[1]] %>% get_var_importance()

#' @export

get_var_importance <- function(fit){
  extracted <- workflowsets::extract_fit_parsnip(fit)
  vip::vi(extracted)
}

#' @export

# For scaling variable importance
scale_var <- function(x){
  scale(x)[,1]
}

#' @export
norm_vi <- function(df, slice_top = 7){
  # Shortcut function for calculating normalized variable importance
  # Not reproducible...
  df %>% group_by(model) %>%
    mutate(Norm = scale_var(Importance)) %>%
    group_by(Variable) %>%
    mutate(Total_Norm = sum(Norm)) %>%
    group_by(model) %>%
    slice_max(Total_Norm, n = slice_top) %>%
    mutate(Variable = fct_reorder(Variable, Norm))
}

#' @export

plot_vi <- function(df, shapes = shape_colours){
  # Shortcut function for plotting normalized variable importance (output of norm_vi)
  df %>% ggplot(aes(y = Variable, x = Norm, colour = model, shape = Sign)) +
    scale_shape_manual(values = shapes) +
    geom_point(size = 4, stroke = 5) +
    xlab("Normalised feature importance") + ylab("")
}


