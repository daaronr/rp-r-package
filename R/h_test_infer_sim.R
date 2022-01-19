#' helper for randomization-inference simulation testing using Infer package; set options in args
#'
#' \code{htest_infer_sim} works through the Infer package to  generate samples and a p-value from a 'null hypothesis distribution'. Written (mainly) by Oska Fentem, originally called "test_hypothesis"
#'
#' @param formula I'm not completely sure how to use a formula here, does it transform a variable?
#' @param response the outcome of interest
#' @param hypothesis -  If the hypothesis option stat is specified then statistics like the mean are calculated for this sample
#' @param gen something about how many samples are generated and how
#'
#' @param p_val_dir Direction for p-value calculation, if supplied
#' @param stat the statistic(s) (multiple?) to calculate?
#'
#' @details We used it for testing whether the median of a difference (input as a single column) is likely to be distinct from 0. 
#' Having this function enabled us to purrr map a bunch of stuff
#' Given a formula pr null hypothesis and arguments for generating samples:
#' Generate samples under this null
#'  If p_val_dir is supplied, then the function computes a p-value for observed data given the null
#'
#' @examples
#'
#' mtcarsX <- mtcars %>% mutate(gear_d_carb = gear - carb)
#' htest_infer_sim(df = mtcarsX,
#' stat = "median",
#' hypothesis = list(null = "point", med = 0),
#' gen =  list(reps = 1000, type = "bootstrap"),
#' response = "gear_d_carb",
#' p_val_dir = "two_sided")

#' @export

htest_infer_sim <- function(df, formula, response = NULL, hypothesis, gen,
                              stat = NULL, p_val_dir = NULL,
                            seed = 123) {
    if (!is.null(seed)){
    set.seed(seed)
  }

  # gtools::invalid counts NA, NaN and NULL
  if (!gtools::invalid(response)) {
    spec <- df %>% infer::specify(response = !!rlang::sym(response))
  }

  else {
    assertthat::assert_that(!is.null(formula),
                            msg = "Please specify a formula or response")
    formula <- as.formula(formula)
    spec <- df %>% specify(formula)
  }

  # Formulate hypothesis
  hypothesis$x <- spec
  hyp <- do.call(hypothesise, hypothesis)

  # Generate samples
  gen$x <- hyp
  samples <- do.call(generate, gen)

  if (!gtools::invalid(stat)){
  # Calculate sample point estimate
  # point_estimate <- infer::calculate(spec, stat = stat)$stat
  stat$x <- spec
  point_estimate <- do.call(infer::calculate, stat)$stat

  # Generate bootstrapped distribution for point estimate
  ## Here we generate samples without the null (using spec)
  boot <- gen
  gen$type <- "bootstrap"
  boot$x <- spec
  boot_dist <- do.call(infer::generate, gen)
  stat$x <- boot_dist
  boot_dist <- do.call(infer::calculate, stat)

  # Calculate confidence interval
  ci <- infer::get_confidence_interval(boot_dist,
                                       point_estimate = point_estimate,
                                       level = .95,
                                       type = "se")

  # Finalise null distribution calculation
  # null_dist <- infer::calculate(samples, stat = stat)
  stat$x <- samples
  null_dist <- do.call(infer::calculate, stat)
  results <- list(null_dist = null_dist,
                  point_estimate = point_estimate,
                  ci = ci)
  }

  else {
    results <- list(null_dist = samples)
  }

  if (!gtools::invalid(p_val_dir)){
    # Calculate and store p value
    p_val <- null_dist %>% infer::get_p_value(obs_stat = point_estimate, direction = p_val_dir)
    results$p_value <- p_val$p_value
  }

  return(results)
}
