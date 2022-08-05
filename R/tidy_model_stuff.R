#'  Extract best parameters etc from tidymodels workflow
#'
#' @details  Given a workflow extract the best parameters and  the corresponding workflow
#'
#' @examples
#'  Used in best_wflow_preds_vi

#' @export


get_best_result <- function(results_df, id, metric = "rmse"){

  # Given a workflow extract the best parameters
  best_params <-
    workflowsets::extract_workflow_set_result(results_df, id = id) %>%

    tune::select_best(metric = metric) #throws errors

  # Get the corresponding workflow
  best_workflow <- workflowsets::extract_workflow(results_df, id = id)

  dplyr::tibble(model = id,
                params = list(best_params),
                workflow = list(best_workflow))
}

#' Not sure what this does ... it is just a syntax helper for extract_fit_parsnip and vi

#'
#' @examples
#' this might be obsolete, I don't see it useful atm
#' best_models$fit[[1]] %>% get_var_importance()


#' @export


get_var_importance <- function(fit){
  extracted <- workflowsets::extract_fit_parsnip(fit)
  vip::vi(extracted)
}


#' Get "best" parameter sets for each modeling approach (RF, DT, GLM-EN), and also generating metrics of fit on training data
#' @details returns a tibble of the workflow ('output' is a 'fit workflow') with the best parameters, predictions, true outcomes, and some metrics
#'
#' @param classification is a logical to tell us if this is a classification model. in which case we want the ROC curve stuff
#' @param vi if we want variable importance measures
#' @param metric is our measure of fit, I guess, like rmse or roc_auc
#'
#' @examples
#' Used in best_wflow_preds_vi, also...
#' best_dt_preproc <- best_wflow_preds_vi(preproc_results, metric = 'roc_auc', classification = TRUE,
#'                                        outcome_var = "showup",
#'                                        train_sample = eas_all_train,
#'                                        test_sample = eas_all_test)

#' @export

best_wflow_preds_vi <- function(regression_workflow_results,
                                outcome_var,
                                classification = FALSE,
                                metric = "rmse",
                                train_sample = train,
                                test_sample = test,
                                vi = TRUE){


  if (classification){
    pred_type <- ".pred_class" #capturing how 'prediction' is named in classifications problems
  }
  else{
    pred_type <- ".pred"
  }

  # Extract best workflow, calculate predictions + variable importance for workflowsets

  # Extract the whole workflow for the models with the best parameters within each approach
  best_params <- purrr::map_df(regression_workflow_results$wflow_id,
                               ~get_best_result(regression_workflow_results,
                                                id = .x,
                                                metric = metric))

  # Finalize model on full train sample
  best_params <- best_params %>% dplyr::mutate(fit = purrr::map2(.x = workflow,
                                                                 .y = params,
                                                                 ~tune::finalize_workflow(.x, .y) %>%
                                                                   fit(train_sample)))

  # Compute predictions (on test sample)
  best_params <- best_params %>%
    dplyr::mutate(preds = purrr::map(.x = fit,
                                     ~predict(.x, test_sample) %>%
                                       dplyr::pull(!!pred_type)), # Convert predictions to vector
                  true_y = rep(list(test_sample[[outcome_var]]), nrow(best_params)))

  if (classification){
    # Get predicted probabilities for ROC curve
    best_params <- best_params %>% dplyr::mutate(pred_prob = purrr::map(.x = fit,
                                                                        ~predict(.x,
                                                                                 test_sample,
                                                                                 type = "prob")))
  }

  if (vi){
  # Calculate variable importance
  best_params <- best_params %>% dplyr::mutate(vi = purrr::map(.x = fit, ~get_var_importance(.x)))

  }
  return(best_params)

}

calculate_metrics <- function(df, metrics, preds = preds, true_y = true_y){
  # Calculate a list of metrics for the data

  df %>%
    mutate(purrr::map2_dfr({{true_y}}, {{preds}},
                           ~ purrr::map_dfc(metrics,
                                            do.call,
                                            list(.x, .y))))
}


# For scaling variable importance
scale_var <- function(x){
  scale(x)[,1]
}

