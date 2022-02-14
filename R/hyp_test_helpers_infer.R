#' Hypothesis test helpers for working with `infer` package
#' \code{extract_hyp_value} is a function to switch hypothesis statistic names to value for standardized output. It is mainly used in \code{extract_hyp_results}. 
#'
#' @details  Don't run for independence (this doesn't specify a statistic value)

#' @export
extract_hyp_value <- function(hypothesis){
  
  if (!is.na(hypothesis['null']) & hypothesis['null'] == "independence"){
    return(hypothesis)
  }
  else{
    x <- names(hypothesis)
    
    for (i in seq_along(x)){
      name <- x[i]
      
      # If name = mu|med|p change it to null_value
      switch_name <- switch(name,
                            mu =, med =, p = "null_value")
      
      if (!is.null(switch_name))
        x[i] <- switch_name
    }
    names(hypothesis) <- x
    return(hypothesis)
  }
  
}

#' \code{extract_hyp_results} is a function to extract key results from hypothesis testing using the infer package
#
#'
#' @details  Don't run for independence (this doesn't specify a statistic value) ... 
#'   Unnests results
#' Changes name of mu, med, p to "null_value"
#'  Displays formulas in text form


#' @examples
#' See 'donations_sparse.Rmd'
#' linked_tests_results <- extract_hyp_results(linked_tests_df) %>% # extract and label key results for reporting and plotting mutat
#' (data_label = linked_df_labels,
#'     data_type = linked_test_var_type)
#'     
#' @export

extract_hyp_results <- function(df,
                                remove = c("df")){
  
  
  df <- df %>% dplyr::mutate(hypothesis = map(hypothesis,
                                              extract_hyp_value))
  
  df <- df %>% tidyr::unnest_wider(hypothesis) %>%
    tidyr::unnest_wider(results) %>%
    unnest_wider(stat) %>%
    unnest_wider(gen)
  
  if ("ci" %in% names(df)){
    df <- df %>% unnest_wider(ci)
  }
  
  df <- df %>% mutate(across(matches("formula"), ~ format(.x)))
  
  df <- df %>% dplyr::select(-contains(remove))
  return(df)
}

