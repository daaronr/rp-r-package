#'  Rename a variable with it's label
#'
#'
#' @param df
#'
#' @details We haven't used this on EAS (yet) but it seems so useful to me that I think we should keep it in our package
#'
#' @examples
#' p_load(expss)
#' mtcars <- apply_labels(mtcars[,1:3] ,
#'                       mpg = "Miles/(US) gallon",
#'                       cyl = "Number of cylinders",
#'                       disp = "Displacement (cu.in.)",
#'                      )
#' rename_to_var_label(mtcars)

#' @export

rename_to_var_label <- function(df){
  # Extract variable label
  labels <- lapply(df, function(x) attributes(x)$label)
  assertthat::assert_that(!list(NULL) %in% labels,
                          msg = "Each column must have a corresponding label")
  # Set names of variables as their label
  names(df) <- unlist(labels)
  return(df)
}


