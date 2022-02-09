#' Functions to deal with lists and text

#'
#' \code{lab_list_to_text} turns a labled list into text, mainly used for inline code
#'
#'
#' @details requires labelled package
#'
#' @examples
#'  Demographics: `r eas_all_s %>% select(all_of(key_demog)) %>% lab_list_to_text`

#' @export


lab_list_to_text <- function(df) {
  df %>%
    var_label %>% unname %>% unlist() %>%
    paste(collapse = ', ')
}


#' \code{remove_str_list} -- not sure what it does
#' @export

remove_str_list <- function(list, string){
  list <- Filter(function(x) !any(grepl(string, x)), list)
  return(list)
}
