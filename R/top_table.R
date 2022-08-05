#' top_table takes the name (outcol) of the top value of outvar in a tidy table, bot_table takes the bottom value
#'
#' @param outcol Is the name of the row of interest
#' @param outvar Is the column we want the top value of
#'
#' @details When you make a table using tabyl etc., you often want to find the 'top value row' to use in inline code
#'
#' @examples
#' 
#'  video_outcomes_phil <- gg_video_breakdowns %>%
#'     filter(audience=="Philanthropy") %>%
#'     group_by(video_theme) %>%
#'     base_results_sum() 
#' 
#' top_outcome <- video_outcomes_phil %>% top_table(video_theme, `$/ result`)


#' @export


top_table <- function(df, outcol, outvar) {
  df %>%
    dplyr::arrange(-{{outvar}}) %>%
    dplyr::select({{outcol}}) %>%
    mutate_if(is.factor, as.character) %>%
    .[[1,1]]
}

bot_table <- function(df,  outvar, outcol) {
  df %>% 
    dplyr::arrange({{outvar}}) %>%
    dplyr::select({{outcol}}) %>% 
    mutate_if(is.factor, as.character) %>%
    .[[1,1]] 
} 

