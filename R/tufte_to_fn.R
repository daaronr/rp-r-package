#' Parse a text file; Convert Reinstein's adaptation of tufte notes into standard markdown footnotes
#'
#' \code{wdwd} is a function that...
#'
#' @filename is the name of the (Rmd, Qmd etc) file that will be edited
#'
#' @examples
#' ch_Rmds <- purrr::map(ch_Rmd, tufte_to_footnote)

#'
#' @export


tufte_to_footnote <- function(filename) {
  reg_mn_div <- rex(zero_or_more("\\*"),
                    zero_or_more(any_spaces),
                    '<div class="marginnote">',
                    zero_or_more(any_spaces),
                    zero_or_more("\\*"),
                    capture(one_or_more(anything, type="lazy")),
                    '</div>')

reg_mn_col_to_fn <- rex(zero_or_more("\\*"),
                          zero_or_more(any_spaces),
                          '::: {.marginnote}',
                          zero_or_more(any_spaces),
                          zero_or_more("\\*"),
                          capture(one_or_more(anything, type="lazy")), ':::')

  # make into footnotes
  ch_ed <- filename %>%
    gsub(reg_mn_div, '^[\\1]', .)  %>%
    gsub(reg_mn_col_to_fn, '^[\\1]', .)

  return(ch_ed)
}


