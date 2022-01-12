#' Functions to set some options that look OK when using the huxtable package
#'
#' \code{huxoptions} for any huxtable tidy table object thing
#' \code{huxreg_opts} for any huxtable regression tan;es
#'
#' @details used in engagement_20
#' @details requires dplyr and huxtable packages
#'
#' @export

huxoptions <- function(df) {
  as_hux(df) %>% set_bold(everywhere, 1, TRUE) %>% # set_background_color(where(. < 0.1), 'grey') %>%
  set_all_borders(1) %>% huxtable::add_colnames() %>% set_number_format(3)
}

#' @export
huxreg_opts  <- function(df) {
 df %>%
    set_bold(1, everywhere)             %>%
    set_bottom_border(1, everywhere) %>%
    map_background_color(by_rows("grey87", "white"))  %>%
    set_caption_pos("bottom") %>%
    set_col_width(c(1.8, rep(.6, times=length(.)-1)))
}
