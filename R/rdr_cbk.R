#'  Convenience function to make codebooks with options, particular file structure
#'
#' \code{wdwd} is a function that...
#'
#' @param cbfile name of 'codebook Rmd' being built
#'
#'
#' @details Be careful -- this only works if you have docs and codebooks folders in the right places
#' @details requires codebook package to be installed
#'
#' @examples
#' rdr_cbk("codebook_eas_all.Rmd")


#' @export


rdr_cbk <- function(cbfile) {
  #Convenience function to make codebooks with options
  rmarkdown::render(
    here("codebooks", cbfile),
    output_dir = here("docs", "codebooks"),
    intermediates_dir = here("docs", "codebooks"),
    knit_root_dir = here("docs", "codebooks")
  )
}


