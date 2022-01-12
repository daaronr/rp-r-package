#' Convenience 'hijacks' of kable and kable_styling to get formats I like
#'
#' \code{.kable_styling} ma
#'
#'
#' @examples
#'   mtcars %>%
#'      .kable(digits=2) %>%
#'     .kable_styling()


#' @export
.kable_styling <- hijack(kableExtra::kable_styling, full_width=FALSE)

#' @export
.kable <- hijack(knitr::kable, format.args = list(big.mark = ",", scientific = FALSE))
