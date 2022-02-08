#' downloads from a url to a local path, without breaking if it fails
#'
#' \code{try_download} downloads from a URL to a local path. Often used for 'getting background stuff in bookdowns. Catches errors
#'
#'
#' @examples

#' try_download(
#' "https://raw.githubusercontent.com/daaronr/dr-rstuff/master/functions/project_setup.R",
#'  here::here("code", "project_setup.R")
#' )
#'

#' @export


try_download <- function(url, path) {
  new_path <- gsub("[.]", "X.", path)
  tryCatch({
    download.file(url = url,
                  destfile = new_path)
  }, error = function(e) {
    print("You are not online, so we can't download")
  })
  tryCatch(
    file.rename(new_path, path)
  )
}


