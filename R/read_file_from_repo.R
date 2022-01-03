#' Read a file from a (personal) GitHub repository
#'
#' \code{read_file_from_repo} reads a file from a GitHub repository.
#'
#' @param repo A character string specifying the GitHub repository.
#' @param path A character string specifying the file path in the GitHub 
#' repository.
#' @param token_key A character string specifying the key to the GitHub personal 
#' access token (PAT), stored using the \code(keyring) package.
#' @param user A character string specifying the GitHub user name. By default 
#' this is 'rethinkpriorities'.
#' @param private A boolean value indicating whether the GitHub repository is a 
#' private repository or not. The default is FALSE.
#' 
#' @details This function relies on the `keyring` package to retrieve a stored
#' GitHub Personal Access Token (PAT) to access private repositories.
#' 
#' @examples
#' \dontrun{
#' data <- read_file_from_repo(
#'   repo = "ea-data", path = "data/edited_data/sexual_orientation.csv", 
#'   token_key = "github-PAT", private = TRUE
#' )
#' }
#'
#' @export
read_file_from_repo <- function(repo, path, token_key, 
    user = "rethinkpriorities", private = FALSE) {
  
  # Extract the file name from the path
  file <- basename(path)
  
  # Construct the URL
  base <- "https://api.github.com/repos/"
  dir <- stringr::str_remove(path, file)
  url <- paste0(base, user, "/", repo, "/contents/", dir)
  
  # Obtain the authorization token if the repo is private
  if (private) {
    token <- tryCatch(
      expr = keyring::key_get(token_key), 
      error = function(e) {
        message(e)
        message("\n")
        message(paste("HINT: Use keyring::key_set() to set the token with the", "
          token key."))
      }
    )
    
    # GET the url with the authorization token
    results <- httr::GET(
      url = url, 
      httr::add_headers(Authorization = paste("token", token))
    )
  } else {
    # GET the url without the authorization token
    results <- httr::GET(
      url = url
    )
  }
  
  # Throw an error if the results are not found
  if (results$status_code == 404) {
    stop("Error: Not found.")
  }
  
  # Get the content
  content <- httr::content(results)
  
  # Find the download url to the file
  download_url <- unlist(purrr::map(content, 
    function(x) {
      if (x$name == file) {
        return(x$download_url)
      }
    }
  ))
  
  # Download the file
  results <- httr::GET(
    download_url, 
    httr::add_headers(Authorization = paste("token", token))
  )
  
  # Get the content
  content <- httr::content(results)
  
  # Get the file extension
  file_extension <- tools::file_ext(file)
  
  # Read the file depending on the file extension
  if (file_extension == "csv") {
    data <- readr::read_csv(httr::content(results))  
  } else if (file_extension == "Rdata") {
    connection <- gzcon(rawConnection(content))
    data <- readRDS(connection)
    close(connection)
  } else {
    error("File type not yet supported.")
  }
  
  return(data)
}
