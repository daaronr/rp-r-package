#' The Rethink Priorities bookdown output format
#'
#' @export
book <- function(
  fig_caption = TRUE, 
  number_sections = TRUE, 
  anchor_sections = TRUE, 
  lib_dir = 'libs', 
  pandoc_args = NULL, 
  extra_dependencies = NULL,
  hide_code = FALSE,
  ...
) {
  template <- system.file(
    "resources", "book.html", 
    package = 'rethinkpriorities', 
    mustWork = TRUE
  )
  
  config <- bookdown::html_chapters(
    toc = TRUE,
    number_sections = number_sections, 
    fig_caption = fig_caption,
    anchor_sections = anchor_sections,
    lib_dir = lib_dir, 
    theme = NULL,
    template = template,
    page_builder = build_page,
    split_bib = TRUE,
    split_by = "chapter",
    extra_dependencies = c(book_dependency(), extra_dependencies), 
    highlight = "default"
  )
  
  post = config$post_processor
  config$post_processor = function(metadata, input, output, clean, verbose) {
    if (is.function(post)) output = post(metadata, input, output, clean, verbose)

    # a hack to remove Pandoc's margin for code blocks since gitbook has already
    # defined margin on <pre> (there would be too much bottom margin)
    #x = xfun::read_utf8(output)
    #x = x[x != 'div.sourceCode { margin: 1em 0; }']
    #xfun::write_utf8(x, output)

    output
  }
  
  config
}

build_toc <- function(toc) {
  # Set the TOC depth
  toc_depth = 0
  
  # Remove all the li elements
  toc <- stringr::str_replace_all(
    string = toc, 
    pattern = "<li>|</li>", 
    replacement = ""
  )
  
  # Loop over each element in the TOC vector and adjust the content if needed
  for (i in 1:length(toc)) {
    # Convert the ul elements to divs and set the bootstrap classes
    # Different classes are assigned depending on the toc depth
    if (stringr::str_detect(toc[i], "<ul>")) {
      toc_depth = toc_depth + 1
      
      if (toc_depth == 1) {
        toc[i] <- stringr::str_replace(toc[i], "<ul>", 
          '<div id="TOC" class="list-group list-group-flush">')
      } else {
        toc[i] <- stringr::str_replace(toc[i], "<ul>", 
          '<div class="list-group list-group-flush list-subgroup">')
      }
    }
    
    if (stringr::str_detect(toc[i], "</ul>")) {
      toc_depth = toc_depth - 1
      toc[i] <- stringr::str_replace(toc[i], "</ul>", '</div>')
    }
    
    # Set classes on the toc links and also add an identifier
    if (stringr::str_detect(toc[i], "<a href=")) {
      id = stringr::str_extract(toc[i], '(?<=html#).+?(?=")')
      toc[i] <- stringr::str_replace(
        string = toc[i], 
        pattern = "<a href=", 
        replacement = paste0(
          '<a id="item-', 
          id, 
          '" class="list-group-item" href=')
        )
      
      # Remove the heading part from the link so the page loads simply loads 
      # from the top and does not scroll to the first heading
      if (toc_depth == 1) {
        toc[i] <- stringr::str_replace(
          string = toc[i], 
          pattern = paste0("#", id), 
          replacement = ""
        )
      }
    }
    
    # Add span tags to the toc values (i.e., the link names)
    if (stringr::str_detect(toc[i], "toc-section-number") & 
        stringr::str_detect(toc[i], "</span> ")) {
      toc[i] <- stringr::str_replace(
        string = toc[i], 
        pattern = "</span> ", 
        replacement = "</span><span>"
      )
    }
  } 
  
  return(toc)
}

build_nav <- function() {
  paste(
    '<!-- Top navigation-->
    <nav 
      id="navbar" 
      class="navbar navbar-expand-lg navbar-light bg-light border-bottom"
    >
      <div class="container-fluid">
        <button class="btn btn-primary p-1" id="sidebarToggle">
          <svg 
            xmlns="http://www.w3.org/2000/svg" 
            width="26" 
            height="26" 
            fill="currentColor" 
            class="bi bi-list" viewBox="0 0 16 16"
          >
            <path 
              fill-rule="evenodd" 
              d="M2.5 12a.5.5 0 0 1 .5-.5h10a.5.5 0 0 1 0 1H3a.5.5 0 0 1-.5-.5zm0-4a.5.5 0 0 1 .5-.5h10a.5.5 0 0 1 0 1H3a.5.5 0 0 1-.5-.5zm0-4a.5.5 0 0 1 .5-.5h10a.5.5 0 0 1 0 1H3a.5.5 0 0 1-.5-.5z"/>
          </svg>
        </button>
        <button 
          class="navbar-toggler" 
          type="button" 
          data-bs-toggle="collapse" 
          data-bs-target="#navbarContent" 
          aria-controls="navbarContent" 
          aria-expanded="false" 
          aria-label="Toggle navigation">
            <span class="navbar-toggler-icon"></span>
        </button>
        <div class="collapse navbar-collapse" id="navbarContent">
          <ul class="navbar-nav ms-auto mt-2 mt-lg-0">
            <li class="nav-item dropdown">
              <a 
                class="nav-link dropdown-toggle" 
                id="navbarDropdown" 
                href="#" 
                role="button" 
                data-bs-toggle="dropdown" 
                aria-haspopup="true" 
                aria-expanded="false">
                  Settings
              </a>
              <div 
                class="dropdown-menu dropdown-menu-end" 
                aria-labelledby="navbarDropdown">
                  <a id="toggleCodeButton" class="dropdown-item" href="#!">Toggle code</a>
              </div>
            </li>
          </ul>
        </div>
      </div>
    </nav>'
  )
}

build_chapter <- function(chapter) {
  in_div <- FALSE
  correct_refs_found <- 0
  refs_found <- 0
  pre_found <- 0
  
  for (i in 1:length(chapter)) {
    if (chapter[i] == '<div class="foldable">') {
      in_div <- TRUE
      chapter[i] <- '<div class="foldable"><a>Show</a><div class="collapse">'
    }
    
    if (in_div) {
      if (chapter[i] == "</div>") {
        in_div <- FALSE
        chapter[i] <- "</div></div>"
      }
    }
    
    if (stringr::str_detect(chapter[i], "<table")) {
      chapter[i] <- stringr::str_replace(
        string = chapter[i], 
        pattern = "<table", 
        replacement = '<div class="table-container overflow-auto"><table class="table"'
      )
    } 
    
    if (stringr::str_detect(chapter[i], "</table>")) {
      chapter[i] <- stringr::str_replace(
        string = chapter[i], 
        pattern = "</table>", 
        replacement = '</table></div>'
      )
    }
    
    # Hide code blocks by default
    if (stringr::str_detect(chapter[i], '<pre class="sourceCode r">')) {
      chapter[i] <- stringr::str_replace(
        string = chapter[i], 
        pattern = '<pre class="sourceCode r">', 
        replacement = '<pre class="sourceCode r collapse"><div>'
      )
      pre_found <- 1
    }
    
    if (pre_found & stringr::str_detect(chapter[i], '</pre>')) {
      chapter[i] <- stringr::str_replace(
        string = chapter[i], 
        pattern = '</pre>', 
        replacement = '</div></pre>'
      )
      pre_found <- 0
    }
    
    # Remove references at the end of the book
    if (stringr::str_detect(chapter[i], '<h3>References</h3>')) {
      correct_refs_found <- 1
    }
    
    if (!correct_refs_found) {
      if (stringr::str_detect(chapter[i], '<div id="refs"')) {
       refs_found <- 1
      }
  
      if (refs_found > 0 & stringr::str_detect(chapter[i], '<div class="csl-entry">')) {
        refs_found <- refs_found + 1
      }
  
      if (refs_found > 0) {
        chapter[i] <- ""
      }
  
      if (refs_found > 0 & stringr::str_detect(chapter[i], "</div>")) {
        refs_found <- refs_found - 1
      }
    }
  }
  
  return(chapter)
}

build_page <- function(
  head, toc, chapter, link_prev, link_next, rmd_cur, html_cur, foot
) {
  toc <- build_toc(toc)
  nav <- build_nav()
  chapter <- build_chapter(chapter)
  
  paste(c(
    head,
    toc,
    '<!-- Page content wrapper-->
    <div id="page-content-wrapper">',
    nav,
    '<!-- Page content-->
    <div id="main-container" class="container-fluid">',
    '<main>',
    chapter,
    '<div class="text-center m-3">',
    ifelse(
      length(link_prev) != 0, 
      sprintf(
        '<a href="%s"><button class="btn btn-secondary">%s</button></a>', 
        link_prev, 'Previous'
      ), 
      ""
    ),
    ifelse(
      length(link_next) != 0, 
      sprintf(
        '<a href="%s"><button class="btn btn-secondary">%s</button></a>', 
        link_next, 'Next'
      ), 
      ""
    ),
    '</div>',
    '</main>',
    '</div>',
    '</div>',
    foot
  ), collapse = '\n')
}

book_dependency <- function() {
  resources <- system.file("resources", package = 'rethinkpriorities')
  
  list(
    htmltools::htmlDependency(
      name = "book",
      version = "1.0.0",
      src = resources,
      stylesheet = "style.css", 
      script = "littlefoot.js"
    )
  )
}