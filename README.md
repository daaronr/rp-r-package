# rethinkpriorities R package

This is the `rethinkpriorities` R package. The goal of this package is to collect R functions that are likely to be re-used across projects from Rethink Priorities. As such, this package is for internal use only.

## Installation

Currently there are no plans to submit this package to CRAN, so the package can be installed from this GitHub repository, using the following code:

```
devtools::install_github("rethinkpriorities/rp-r-package")
```

After installing the package, you can load the package using `library(rethinkpriorities)`.

Note that the name of the R package is different from the name of the GitHub repository.

## Tips on adding functions to this package

[illustration of how to add a function here](https://www.dropbox.com/s/7lbmrdhjgalzoic/adding_a_function_to_rp_r_package_2380608530.mp4?dl=0) (video)

## Notes on the functions added

DR: I went through my `functions.R` file and added nearly all the functions actually used in EA Survey work (plus a very few more that I thought were worth highlighting).

Note that some of these functions/R files...:

- are just slight tweaks on existing functions, setting options or whatever

- are only used in the ML tidymodels work

- some .R files have multiple exported functions, not sure I did this right

- some are not very 'general use', as they refer to specific existing relative folder paths, or require other packages installed (which I've tried to document, but not always)

## Other relevant packages/repos

DR:

- I started an additional [’noodling functions’ package](https://github.com/rethinkpriorities/r-noodling-package). Those are functions that are just helpful when dynamically working in R; they are mainly not for producing output.

- As noted above, most of these functions were used in [ea data](https://github.com/rethinkpriorities/ea-data) work. However, some of these might be improved or replaced

