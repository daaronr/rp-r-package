
# Update ------------------------------------------------------------------

# Update documentation
devtools::document()

# Load the package
devtools::load_all()

# Install the dev version
devtools::install()
#.rs.restartR()

# Setup -------------------------------------------------------------------

# Use magrittr's pipe in this package 
usethis::use_pipe(export = TRUE)

# Add dependency ----------------------------------------------------------

usethis::use_package("lavaan", "Suggests")
usethis::use_tidy_description()

# Create a vignette -------------------------------------------------------

# usethis::use_vignette("")

# Add a data set ----------------------------------------------------------

# usethis::use_data()

# CRAN submission ---------------------------------------------------------

# Check examples
# devtools::run_examples()

# Check package
# devtools::load_all()
#devtools::check()
#devtools::check(args = c('--run-donttest')) # Without examples test
#devtools::check(args = c('--as-cran'))

# run R CMD check on CRAN’s servers
#devtools::check_win_devel()
#devtools::check_win_release()

# Build tar
#devtools::build()
