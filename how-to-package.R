
# Update ------------------------------------------------------------------

# Update documentation and (re)install the package
library(dplyr)
devtools::document()
devtools::install()

# Unload the package and load it again
detach("package:rethinkpriorities", unload = TRUE)
library(rethinkpriorities)

# Restart sessions --------------------------------------------------------

.rs.restartR()
library(rethinkpriorities)

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
