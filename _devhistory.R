library(usethis)
library(here)

# Creating package structure
create_tidy_package(path = here(), copyright_holder = NULL)

# Get a GPL licence
use_gpl3_license()

# Build temporary package and compile the README
devtools::build_readme()


