library(usethis)
library(here)

# Creating package structure
create_tidy_package(path = here(), copyright_holder = NULL)

# Get a GPL licence
use_gpl3_license()

# Build temporary package and compile the README
devtools::build_readme()


# Get the fish data package
devtools::install_github("PascalIrz/aspe")


install.packages("pkgfilecache")
use_package("pkgfilecache", type = "Imports", min_version = NULL)

# Create R function file
use_r("get_aspe_data")
