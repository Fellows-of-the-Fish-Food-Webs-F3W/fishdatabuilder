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
use_r("replace_strings")
use_r("cleaning_site_location")
use_r("cleaning_operation")
use_r("fish_batches")
use_r("filtering_species")
use_r("sanitize_fish_size")
use_r("fish_weight")


# Use test 
usethis::use_testthat(3)
use_test("aspe_data")
use_test("station_cleaning")
use_test("operation_cleaning")
use_test("helpers")
use_test("gen_size_from_fish_batch")
use_test("filter-operation-batch-measure")
use_test("filtering_species")
use_test("convert-fork-to-total")
use_test("remove-impossible-lengths")
use_test("length-sanitization-integration")
use_test("sanitize-batch-data")
use_test("generate-individual-size")
use_test("compute-fish-weight")


#usethis::use_package("mockery", "Suggests")
usethis::use_package("covr", "Suggests")
usethis::use_package("truncdist", "Suggests")

usethis::use_vignette("getting_started.qmd", "Getting started")
usethis::use_build_ignore("vignettes/*_files")
usethis::use_git_ignore("*_files", "vignettes")

# Cleaning dependencies 
pkg_dep_to_add <- c('dplyr', 'lubridate', 'purrr', 'rfishbase',
  'rlang', 'sf', 'stringr', 'tidyr', 'truncdist', 'tibble')
purrr::walk(
  pkg_dep_to_add,
  \(x) use_package(x, type = "Imports", min_version = NULL)
)

usethis::use_package_doc()
usethis::use_github_action("test-coverage")


