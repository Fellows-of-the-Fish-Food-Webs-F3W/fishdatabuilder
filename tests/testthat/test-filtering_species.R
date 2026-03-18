test_that("check_aspe_fish_species identifies missing reference codes", {
  skip_if_not_installed("rfishbase")
  skip_if_offline()

  # Create test reference
  test_ref <- data.frame(
    species_code = c("ABL", "TRF", "ANG"),
    latin_name = c("Alburnus alburnus", "Salmo trutta", "Anguilla anguilla")
  )
 
  # Test with warning
  expect_warning(
    result <- check_aspe_fish_species(
      species = c("ABL", "XXX", "YYY"),
      species_ref = test_ref
    ),
    "Missing species in reference: XXX, YYY"
  )
 
  # Check invisible return
  expect_type(result, "list")
  expect_equal(result$missing_in_ref, c("XXX", "YYY"))
})

test_that("check_aspe_fish_species handles FishBase validation", {
  # Skip if no internet or rfishbase not available
  skip_if_not_installed("rfishbase")
  skip_if_offline()
  
  test_ref <- data.frame(
    species_code = c("ABL", "BAD", "TRT"),
    latin_name = c("Alburnus alburnus", "Invalidus nameus", "Salmo trutta")
  )
 
  expect_warning(
    result <- check_aspe_fish_species(
      species = c("ABL", "BAD", "TRT"),
      species_ref = test_ref
    ),
    "Missing species in FishBase"
  )
 
  expect_true(all(c("BAD", "TRT") %in% result$missing_in_fishbase))
  expect_true(all(c("ABL") %in% result$valid_codes))
})

test_that("check_aspe_fish_species validates inputs", {
  # Test non-character species
  expect_error(
    check_aspe_fish_species(species = 123),
    "`species` must be a character vector"
  )
 
  # Test invalid reference
  expect_error(
    check_aspe_fish_species(species_ref = "not_a_df"),
    "`species_ref` must be a data frame"
  )
 
  # Test missing columns in reference
  bad_ref <- data.frame(wrong_col = 1:2)
  expect_error(
    check_aspe_fish_species(species_ref = bad_ref),
    "missing required columns"
  )
})

test_that("check_aspe_fish_species handles empty or NULL inputs", {
  test_ref <- data.frame(
    species_code = c("ABL", "TRF"),
    latin_name = c("Alburnus alburnus", "Salmo trutta")
  )
  
  # Empty species vector
  expect_silent(result <- check_aspe_fish_species(character(0), test_ref))
  expect_equal(result$valid_codes, character(0))
  expect_equal(result$missing_in_ref, character(0))
  
  # No valid codes
  expect_warning(
    result <- check_aspe_fish_species(c("XXX", "YYY"), test_ref),
    "Missing species in reference: XXX, YYY"
  )
  expect_equal(result$valid_codes, character(0))
})
test_that("sanitize_species_code removes unwanted species", {
  # Test data
  test_data <- data.frame(
    species_code = c("TRF", "ASL", "BBX", "GAR", "PFL"),
    count = 1:5
  )
  
  # Test with custom removal
  result <- sanitize_species_code(
    data = test_data,
    species_to_remove = c("ASL", "PFL"),
    species_to_replace = c("BBX" = "BBG")
  )
  
  expect_equal(nrow(result), 3)  # Removed 2 rows
  expect_true(!"ASL" %in% result$species_code)
  expect_true(!"PFL" %in% result$species_code)
})

test_that("sanitize_species_code replaces codes correctly", {
  test_data <- data.frame(
    species_code = c("BBX", "LP?", "CMI", "TRF"),
    count = 1:4
  )
 
  result <- sanitize_species_code(
    data = test_data,
    species_to_remove = character(0),  # Don't remove anything
    species_to_replace = c(
      "BBX" = "BBG",
      "LP\\?" = "LPP",
      "CMI" = "CCO"
    )
  )

  expect_equal(result$species_code, c("BBG", "LPP", "CCO", "TRF"))
})

test_that("sanitize_species_code handles custom column names", {
  test_data <- data.frame(
    spp = c("TRF", "BBX", "ASL"),
    value = 1:3
  )

  result <- sanitize_species_code(
    data = test_data,
    species_var = spp,
    species_to_remove = c("ASL"),
    species_to_replace = c("BBX" = "BBG")
  )
  
  expect_equal(result$spp, c("TRF", "BBG"))
})

test_that("sanitize_species_code validates inputs", {
  test_data <- data.frame(species_code = c("TRF", "BBX"))
  
  # Test non-data.frame input
  expect_error(
    sanitize_species_code(data = "not_a_df"),
    "`data` must be a data frame"
  )
  
  # Test missing column
  bad_data <- data.frame(wrong_col = 1:2)
  expect_error(
    sanitize_species_code(data = bad_data),
    "Column 'species_code' not found in data"
  )
  
  # Test invalid species_to_replace
  expect_error(
    sanitize_species_code(
      data = test_data,
      species_to_replace = c("BBX", "BBG")  # Not named
    ),
    "must be a named character vector"
  )
})

test_that("species_code_to_remove returns correct codes", {
  # Test default
  default <- species_code_to_remove()
  expect_type(default, "character")
  expect_true(all(nchar(default) == 3))
  expect_true(all(default %in% c(
    "ASL", "CRC", "ECR", "OCL",
    "PCC", "OCI", "PCV",
    "PFL", "COR", "SAL")
  ))
 
  # Test not_fish = FALSE
  no_fish_false <- species_code_to_remove(not_fish = FALSE)
  expect_equal(no_fish_false, c("COR", "SAL"))
 
  # Test both FALSE
  both_false <- species_code_to_remove(FALSE, FALSE)
  expect_equal(both_false, character(0))

  # Test custom additions
  add_code <- species_code_to_remove(FALSE, FALSE, additional_codes = c("ABA"))
  expect_equal(add_code, c("ABA"))
 
  # Test input validation
  expect_error(species_code_to_remove(not_fish = "yes"))
  expect_error(species_code_to_remove(not_determined = c(TRUE, FALSE)))
  expect_error(species_code_to_remove(additional_codes = 1))
})

test_that("species_code_to_replace returns correct mapping", {
  mapping <- species_code_to_replace()
  
  # Check structure
  expect_type(mapping, "character")
  expect_true(!is.null(names(mapping)))
  
  # Check specific mappings
  expect_equal(unname(mapping["BBX"]), "BBG")
  expect_equal(unname(mapping["LP\\?"]), "LPP")
  expect_equal(unname(mapping["CMI"]), "CCO")
  
  # Check that all replacement codes exist (if you have a valid species list)
  valid_codes <- cleaning_species_ref_aspe() |> dplyr::pull(species_code)
  expect_true(all(mapping %in% valid_codes))
})
