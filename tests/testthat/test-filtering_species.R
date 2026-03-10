test_that("species_to_remove returns correct codes", {
  # Test default
  default <- species_to_remove()
  expect_type(default, "character")
  expect_true(all(nchar(default) == 3))
  expect_true(all(default %in% c(
    "ASL", "CRC", "ECR", "OCL",
    "PCC", "OCI", "PCV",
    "PFL", "COR", "SAL")
  ))
 
  # Test not_fish = FALSE
  no_fish_false <- species_to_remove(not_fish = FALSE)
  expect_equal(no_fish_false, c("COR", "SAL"))
 
  # Test both FALSE
  both_false <- species_to_remove(FALSE, FALSE)
  expect_equal(both_false, character(0))

  # Test custom additions
  add_code <- species_to_remove(FALSE, FALSE, additional_codes = c("ABA"))
  expect_equal(add_code, c("ABA"))
 
  # Test input validation
  expect_error(species_to_remove(not_fish = "yes"))
  expect_error(species_to_remove(not_determined = c(TRUE, FALSE)))
  expect_error(species_to_remove(additional_codes = 1))
})

test_that("species_code_to_replace returns correct mapping", {
  mapping <- species_code_to_replace()
  
  # Check structure
  expect_type(mapping, "character")
  expect_true(!is.null(names(mapping)))
  
  # Check specific mappings
  expect_equal(unname(mapping["BBX"]), "BBG")
  expect_equal(unname(mapping["LP?"]), "LPP")
  expect_equal(unname(mapping["CMI"]), "CCO")
  
  # Check that all replacement codes exist (if you have a valid species list)
  valid_codes <- cleaning_species_ref_aspe() |> pull(species_code)
  expect_true(all(mapping %in% valid_codes))
})
