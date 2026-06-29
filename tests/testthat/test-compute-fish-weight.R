# Helper to create test data
create_test_length_data <- function() {
  fish_data <- data.frame(
    species_code = c("PER", "TRF", "PER", "UNKNOWN", "PER"),
    size_mm = c(120, 250, 95, 150, 180),
    stringsAsFactors = FALSE
  )
  
  species_ref <- data.frame(
    species_code = c("PER", "TRF", "BLC"),
    latin_name = c("Perca fluviatilis", "Salmo trutta", "Barbatula barbatula"),
    stringsAsFactors = FALSE
  )
  
  list(fish_data = fish_data, species_ref = species_ref)
}

# Mock FishBase data
mock_fishbase_data <- function() {
  data.frame(
    Species = c("Perca fluviatilis", "Salmo trutta"),
    Type = c("TL", "TL"),
    Method = c("Type I linear regression", "Type I linear regression"),
    a = c(0.012, 0.008),
    b = c(3.05, 3.10),
    Number = c(5, 3),
    stringsAsFactors = FALSE
  )
}

test_that("convert_length_to_weight validates inputs", {
  td <- create_test_length_data()
  
  # Missing required columns
  bad_data <- td$fish_data |> dplyr::select(-size_mm)
  expect_error(
    convert_length_to_weight(bad_data, td$species_ref),
    "missing required columns: size_mm"
  )
  
  # Invalid species_ref
  expect_error(
    convert_length_to_weight(td$fish_data, "not_df"),
    "species_ref must be a data frame"
  )
  
  # Missing latin_name column
  bad_ref <- td$species_ref |> dplyr::select(-latin_name)
  expect_error(
    convert_length_to_weight(td$fish_data, bad_ref),
    "missing required columns: latin_name"
  )
})

test_that("convert_length_to_weight uses FishBase coefficients correctly", {
  td <- create_test_length_data()
  mock_fb <- mock_fishbase_data()
  
  result <- convert_length_to_weight(
    fish_data = td$fish_data,
    species_ref = td$species_ref,
    length_weight_table = mock_fb,
    verbose = FALSE
  )
  
  # Check PER: 0.012 * (12^3.05) = 0.012 * 12^3.05
  per_weight <- result$fish_data$weight_g[result$fish_data$species_code == "PER"][1]
  expected_per <- 0.012 * (12^3.05)
  expect_equal(per_weight, expected_per, tolerance = 0.01)
  
  # Check coefficients table
  expect_true("PER" %in% result$coefficients$species_code)
  expect_equal(result$coefficients$a[result$coefficients$species_code == "PER"], 0.012)
  expect_equal(result$coefficients$b[result$coefficients$species_code == "PER"], 3.05)
  expect_equal(result$coefficients$source[result$coefficients$species_code == "PER"], "fishbase")
})

test_that("convert_length_to_weight uses species conversion", {
  td <- create_test_length_data()
  
  # Add conversion for UNKNOWN to PER
  conversion <- c(UNKNOWN = "PER")
  
  mock_fb <- mock_fishbase_data()
  
  result <- convert_length_to_weight(
    fish_data = td$fish_data,
    species_ref = td$species_ref,
    length_weight_table = mock_fb,
    species_conversion = conversion,
    verbose = FALSE
  )
  
  # UNKNOWN should use PER coefficients (0.012, 3.05)
  unknown_weight <- result$fish_data$weight_g[result$fish_data$species_code == "UNKNOWN"]
  expected_unknown <- 0.012 * (15^3.05)  # 150mm = 15cm
  expect_equal(unknown_weight, expected_unknown, tolerance = 0.01)
  
  # Check source
  expect_equal(result$coefficients$source[result$coefficients$species_code == "UNKNOWN"], "conversion")
})

test_that("convert_length_to_weight uses default coefficients for missing species", {
  td <- create_test_length_data()
  
  # Remove UNKNOWN from species_ref so no FishBase
  species_ref_no_unknown <- td$species_ref |>
    rbind(data.frame(species_code = "UNKNOWN", latin_name = "bla"))
  
  result <- convert_length_to_weight(
    fish_data = td$fish_data,
    species_ref = species_ref_no_unknown,
    length_weight_table = mock_fishbase_data(),
    verbose = FALSE
  )
  
  # UNKNOWN should use default coefficients (0.01, 3.03)
  unknown_weight <- result$fish_data$weight_g[result$fish_data$species_code == "UNKNOWN"]
  expected_unknown <- 0.01 * (15^3.03)
  expect_equal(unknown_weight, expected_unknown, tolerance = 0.01)
  
  # Check missing species tracking
  expect_true("UNKNOWN" %in% result$missing_species)
  expect_equal(result$coefficients$source[result$coefficients$species_code == "UNKNOWN"], "default")
})

test_that("convert_length_to_weight handles vectorized conversion", {
  td <- create_test_length_data()
  mock_fb <- mock_fishbase_data()
  
  result <- convert_length_to_weight(
    fish_data = td$fish_data,
    species_ref = td$species_ref,
    length_weight_table = mock_fb,
    verbose = FALSE
  )
  
  # All fish should have weight_g
  expect_true(all(!is.na(result$fish_data$weight_g)))
  expect_equal(nrow(result$fish_data), nrow(td$fish_data))
  
  # Check multiple PER entries
  per_weights <- result$fish_data$weight_g[result$fish_data$species_code == "PER"]
  expect_equal(length(per_weights), 3)  # 3 PER entries
  expect_true(all(per_weights > 0))
})

test_that("convert_length_to_weight returns correct structure", {
  td <- create_test_length_data()
  mock_fb <- mock_fishbase_data()
  
  result <- convert_length_to_weight(
    fish_data = td$fish_data,
    species_ref = td$species_ref,
    length_weight_table = mock_fb,
    verbose = FALSE
  )
  
  # Check list structure
  expect_type(result, "list")
  expect_named(result, c("fish_data", "coefficients", "missing_species", "source_summary"))
  
  # Check fish_data
  expect_s3_class(result$fish_data, "data.frame")
  expect_true("weight_g" %in% names(result$fish_data))
  
  # Check coefficients
  expect_s3_class(result$coefficients, "data.frame")
  expect_named(result$coefficients, c("species_code", "a", "b", "source"))
  
  # Check source_summary
  expect_s3_class(result$source_summary, "data.frame")
  expect_true(all(c("source", "n_species") %in% names(result$source_summary)))
})

test_that("convert_length_to_weight respects verbose parameter", {
  td <- create_test_length_data()
  mock_fb <- mock_fishbase_data()
  
  # Verbose should produce messages
  suppressMessages( 
    expect_message(
      convert_length_to_weight(td$fish_data, td$species_ref, mock_fb, verbose = TRUE),
      "Processing.*unique species"
    )
  )
  
  # Silent should produce no messages
  expect_silent(
    convert_length_to_weight(td$fish_data, td$species_ref, mock_fb, verbose = FALSE)
  )
})

test_that("convert_length_to_weight handles species without Latin names", {
  td <- create_test_length_data()
 
  # Add species without Latin name
  fish_data_extra <- rbind(td$fish_data, data.frame(species_code = "NO_LATIN", size_mm = 100))
 
  mock_fb <- mock_fishbase_data()
 
  result <- convert_length_to_weight(
    fish_data = fish_data_extra,
    species_ref = td$species_ref,  # NO_LATIN not in ref
    length_weight_table = mock_fb,
    verbose = FALSE
  )
 
  # Should use default coefficients
  expect_true("NO_LATIN" %in% result$missing_species)
  expect_equal(result$coefficients$source[result$coefficients$species_code == "NO_LATIN"], "default")
})

test_that("convert_length_to_weight works with empty FishBase data", {
  td <- create_test_length_data()
 
  # Empty FishBase table
  empty_fb <- mock_fishbase_data() |>
    dplyr::slice(0)
 
  result <- convert_length_to_weight(
    fish_data = td$fish_data,
    species_ref = td$species_ref,
    length_weight_table = empty_fb,
    verbose = FALSE
  )
 
  # All species should use default coefficients
  expect_true(all(result$coefficients$source == "default"))
  expect_true(all(!is.na(result$fish_data$weight_g)))
})

test_that("convert_length_to_weight uses correct unit conversion", {
  td <- create_test_length_data()
  
  # Use simple coefficients for easy verification
  mock_fb_simple <- data.frame(
    Species = "Perca fluviatilis",
    Type = "TL",
    Method = "Type I linear regression",
    a = 1,
    b = 3,
    Number = 1,
    stringsAsFactors = FALSE
  )
  
  # Create single PER fish with 100mm length
  fish_single <- data.frame(species_code = "PER", size_mm = 100)
  
  result <- convert_length_to_weight(
    fish_data = fish_single,
    species_ref = td$species_ref,
    length_weight_table = mock_fb_simple,
    verbose = FALSE
  )
  
  # 100mm = 10cm, weight = 1 * 10^3 = 1000g
  expect_equal(result$fish_data$weight_g, 1000)
})

test_that("convert_length_to_weight prioritizes FishBase over conversion", {
  td <- create_test_length_data()
  
  # Add conversion for PER to something else
  conversion <- c(PER = "TRF")
  
  mock_fb <- mock_fishbase_data()
  
  result <- convert_length_to_weight(
    fish_data = td$fish_data |> dplyr::filter(species_code == "PER"),
    species_ref = td$species_ref,
    length_weight_table = mock_fb,
    species_conversion = conversion,
    verbose = FALSE
  )
  
  # Should use FishBase (0.012, 3.05) not TRF (0.008, 3.10)
  expect_equal(result$coefficients$a[result$coefficients$species_code == "PER"], 0.012)
  expect_equal(result$coefficients$source[result$coefficients$species_code == "PER"], "fishbase")
})
