# Helper function to create test data
create_test_sanitized_data <- function() {
  fish_batch <- data.frame(
    batch_id = 1:8,
    batch_type = c("G", "G", "S/L", "S/L", "I", "I", "N", "N"),
    number = c(10, 5, 8, 3, 4, 2, 1, 1),
    species_code = c("PER", "TRF", "PER", "TRF", "PER", "TRF", "PER", "TRF"),
    min_length = c(100, 200, NA, NA, NA, NA, NA, NA),
    max_length = c(150, 300, NA, NA, NA, NA, NA, NA),
    maximal_length_mm = c(600, 1000, 600, 1000, 600, 1000, 600, 1000),
    stringsAsFactors = FALSE
  )
  
  ind_measure <- data.frame(
    batch_id = as.integer(c(3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 5, 6, 6, 7, 8)),
    size = as.integer(c(110, 115, 120, 125, 130, 135, 140, 145, 250, 260, 270, 
             45, 50, 55, 60, 80, 85, 300, 400)),
    stringsAsFactors = FALSE
  )
  
  list(fish_batch = fish_batch, ind_measure = ind_measure)
}

test_that("generate_individual_sizes validates inputs", {
  test_data <- create_test_sanitized_data()
  
  # Test non-list input
  expect_error(
    generate_individual_sizes(sanitized = "not_a_list"),
    "sanitized must be output from sanitize_batch_data"
  )
  
  # Test missing components
  expect_error(
    generate_individual_sizes(sanitized = list(fish_batch = test_data$fish_batch)),
    "sanitized must be output from sanitize_batch_data"
  )
  
  # Test missing max_length_mm column
  bad_batch <- test_data$fish_batch |> dplyr::select(-maximal_length_mm)
  expect_error(
    generate_individual_sizes(sanitized = list(
      fish_batch = bad_batch, 
      ind_measure = test_data$ind_measure
    )),
    "must contain 'maximal_length_mm' column"
  )
})

test_that("generate_individual_sizes processes type G batches correctly", {
  test_data <- create_test_sanitized_data()
  
  result <- generate_individual_sizes(test_data, seed = 123, verbose = FALSE)
  
  # Check G batches
  g_results <- result |> dplyr::filter(batch_id %in% c(1, 2))
  
  # Batch 1: PER, 10 individuals, min=100, max=150, species_max=600
  batch1 <- g_results |> dplyr::filter(batch_id == 1)
  expect_equal(nrow(batch1), 10)
  expect_true(all(batch1$species_code == "PER"))
  expect_true(all(batch1$size_mm >= 0))
  expect_true(all(batch1$size_mm <= 600))  # Should not exceed species max
  expect_true(all(!is.na(batch1$size_mm)))
  
  # Batch 2: TRF, 5 individuals, min=200, max=300, species_max=1000
  batch2 <- g_results |> dplyr::filter(batch_id == 2)
  expect_equal(nrow(batch2), 5)
  expect_true(all(batch2$species_code == "TRF"))
  expect_true(all(batch2$size_mm >= 0))
  expect_true(all(batch2$size_mm <= 1000))
})

test_that("generate_individual_sizes processes type S/L batches correctly", {
  test_data <- create_test_sanitized_data()
  
  result <- generate_individual_sizes(test_data, seed = 123, verbose = FALSE)
  
  # Check S/L batches
  sl_results <- result |> dplyr::filter(batch_id %in% c(3, 4))
  
  # Batch 3: PER, 8 individuals, measured sizes 110-145
  batch3 <- sl_results |> dplyr::filter(batch_id == 3)
  expect_equal(nrow(batch3), 8)
  expect_true(all(batch3$species_code == "PER"))
  expect_true(all(batch3$size_mm >= 0))
  expect_true(all(batch3$size_mm <= 600))  # Species max for PER
  expect_true(all(!is.na(batch3$size_mm)))
  
  # Batch 4: TRF, 3 individuals, measured sizes 250-270
  batch4 <- sl_results |> dplyr::filter(batch_id == 4)
  expect_equal(nrow(batch4), 3)
  expect_true(all(batch4$species_code == "TRF"))
  expect_true(all(batch4$size_mm <= 1000))
})

test_that("generate_individual_sizes processes type I batches correctly", {
  test_data <- create_test_sanitized_data()
  
  result <- generate_individual_sizes(test_data, seed = 123, verbose = FALSE)
  
  # Check I batches
  i_results <- result |> dplyr::filter(batch_id %in% c(5, 6))
  
  # Batch 5: PER, 4 individuals
  batch5 <- i_results |> dplyr::filter(batch_id == 5)
  expect_equal(nrow(batch5), 4)
  expect_equal(sort(batch5$size_mm), c(45, 50, 55, 60))
  
  # Batch 6: TRF, 2 individuals
  batch6 <- i_results |> dplyr::filter(batch_id == 6)
  expect_equal(nrow(batch6), 2)
  expect_equal(sort(batch6$size_mm), c(80, 85))
})

test_that("generate_individual_sizes processes type N batches correctly", {
  test_data <- create_test_sanitized_data()
  
  result <- generate_individual_sizes(test_data, seed = 123, verbose = FALSE)
  
  # Check N batches
  n_results <- result |> dplyr::filter(batch_id %in% c(7, 8))
  
  # Batch 7: PER, 1 individual
  batch7 <- n_results |> dplyr::filter(batch_id == 7)
  expect_equal(nrow(batch7), 1)
  expect_equal(batch7$size_mm, 300)
  
  # Batch 8: TRF, 1 individual
  batch8 <- n_results |> dplyr::filter(batch_id == 8)
  expect_equal(nrow(batch8), 1)
  expect_equal(batch8$size_mm, 400)
})

test_that("generate_individual_sizes respects seed parameter", {
  test_data <- create_test_sanitized_data()
  
  # Two runs with same seed should produce identical results
  result1 <- generate_individual_sizes(test_data, seed = 42, verbose = FALSE)
  result2 <- generate_individual_sizes(test_data, seed = 42, verbose = FALSE)
  
  expect_equal(result1$size_mm, result2$size_mm)
  
  # Different seeds should produce different results
  result3 <- generate_individual_sizes(test_data, seed = 123, verbose = FALSE)
  
  # At least some values should differ (but not guaranteed for small samples)
  # This test checks the seed is actually being used
  expect_false(identical(result1$size_mm, result3$size_mm))
})

test_that("generate_individual_sizes handles verbose output", {
  test_data <- create_test_sanitized_data()
  
  # Test with verbose = TRUE
  expect_message(
    result <- generate_individual_sizes(test_data, verbose = TRUE),
    "=== Generating Individual Fish Sizes ==="
  )
  
  # Test with verbose = FALSE
  expect_silent(
    result <- generate_individual_sizes(test_data, verbose = FALSE)
  )
})

test_that("generate_individual_sizes produces correct output structure", {
  test_data <- create_test_sanitized_data()
  test_data$fish_batch
  test_data$ind_measure
  
  result <- generate_individual_sizes(test_data, verbose = FALSE)
  
  # Check structure
  expect_s3_class(result, "data.frame")
  expect_named(result, c("batch_id", "species_code", "size_mm"))
  expect_type(result$batch_id, "integer")
  expect_type(result$species_code, "character")
  expect_type(result$size_mm, "double") # Rounded to mm
  
  # Check total row count
  expected_rows <- sum(test_data$fish_batch$number)
  expect_equal(nrow(result), expected_rows)
})

test_that("generate_individual_sizes handles edge cases", {
  test_data <- create_test_sanitized_data()
  # Empty data
  empty_sanitized <- list(
    fish_batch = test_data$fish_batch[0, ],
    ind_measure = test_data$ind_measure[0, ]
  )
  
  result <- generate_individual_sizes(empty_sanitized, verbose = FALSE)
  expect_equal(nrow(result), 0)
  #expect_named(result, c("batch_id", "species_code", "size_mm"))
  
  # Single batch
  single_batch <- list(
    fish_batch = test_data$fish_batch[1, ],
    ind_measure = test_data$ind_measure[0, ]
  )

  result_single <- generate_individual_sizes(single_batch, verbose = FALSE)
  expect_equal(nrow(result_single), single_batch$fish_batch$number)
})

test_that("generate_individual_sizes respects species maximal lengths", {
  test_data <- create_test_sanitized_data()

  # Modify species max to be very small to test boundary
  test_data$fish_batch$maximal_length_mm[test_data$fish_batch$species_code == "PER"] <- 200
g_sl_batch <- test_data$fish_batch |>
        filter(batch_type %in% c("G", "S/L")) |> pull(batch_id)

  result <- generate_individual_sizes(test_data, seed = 123, verbose = FALSE)

  # All PER individuals should be <= 200 mm
  per_individuals <- result |>
    dplyr::filter(species_code == "PER")  |>
    dplyr::filter(batch_id %in% g_sl_batch)

  expect_true(all(per_individuals$size_mm <= 200))
  
  # TRF individuals should still be <= 1000
  trf_individuals <- result |> dplyr::filter(species_code == "TRF")
  expect_true(all(trf_individuals$size_mm <= 1000))
})

test_that("generate_individual_sizes produces biologically realistic sizes", {
  test_data <- create_test_sanitized_data()
  
  result <- generate_individual_sizes(test_data, seed = 123, verbose = FALSE)
  
  # No negative sizes
  expect_true(all(result$size_mm >= 0))
  
  # No sizes exceeding species maximal
  max_by_species <- test_data$fish_batch |>
    dplyr::select(species_code, maximal_length_mm) |>
    dplyr::distinct()
  
  for (i in seq_len(nrow(max_by_species))) {
    species <- max_by_species$species_code[i]
    max_len <- max_by_species$maximal_length_mm[i]
    species_sizes <- result$size_mm[result$species_code == species]
    if (length(species_sizes) > 0) {
      expect_true(all(species_sizes <= max_len))
    }
  }
})
