# tests/testthat/test-sanitize-batch-data.R

# Helper function to create test data
create_test_batch_data <- function() {
  fish_batch <- data.frame(
    batch_id = 1:10,
    batch_type = c("G", "S/L", "I", "N", "G", "S/L", "I", "N", "G", "INVALID"),
    number = c(10, 5, 3, 1, 0, 5, 3, 1, 10, 5),
    species_code = c("PER", "TRF", "PER", "TRF", "PER", "TRF", "PER", "TRF", "PER", "TRF"),
    min_length = c(100, NA, NA, NA, 150, NA, NA, NA, NA, 100),
    max_length = c(150, NA, NA, NA, 120, NA, NA, NA, NA, 150),
    stringsAsFactors = FALSE
  )
  
  ind_measure <- data.frame(
    batch_id = c(1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4),
    size = c(110, 125, 140, 200, 220, 240, 260, 280, 45, 50, 55, 300, 310, 320, 330, 340, 350, 360, 370, 380, 390),
    stringsAsFactors = FALSE
  )
  
  list(fish_batch = fish_batch, ind_measure = ind_measure)
}

test_that("sanitize_batch_data validates inputs", {
  test_data <- create_test_batch_data()
  
  # Test non-data.frame inputs
  expect_error(
    sanitize_batch_data(fish_batch = "not_a_df", ind_measure = test_data$ind_measure),
    "`fish_batch` must be a data frame"
  )
  
  expect_error(
    sanitize_batch_data(fish_batch = test_data$fish_batch, ind_measure = "not_a_df"),
    "`ind_measure` must be a data frame"
  )
  
  # Test missing columns
  bad_fish_batch <- test_data$fish_batch |> dplyr::select(-batch_id)
  expect_error(
    sanitize_batch_data(fish_batch = bad_fish_batch, ind_measure = test_data$ind_measure),
    "missing required columns: batch_id"
  )
  
  bad_ind_measure <- test_data$ind_measure |> dplyr::select(-size)
  expect_error(
    sanitize_batch_data(fish_batch = test_data$fish_batch, ind_measure = bad_ind_measure),
    "missing required columns: size"
  )
})

test_that("sanitize_batch_data filters measurements without matching batches", {
  test_data <- create_test_batch_data()
  
  # Add measurement with non-existent batch
  test_data$ind_measure <- rbind(
    test_data$ind_measure,
    data.frame(batch_id = 999, size = 100)
  )
  
  result <- sanitize_batch_data(
    fish_batch = test_data$fish_batch,
    ind_measure = test_data$ind_measure,
    verbose = FALSE
  )
  
  # The extra measurement should be removed
  expect_false(999 %in% result$ind_measure$batch_id)
  expect_true(nrow(result$ind_measure) < nrow(test_data$ind_measure))
})

test_that("sanitize_batch_data removes invalid batch types", {
  test_data <- create_test_batch_data()
  
  result <- sanitize_batch_data(
    fish_batch = test_data$fish_batch,
    ind_measure = test_data$ind_measure,
    verbose = FALSE
  )
  
  # Batch 10 has type "INVALID" and should be removed
  expect_false(10 %in% result$fish_batch$batch_id)
  expect_true("INVALID" %in% test_data$fish_batch$batch_type)
  expect_false("INVALID" %in% result$fish_batch$batch_type)
  
  # Validation issues should include the invalid type
  expect_true(any(result$validation_issues$issue == "Invalid batch type"))
})

test_that("sanitize_batch_data removes batches with invalid fish count", {
  test_data <- create_test_batch_data()
  
  # Batch 5 has number = 0 (invalid)
  # Batch 1 has number = 10 (valid)
  
  result <- sanitize_batch_data(
    fish_batch = test_data$fish_batch,
    ind_measure = test_data$ind_measure,
    verbose = FALSE
  )
  
  # Batch 5 should be removed
  expect_false(5 %in% result$fish_batch$batch_id)
  # Batch 9 (NA min/max) should be removed
  expect_false(9 %in% result$fish_batch$batch_id)
  # Batch 1 should remain
  expect_true(1 %in% result$fish_batch$batch_id)
  
  # Validation issues should include count validation
  expect_true(any(result$validation_issues$issue == "Invalid or missing fish count"))
})

test_that("sanitize_batch_data validates type G batches correctly", {
  
  # Create specific type G test data
  fish_batch_G <- data.frame(
    batch_id = c(1, 2, 3, 4, 5),
    batch_type = rep("G", 5),
    number = c(10, 10, 10, 10, 3),  # Last one has low count
    species_code = rep("PER", 5),
    min_length = c(100, NA, 150, 200, 100),
    max_length = c(150, 200, 120, 200, 150),  # Batch 3 has min > max
    stringsAsFactors = FALSE
  )
  
  ind_measure_G <- data.frame(
    batch_id = c(1, 1, 1),
    size = c(110, 125, 140),
    stringsAsFactors = FALSE
  )
  
  result <- sanitize_batch_data(
    fish_batch = fish_batch_G,
    ind_measure = ind_measure_G,
    min_individuals_G = 5,
    verbose = FALSE
  )
  
  # Batch 2 (NA min/max) should be removed
  expect_false(2 %in% result$fish_batch$batch_id)
  
  # Batch 3 (min > max) should be removed
  expect_false(3 %in% result$fish_batch$batch_id)
  
  # Batch 5 (low number of individuals) should be removed
  expect_false(5 %in% result$fish_batch$batch_id)
  
  # Batch 1 and 4 should remain
  expect_true(all(c(1, 4) %in% result$fish_batch$batch_id))
  
  # Check specific validation issues
  issues <- result$validation_issues
  expect_true(any(issues$issue == "NA in min_length or max_length"))
  expect_true(any(issues$issue == "min_length > max_length"))
  expect_true(any(grepl("Number of individuals <", issues$issue)))
})

test_that("sanitize_batch_data validates type S/L batches correctly", {
  
  # Create specific type S/L test data
  fish_batch_SL <- data.frame(
    batch_id = c(1, 2),
    batch_type = rep("S/L", 2),
    number = c(5, 10),
    species_code = c("PER", "TRF"),
    min_length = c(NA, NA),
    max_length = c(NA, NA),
    stringsAsFactors = FALSE
  )
  
  # Batch 1 has only 5 measurements (below min_individuals_SL = 10)
  ind_measure_SL <- data.frame(
    batch_id = c(rep(1, 5), rep(2, 15)),
    size = c(100, 110, 120, 130, 140, rep(200, 15)),
    stringsAsFactors = FALSE
  )
  
  result <- sanitize_batch_data(
    fish_batch = fish_batch_SL,
    ind_measure = ind_measure_SL,
    min_individuals_SL = 10,
    verbose = FALSE
  )
  
  # Batch 1 should be removed (insufficient measurements)
  expect_false(1 %in% result$fish_batch$batch_id)
  
  # Batch 2 should remain
  expect_true(2 %in% result$fish_batch$batch_id)
  
  # Check validation issues
  expect_true(any(grepl("Measured individuals <", result$validation_issues$issue)))
})

test_that("sanitize_batch_data validates type I and N batches correctly", {
  # Test type I
  fish_batch_I <- data.frame(
    batch_id = c(1, 2),
    batch_type = c("I", "I"),
    number = c(3, 5),
    species_code = c("PER", "TRF"),
    min_length = c(NA, NA),
    max_length = c(NA, NA),
    stringsAsFactors = FALSE
  )
  
  ind_measure_I <- data.frame(
    batch_id = c(rep(1, 3), rep(2, 3)),  # Batch 2 has mismatch (3 vs expected 5)
    size = c(100, 110, 120, 200, 210, 220),
    stringsAsFactors = FALSE
  )
  
  result_I <- sanitize_batch_data(
    fish_batch = fish_batch_I,
    ind_measure = ind_measure_I,
    verbose = FALSE
  )
  
  # Batch 2 should be removed (count mismatch)
  expect_false(2 %in% result_I$fish_batch$batch_id)
  expect_true(1 %in% result_I$fish_batch$batch_id)
  
  # Test type N
  fish_batch_N <- data.frame(
    batch_id = c(3, 4),
    batch_type = c("N", "N"),
    number = c(1, 1),
    species_code = c("PER", "TRF"),
    min_length = c(NA, NA),
    max_length = c(NA, NA),
    stringsAsFactors = FALSE
  )
  
  ind_measure_N <- data.frame(
    batch_id = c(3, 4, 4),  # Batch 4 has 2 measurements (should be 1)
    size = c(300, 400, 410),
    stringsAsFactors = FALSE
  )
  
  result_N <- sanitize_batch_data(
    fish_batch = fish_batch_N,
    ind_measure = ind_measure_N,
    verbose = FALSE
  )
  
  # Batch 4 should be removed (count mismatch)
  expect_false(4 %in% result_N$fish_batch$batch_id)
  expect_true(3 %in% result_N$fish_batch$batch_id)
})

test_that("sanitize_batch_data returns correct structure", {
  test_data <- create_test_batch_data()
  
  result <- sanitize_batch_data(
    fish_batch = test_data$fish_batch,
    ind_measure = test_data$ind_measure,
    verbose = FALSE
  )
  
  # Check return structure
  expect_type(result, "list")
  expect_named(result, c("fish_batch", "ind_measure", "filtering_log", "validation_issues"))
  expect_s3_class(result$fish_batch, "data.frame")
  expect_s3_class(result$ind_measure, "data.frame")
  expect_s3_class(result$filtering_log, "data.frame")
  expect_s3_class(result$validation_issues, "data.frame")
})

test_that("sanitize_batch_data respects min_individuals_G parameter", {
  
  # Create type G batch with 3 individuals
  fish_batch_G <- data.frame(
    batch_id = 1,
    batch_type = "G",
    number = 3,
    species_code = "PER",
    min_length = 100,
    max_length = 150,
    stringsAsFactors = FALSE
  )
  
  ind_measure_G <- data.frame(
    batch_id = 1,
    size = c(110, 125, 140),
    stringsAsFactors = FALSE
  )
  
  # With strict threshold (min_individuals_G = 10) - should be removed
  result_strict <- sanitize_batch_data(
    fish_batch = fish_batch_G,
    ind_measure = ind_measure_G,
    min_individuals_G = 10,
    verbose = FALSE
  )
  expect_false(1 %in% result_strict$fish_batch$batch_id)
  
  # With lenient threshold (min_individuals_G = 2) - should remain
  result_lenient <- sanitize_batch_data(
    fish_batch = fish_batch_G,
    ind_measure = ind_measure_G,
    min_individuals_G = 2,
    verbose = FALSE
  )
  expect_true(1 %in% result_lenient$fish_batch$batch_id)
})

test_that("sanitize_batch_data respects min_individuals_SL parameter", {
  # Create type S/L batch with 8 measurements
  fish_batch_SL <- data.frame(
    batch_id = 1,
    batch_type = "S/L",
    number = 10,
    species_code = "PER",
    min_length = NA,
    max_length = NA,
    stringsAsFactors = FALSE
  )
  
  ind_measure_SL <- data.frame(
    batch_id = rep(1, 8),
    size = 100:107,
    stringsAsFactors = FALSE
  )
  
  # With strict threshold (min_individuals_SL = 10) - should be removed
  result_strict <- sanitize_batch_data(
    fish_batch = fish_batch_SL,
    ind_measure = ind_measure_SL,
    min_individuals_SL = 10,
    verbose = FALSE
  )
  expect_false(1 %in% result_strict$fish_batch$batch_id)
  
  # With lenient threshold (min_individuals_SL = 5) - should remain
  result_lenient <- sanitize_batch_data(
    fish_batch = fish_batch_SL,
    ind_measure = ind_measure_SL,
    min_individuals_SL = 5,
    verbose = FALSE
  )
  expect_true(1 %in% result_lenient$fish_batch$batch_id)
})

test_that("sanitize_batch_data handles verbose output", {
  test_data <- create_test_batch_data()

  # Test with verbose = TRUE
  expect_message(
    result <- sanitize_batch_data(
      fish_batch = test_data$fish_batch,
      ind_measure = test_data$ind_measure,
      verbose = TRUE,
      min_individuals_SL = 10,
      min_individuals_G = 5
    ),
    "=== Batch Data Sanitization Summary ==="
  )

  # Test with verbose = FALSE (should not produce messages)
  expect_silent(
    result <- sanitize_batch_data(
      fish_batch = test_data$fish_batch,
      ind_measure = test_data$ind_measure,
      verbose = FALSE
    )
  )
})

test_that("sanitize_batch_data handles edge cases", {
  # Empty data frames
  empty_fish_batch <- data.frame(
    batch_id = integer(),
    batch_type = character(),
    number = integer(),
    species_code = character(),
    min_length = numeric(),
    max_length = numeric(),
    stringsAsFactors = FALSE
  )
  
  empty_ind_measure <- data.frame(
    batch_id = integer(),
    size = numeric(),
    stringsAsFactors = FALSE
  )
  
  result <- sanitize_batch_data(
    fish_batch = empty_fish_batch,
    ind_measure = empty_ind_measure,
    verbose = FALSE
  )
  
  expect_equal(nrow(result$fish_batch), 0)
  expect_equal(nrow(result$ind_measure), 0)
  expect_equal(nrow(result$filtering_log), 0)
  expect_equal(nrow(result$validation_issues), 0)
  
  # NA values in number column
  fish_batch_na <- data.frame(
    batch_id = 1,
    batch_type = "G",
    number = NA,
    species_code = "PER",
    min_length = 100,
    max_length = 150,
    stringsAsFactors = FALSE
  )
  
  ind_measure_na <- data.frame(
    batch_id = 1,
    size = 120,
    stringsAsFactors = FALSE
  )
  
  result_na <- sanitize_batch_data(
    fish_batch = fish_batch_na,
    ind_measure = ind_measure_na,
    verbose = FALSE
  )
  
  expect_equal(nrow(result_na$fish_batch), 0)
  expect_true(any(result_na$validation_issues$issue == "Invalid or missing fish count"))
})

test_that("sanitize_batch_data correctly handles min_length == max_length", {
  # With your change (min_length > max_length), equal values should be allowed
  fish_batch_equal <- data.frame(
    batch_id = 1,
    batch_type = "G",
    number = 10,
    species_code = "PER",
    min_length = 100,
    max_length = 100,  # Equal values
    stringsAsFactors = FALSE
  )
  
  ind_measure_equal <- data.frame(
    batch_id = 1,
    size = 100,
    stringsAsFactors = FALSE
  )
  
  result <- sanitize_batch_data(
    fish_batch = fish_batch_equal,
    ind_measure = ind_measure_equal,
    verbose = FALSE
  )
  
  # Should NOT be removed (equal values are allowed)
  expect_true(1 %in% result$fish_batch$batch_id)
  expect_false(any(result$validation_issues$issue == "min_length > max_length"))
})
