test_that("Full pipeline works correctly", {
  test_data <- create_test_length_data()

  # Add outliers and fork measurements
  test_data$ind_measure$size[1] <- 800  # Outlier
  test_data$ind_measure$size[6] <- 300  # ALA max 600, OK
  test_data$ind_measure$size[7] <- 650  # ALA max 600, outlier

  # Step 1: Convert fork to total
  converted <- convert_fork_to_total(
    fish_batch = test_data$fish_batch,
    ind_measure = test_data$ind_measure,
    species_ref = test_data$species_ref,
    conversion_vector = test_conversion_vector(),
    fishbase_length_length = mock_fishbase_length_length(),
    verbose = FALSE
  )

  # Step 2: Remove impossible lengths
  cleaned <- remove_impossible_lengths(
    ind_measure = converted$ind_measure,
    fish_batch = converted$fish_batch,
    species_ref = test_data$species_ref,
    remove_outliers = TRUE
  )

  # Verify final structure
  expect_type(cleaned, "list")
  expect_named(cleaned, c("ind_measure", "fish_batch", "outliers",
                          "outlier_summary", "batch_outliers"))

  # Verify only UNKNOWN species fork measurements remain
  expect_true(sum(cleaned$fish_batch$length_type == "fork") == 1)

  # Verify outliers were handled
  expect_true(nrow(cleaned$outliers) > 0)
  expect_true(all(is.na(cleaned$ind_measure$size[
    cleaned$ind_measure$measure_id %in% cleaned$outliers$measure_id
    ])))
})

test_that("Function handles empty data frames gracefully", {
  test_data <- create_test_length_data()

  # Empty data frames
  empty_batch <- test_data$fish_batch[0, ]
  empty_ind <- test_data$ind_measure[0, ]

  result <- convert_fork_to_total(
    fish_batch = empty_batch,
    ind_measure = empty_ind,
    species_ref = test_data$species_ref,
    conversion_vector = test_conversion_vector()
  )

  expect_equal(nrow(result$ind_measure), 0)
  expect_equal(nrow(result$fish_batch), 0)
  expect_equal(nrow(result$conversion_log), 0)
})

test_that("Function maintains data integrity", {
  test_data <- create_test_length_data()

  result <- convert_fork_to_total(
    fish_batch = test_data$fish_batch,
    ind_measure = test_data$ind_measure,
    species_ref = test_data$species_ref,
    conversion_vector = test_conversion_vector(),
    fishbase_length_length = mock_fishbase_length_length(),
    verbose = FALSE
  )

  # Check that all original IDs are preserved
  expect_true(all(test_data$ind_measure$measure_id %in% result$ind_measure$measure_id))
  expect_true(all(test_data$fish_batch$batch_id %in% result$fish_batch$batch_id))

  # Check no duplicate IDs
  expect_equal(length(unique(result$ind_measure$measure_id)), nrow(result$ind_measure))
  expect_equal(length(unique(result$fish_batch$batch_id)), nrow(result$fish_batch))
})
