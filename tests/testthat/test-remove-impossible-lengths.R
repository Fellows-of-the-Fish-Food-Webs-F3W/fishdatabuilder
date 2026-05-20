test_that("remove_impossible_lengths identifies outliers correctly", {
  test_data <- create_test_length_data()
  
  # Add some unrealistic measurements
  test_data$ind_measure$size[1] <- 800  # PER max 600
  test_data$ind_measure$size[5] <- 1200 # TRF max 1000
  test_data$ind_measure$size[9] <- 700  # PER max 600
  
  result <- remove_impossible_lengths(
    ind_measure = test_data$ind_measure,
    fish_batch = test_data$fish_batch,
    species_ref = test_data$species_ref,
    remove_outliers = TRUE
  )
  
  # Check outliers were detected
  expect_equal(nrow(result$outliers), 3)
  expect_true(800 %in% result$outliers$size)
  expect_true(1200 %in% result$outliers$size)
  
  # Check outliers were replaced with NA
  cleaned_sizes <- result$ind_measure$size
  expect_true(is.na(cleaned_sizes[1]))
  expect_true(is.na(cleaned_sizes[5]))
  expect_true(is.na(cleaned_sizes[9]))
})

test_that("remove_impossible_lengths reconciles S/L batches", {
  test_data <- create_test_length_data()
  
  # Add unrealistic measurements that affect batch stats
  test_data$ind_measure$size[1] <- 800  # PER max 600, affects batch 1
  test_data$ind_measure$size[9] <- 850  # PER max 600, affects batch 4
  
  result <- remove_impossible_lengths(
    ind_measure = test_data$ind_measure,
    fish_batch = test_data$fish_batch,
    species_ref = test_data$species_ref,
    remove_outliers = TRUE
  )
  
  # Check batch 1 (PER, batch_type = "S/L") was updated
  batch1 <- result$fish_batch |> dplyr::filter(batch_id == 1)
  batch4 <- result$fish_batch |> dplyr::filter(batch_id == 4)
  
  # After removing outliers, min/max should be recalculated from remaining measurements
  remaining_sizes <- c(125, 140)  # Original sizes 110,125,140 but 110 was outlier
  expect_equal(batch1$min_length, min(remaining_sizes))
  expect_equal(batch1$max_length, max(remaining_sizes))
  remaining_sizes <- c(125, 175)  # Original sizes 110,125,140 but 110 was outlier
  expect_equal(batch4$min_length, min(remaining_sizes))
  expect_equal(batch4$max_length, max(remaining_sizes))
})

test_that("remove_impossible_lengths handles remove_outliers = FALSE", {
  test_data <- create_test_length_data()
  test_data$ind_measure$size[1] <- 800  # Outlier

  expect_warning(result <- remove_impossible_lengths(
    ind_measure = test_data$ind_measure,
    fish_batch = test_data$fish_batch,
    species_ref = test_data$species_ref,
    remove_outliers = FALSE
  ),
    "Found 1 batches with impossible lengths"
  )

  # Outliers should be flagged but not removed
  expect_equal(nrow(result$outliers), 1)
  expect_equal(result$ind_measure$size[1], 800)  # Not replaced with NA
  expect_equal(result$outlier_summary, data.frame())  # No summary when not removing
})

test_that("remove_impossible_lengths warns about missing max lengths", {
  test_data <- create_test_length_data()
  
  # Remove max length for a species
  test_data$species_ref$maximal_length_mm[5] <- NA
  
  expect_warning(
    result <- remove_impossible_lengths(
      ind_measure = test_data$ind_measure,
      fish_batch = test_data$fish_batch,
      species_ref = test_data$species_ref
    ),
    "Missing maximal length for species: UNKNOWN"
  )
})

test_that("remove_impossible_lengths identifies batch-level outliers", {
  test_data <- create_test_length_data()
  
  # Add unrealistic batch max lengths
  test_data$fish_batch$max_length[1] <- 1000  # PER max 600
  test_data$fish_batch$min_length[2] <- 1200  # TRF max 1000
  test_data$fish_batch <- test_data$fish_batch |>
    rbind(
      tibble::tibble(
        batch_id = 9:10,
        species_code = c("PER", "TRF"),
        length_type = "total",
        batch_type = "G",
        min_length = c(4, 1001),
        max_length = c(1000, 2000)
      )
    )
  
  expect_warning(
    result <- remove_impossible_lengths(
      ind_measure = test_data$ind_measure,
      fish_batch = test_data$fish_batch,
      species_ref = test_data$species_ref,
      remove_outliers = FALSE
    ),
    "Found 2 batches with impossible lengths"
  )
  
  expect_equal(nrow(result$batch_outliers), 2)
  # Because S/L batch min & max are recomputed internally from ind_measure:
  expect_false(1 %in% result$batch_outliers$batch_id)
  expect_false(2 %in% result$batch_outliers$batch_id)

  expect_true(9 %in% result$batch_outliers$batch_id)
  expect_true(10 %in% result$batch_outliers$batch_id)
})

test_that("remove_impossible_lengths validates inputs", {
  test_data <- create_test_length_data()
 
  # Test non-data.frame inputs
  expect_error(
    remove_impossible_lengths(ind_measure = "not_a_df"),
    "ind_measure must be a data frame"
  )
 
  expect_error(
    remove_impossible_lengths(fish_batch = "not_a_df", ind_measure = data.frame()),
    "fish_batch must be a data frame"
  )
 
  expect_error(
    remove_impossible_lengths(
      species_ref = "not_a_df",
      fish_batch = data.frame(),
      ind_measure = data.frame()),
    "species_ref must be a data frame"
  )

  # Test missing required columns
  bad_ind <- test_data$ind_measure |> dplyr::select(-size)
  expect_error(
    remove_impossible_lengths(ind_measure = bad_ind,
      species_ref = data.frame(),
      fish_batch = data.frame()
    ),
    "missing columns: size"
  )
 
})
