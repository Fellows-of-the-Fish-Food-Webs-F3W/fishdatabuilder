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
  expect_named(result, c("batch_id", "species_code", "size_mm", "measured"))
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


# Get batch type lookup
batch_type_lookup <- function(fish_batch) {
  stats::setNames(fish_batch$batch_type, fish_batch$batch_id)
}

test_that("All batch types process correctly", {
  td <- create_test_sanitized_data()
  result <- generate_individual_sizes(td, seed = 123, verbose = FALSE)
  lookup <- batch_type_lookup(td$fish_batch)
  
  # Add batch type to result
  result$batch_type <- lookup[as.character(result$batch_id)]
  
  # Check row counts match expectations
  expect_equal(nrow(result), sum(td$fish_batch$number))
  
  # Type G: exactly 2 measured per batch
  g_counts <- result |> dplyr::filter(batch_type == "G") |> dplyr::count(batch_id, measured)
  expect_true(all(g_counts$n[g_counts$measured] == 2))
  
  # Type I & N: all measured
  in_counts <- result |> dplyr::filter(batch_type %in% c("I", "N")) |> dplyr::count(batch_id, measured)
  expect_true(all(in_counts$measured))
  
  # Type S/L: measured count matches ind_measure
  for (bid in unique(result$batch_id[result$batch_type == "S/L"])) {
    expect_equal(sum(result$batch_id == bid & result$measured), sum(td$ind_measure$batch_id == bid))
  }
})

test_that("Measured individuals come first in each batch", {
  td <- create_test_sanitized_data()
  result <- generate_individual_sizes(td, seed = 123, verbose = FALSE)
  
  result <- result |>
    dplyr::group_by(batch_id) |>
    dplyr::mutate(pos = dplyr::row_number()) |>
    dplyr::ungroup()
  
  # For each batch, measured should be in first n positions
  batch_checks <- result |>
    dplyr::group_by(batch_id) |>
    dplyr::summarise(
      n_measured = sum(measured),
      measured_first = all(measured[1:n_measured]),
      imputed_last = if(n_measured < dplyr::n()) all(!measured[(n_measured+1):dplyr::n()]) else TRUE,
      .groups = "drop"
    )
 
  expect_true(all(batch_checks$measured_first))
  expect_true(all(batch_checks$imputed_last))
})

test_that("Type G measured individuals are min and max", {
  td <- create_test_sanitized_data()
  result <- generate_individual_sizes(td, seed = 123, verbose = FALSE)
  
  g_batches <- td$fish_batch |> dplyr::filter(batch_type == "G")
  
  for (i in seq_len(nrow(g_batches))) {
    bid <- g_batches$batch_id[i]
    measured_vals <- result |> dplyr::filter(batch_id == bid, measured) |> dplyr::pull(size_mm)
    expect_equal(sort(measured_vals), c(g_batches$min_length[i], g_batches$max_length[i]))
  }
})

test_that("Type S/L preserves original measured values", {
  td <- create_test_sanitized_data()
  result <- generate_individual_sizes(td, seed = 123, verbose = FALSE)
  
  for (bid in unique(td$ind_measure$batch_id)) {
    result_measured <- result |> dplyr::filter(batch_id == bid, measured) |> dplyr::pull(size_mm)
    original_measured <- td$ind_measure |> dplyr::filter(batch_id == bid) |> dplyr::pull(size)
    expect_equal(sort(result_measured), sort(original_measured))
  }
})

test_that("Seed reproducibility works", {
  td <- create_test_sanitized_data()
  r1 <- generate_individual_sizes(td, seed = 42, verbose = FALSE)
  r2 <- generate_individual_sizes(td, seed = 42, verbose = FALSE)
  r3 <- generate_individual_sizes(td, seed = 123, verbose = FALSE)
  
  expect_equal(r1$size_mm, r2$size_mm)
  expect_false(identical(r1$size_mm, r3$size_mm))
})

test_that("Species maximum lengths are respected", {
  td <- create_test_sanitized_data()
  td$fish_batch$maximal_length_mm[td$fish_batch$species_code == "PER"] <- 200
  
  result <- generate_individual_sizes(td, seed = 123, verbose = FALSE)
  
  per_sizes <- result |> dplyr::filter(species_code == "PER", measured = FALSE)
  expect_true(all(per_sizes$size_mm <= 200))
  expect_true(all(per_sizes$size_mm >= 0))
})

test_that("Edge cases (empty data, n=2 batches) work", {
  td <- create_test_sanitized_data()
  
  # Empty data
  empty <- list(fish_batch = td$fish_batch[0,], ind_measure = td$ind_measure[0,])
  expect_equal(nrow(generate_individual_sizes(empty, verbose = FALSE)), 0)
  
  # Add G batch with n=2 (no imputation)
  n2_batch <- data.frame(batch_id = 9, batch_type = "G", number = 2, species_code = "PER",
                         min_length = 100, max_length = 150, maximal_length_mm = 600)
  td$fish_batch <- rbind(td$fish_batch, n2_batch)
  
  result <- generate_individual_sizes(td, verbose = FALSE)
  batch9 <- result |> dplyr::filter(batch_id == 9)
  expect_equal(nrow(batch9), 2)
  expect_true(all(batch9$measured))
})

test_that("Verbose output works", {
  td <- create_test_sanitized_data()
  expect_message(generate_individual_sizes(td, verbose = TRUE), "Generating Individual Fish Sizes")
  expect_silent(generate_individual_sizes(td, verbose = FALSE))
})

