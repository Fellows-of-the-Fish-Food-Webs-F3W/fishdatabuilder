test_that("convert_fork_to_total prioritizes manual over FishBase when manual_priority = TRUE", {
  test_data <- create_test_length_data()

  species_fork <- test_data$fish_batch |>
    dplyr::filter(length_type %in% "fork") |>
    dplyr::distinct(species_code) |>
    dplyr::left_join(
      dplyr::select(test_data$species_ref, species_code, latin_name),
      by = "species_code"
    )

  conversion_coeffs <- build_conversion_coefficients(
    species_fork = species_fork,
    fishbase_length_length = mock_fishbase_length_length(),
    conversion_vector = test_conversion_vector(),
    convert_intercept_cm2mm = TRUE,
    manual_priority = FALSE,
    verbose = FALSE
  )
  expect_equal(
    conversion_coeffs$coeffs |> dplyr::arrange(species_code),
    tibble::tibble(
      species_code = c("ALA", "LOF", "PER", "TRF"),
      b = c(1.03, 1.00, 1.01, 1.02),
      a = 0,
      source = c("FishBase", "Manual", "FishBase", "FishBase")
    )
  )

  result <- convert_fork_to_total(
    fish_batch = test_data$fish_batch,
    ind_measure = test_data$ind_measure,
    species_ref = test_data$species_ref,
    conversion_vector = test_conversion_vector(),  # PER = 1.05
    fishbase_length_length = mock_fishbase_length_length(),  # PER = 0.95
    manual_priority = TRUE,
    verbose = FALSE
  )

  # Check structure
  expect_type(result, "list")
  expect_named(result, c("ind_measure", "fish_batch", "conversion_log", "unconverted_species", "source_log"))
  
  # Check that fork measurements were converted
  converted_batches <- result$fish_batch |>
    dplyr::filter(batch_id %in% c(1, 2, 3, 5))
  
  # PER (batch 1): 100-150 should become 105-157.5 (b=1.05)
  per_batch <- converted_batches |> dplyr::filter(batch_id == 1)
  expect_equal(per_batch$min_length, 105)
  expect_equal(per_batch$max_length, 157.5)
  expect_equal(per_batch$length_type, "total")
  
  # Check individual measurements were converted
  converted_measurements <- result$ind_measure |>
    dplyr::filter(batch_id %in% c(1, 2, 3, 5))
  
  # PER measurements (batch 1): 110, 125, 140 should become 115.5, 131.25, 147
  per_meas <- converted_measurements |> dplyr::filter(batch_id == 1)
  expect_equal(per_meas$size, c(115.5, 131.25, 147))
 
  # Source log should show manual was used for PER
  expect_equal(
    result$source_log$source_used[result$source_log$species_code == "PER"],
    "Manual"
  )
})

test_that("convert_fork_to_total prioritizes FishBase over manual when manual_priority = FALSE", {
  test_data <- create_test_length_data()

  species_fork <- test_data$fish_batch |>
    dplyr::filter(length_type %in% "fork") |>
    dplyr::distinct(species_code) |>
    dplyr::left_join(
      dplyr::select(test_data$species_ref, species_code, latin_name),
      by = "species_code"
    )

  conversion_coeffs <- build_conversion_coefficients(
    species_fork = species_fork,
    fishbase_length_length = mock_fishbase_length_length(),
    conversion_vector = test_conversion_vector(),
    convert_intercept_cm2mm = TRUE,
    manual_priority = TRUE,
    verbose = FALSE
  )
  expect_equal(
    conversion_coeffs$coeffs |> dplyr::arrange(species_code),
    tibble::tibble(
      species_code = c("ALA", "LOF", "PER", "TRF"),
      a = 0,
      b = c(1.09, 1.00, 1.05, 1.04),
      source = "Manual"
    )
  )
 
  result <- convert_fork_to_total(
    fish_batch = test_data$fish_batch,
    ind_measure = test_data$ind_measure,
    species_ref = test_data$species_ref,
    conversion_vector = test_conversion_vector(),  # PER = 1.05
    fishbase_length_length = mock_fishbase_length_length(),  # PER = 1.01
    manual_priority = FALSE,
    verbose = FALSE
  )
  
  # Should use FishBase coefficient (0.95)
  per_batch <- result$fish_batch |> dplyr::filter(batch_id == 1)
  expect_equal(per_batch$min_length, 100.0 * 1.01)
  expect_equal(per_batch$max_length, 150.0 * 1.01)
  
  # Source log should show FishBase was used for PER
  expect_equal(
    result$source_log$source_used[result$source_log$species_code == "PER"],
    "FishBase"
  )
})

test_that("convert_fork_to_total handles missing species gracefully", {
  test_data <- create_test_length_data()

  species_fork <- test_data$fish_batch |>
    dplyr::filter(length_type %in% "fork") |>
    dplyr::distinct(species_code) |>
    dplyr::left_join(
      dplyr::select(test_data$species_ref, species_code, latin_name),
      by = "species_code"
    )
  conversion_coeffs <- build_conversion_coefficients(
    species_fork = species_fork,
    fishbase_length_length = mock_fishbase_length_length(),
    conversion_vector = test_conversion_vector(),
    convert_intercept_cm2mm = TRUE,
    manual_priority = TRUE,
    verbose = FALSE
  )
  expect_equal(conversion_coeffs$missing_species, "UNKNOWN")
  result <- convert_fork_to_total(
    fish_batch = test_data$fish_batch,
    ind_measure = test_data$ind_measure,
    species_ref = test_data$species_ref,
    conversion_vector = test_conversion_vector(),  # PER = 1.05
    fishbase_length_length = mock_fishbase_length_length(),  # PER = 0.95
    manual_priority = FALSE,
    verbose = FALSE
  )

  # When two species are absent
  conversion_coeffs <- build_conversion_coefficients(
    species_fork = species_fork,
    fishbase_length_length = mock_fishbase_length_length(),
    conversion_vector = NULL,
    convert_intercept_cm2mm = TRUE,
    manual_priority = TRUE,
    verbose = FALSE
  )
  expect_equal(conversion_coeffs$missing_species, c("LOF","UNKNOWN"))
 
  result <- convert_fork_to_total(
    fish_batch = test_data$fish_batch,
    ind_measure = test_data$ind_measure,
    species_ref = test_data$species_ref,
    fishbase_length_length = mock_fishbase_length_length(),
    conversion_vector = NULL,
    convert_intercept_cm2mm = TRUE,
    manual_priority = FALSE,
    verbose = FALSE
  )

# UNKNOWN fork measurements should NOT be converted
  unknown_batch <- result$fish_batch |> dplyr::filter(batch_id == 6)
  expect_equal(unknown_batch$min_length, 80)
  expect_equal(unknown_batch$max_length, 100)
  expect_equal(unknown_batch$length_type, "fork")
 
})


test_that("convert_fork_to_total warns about missing conversion coefficients", {
  test_data <- create_test_length_data()

  expect_warning(
    result <- convert_fork_to_total(
      fish_batch = test_data$fish_batch,
      ind_measure = test_data$ind_measure,
      species_ref = test_data$species_ref,
      conversion_vector = c(),  # Empty manual vector
      fishbase_length_length = tibble::tibble(
        Species = c("pikachuuuuuu"),
        Length1 = c("TL"),
        Length2 = c("FL"),
        a = c(0.5),   # 0.5 cm intercept
        b = c(1.05)),
      manual_priority = TRUE,
      convert_intercept_cm2mm = FALSE
    ),
    "No conversion coefficients for species: PER, TRF, ALA, LOF, UNKNOWN"
  )
  expect_equal(result$ind_measure |> select(-source) |> dplyr::arrange(measure_id),
    test_data$ind_measure)
  expect_equal(result$fish_batch |> select(-source) |> dplyr::arrange(batch_id),
    test_data$fish_batch)
})

test_that("convert_fork_to_total handles intercept conversion correctly", {
  test_data <- create_test_length_data()
  
  # Create FishBase data with intercept
  fb_with_intercept <- data.frame(
    Species = c("Perca fluviatilis"),
    Length1 = c("TL"),
    Length2 = c("FL"),
    a = c(0.5),   # 0.5 cm intercept
    b = c(0.95),
    stringsAsFactors = FALSE
  )
  
  # Test with intercept conversion (cm to mm)
  convert_fork_to_total(
      fish_batch = test_data$fish_batch |> dplyr::filter(species_code == "PER"),
      ind_measure = test_data$ind_measure |> dplyr::filter(species_code == "PER"),
      species_ref = test_data$species_ref,
      conversion_vector = c(),
      fishbase_length_length = fb_with_intercept,
      convert_intercept_cm2mm = TRUE
    ) |>
    expect_warning("Some batch maximum lengths decreased after conversion") |>
    expect_warning("Some individual measurements decreased after conversion")
    result_cm <- suppressWarnings(
    convert_fork_to_total(
      fish_batch = test_data$fish_batch |> dplyr::filter(species_code == "PER"),
      ind_measure = test_data$ind_measure |> dplyr::filter(species_code == "PER"),
      species_ref = test_data$species_ref,
      conversion_vector = c(),
      fishbase_length_length = fb_with_intercept,
      convert_intercept_cm2mm = TRUE,
      verbose = FALSE
  ))
  # a = 0.5 cm = 5 mm, b = 0.95
  # TL = 5 + 0.95 * FL
  # For FL = 100: TL = 5 + 95 = 100
  per_batch_cm <- result_cm$fish_batch |> dplyr::filter(batch_id == 1)
  expect_equal(per_batch_cm$min_length, 100)  # 5 + 95 = 100
  
  # Test without intercept conversion (a remains in cm)
  result_no <- convert_fork_to_total(
    fish_batch = test_data$fish_batch |> dplyr::filter(species_code == "PER"),
    ind_measure = test_data$ind_measure |> dplyr::filter(species_code == "PER"),
    species_ref = test_data$species_ref,
    conversion_vector = c(),
    fishbase_length_length = fb_with_intercept,
    convert_intercept_cm2mm = FALSE
  ) |>
    expect_warning("Some batch minimum lengths decreased after conversion") |>
    expect_warning("Some individual measurements decreased after conversion") |>
    expect_warning("Some batch maximum lengths decreased after conversion")

  result_no <- convert_fork_to_total(
    fish_batch = test_data$fish_batch |> dplyr::filter(species_code == "PER"),
    ind_measure = test_data$ind_measure |> dplyr::filter(species_code == "PER"),
    species_ref = test_data$species_ref,
    conversion_vector = c(),
    fishbase_length_length = fb_with_intercept,
    convert_intercept_cm2mm = FALSE
  ) |>
    suppressWarnings()
  # a = 0.5 cm, b = 0.95, but FL in mm
  # TL = 0.5 + 0.95 * (FL/10) ??? Actually FishBase expects FL in cm
  # This is why conversion is important!
  per_batch_no <- result_no$fish_batch |> dplyr::filter(batch_id == 1)
  # The result will be off by factor of 10
  expect_false(per_batch_no$min_length == 100)
})
