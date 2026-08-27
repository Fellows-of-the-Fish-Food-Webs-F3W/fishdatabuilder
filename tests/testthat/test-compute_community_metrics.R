test_that("compute_community_metrics computes community-level metrics correctly", {
  fish_data <- data.frame(
    operation_id = c(1, 1, 1, 1, 2, 2),
    species_code = c("AAA", "AAA", "BBB", "BBB", "AAA", "CCC"),
    weight_g = c(10, 20, 5, 15, 30, 10)
  )

  operation <- data.frame(
    operation_id = c(1, 2),
    computed_surface = c(100, 50)
  )

  result <- compute_community_metrics(
    fish_data = fish_data,
    operation = operation
  )

  expect_s3_class(result, "tbl_df")

  expect_named(
    result,
    c(
      "operation_id",
      "total_richness",
      "total_abundance",
      "total_biomass_g",
      "richness_per_m2",
      "abundance_per_m2",
      "biomass_g_per_m2",
      "abundance_by_species",
      "biomass_by_species"
    )
  )

  expect_equal(nrow(result), 2)

  operation_1 <- result |>
    dplyr::filter(operation_id == 1)

  expect_equal(operation_1$total_richness, 2)
  expect_equal(operation_1$total_abundance, 4)
  expect_equal(operation_1$total_biomass_g, 50)

  expect_equal(operation_1$richness_per_m2, 0.02)
  expect_equal(operation_1$abundance_per_m2, 0.04)
  expect_equal(operation_1$biomass_g_per_m2, 0.5)
})

test_that("abundance_by_species contains correct species-level metrics", {
  fish_data <- data.frame(
    operation_id = c(1, 1, 1, 1),
    species_code = c("AAA", "AAA", "BBB", "BBB"),
    weight_g = c(10, 20, 5, 15)
  )

  operation <- data.frame(
    operation_id = 1,
    computed_surface = 100
  )

  result <- compute_community_metrics(
    fish_data = fish_data,
    operation = operation
  )

  abundance <- result$abundance_by_species[[1]]

  expected <- tibble::tibble(
    species_code = c("AAA", "BBB"),
    total_abundance = c(2L, 2L),
    abundance_per_m2 = c(0.02, 0.02)
  )

  expect_equal(
    abundance,
    expected
  )
})

test_that("biomass_by_species contains correct species-level metrics", {
  fish_data <- data.frame(
    operation_id = c(1, 1, 1, 1),
    species_code = c("AAA", "AAA", "BBB", "BBB"),
    weight_g = c(10, 20, 5, 15)
  )

  operation <- data.frame(
    operation_id = 1,
    computed_surface = 100
  )

  result <- compute_community_metrics(
    fish_data = fish_data,
    operation = operation
  )

  biomass <- result$biomass_by_species[[1]]

  expected <- tibble::tibble(
    species_code = c("AAA", "BBB"),
    total_biomass_g = c(30, 20),
    biomass_g_per_m2 = c(0.30, 0.20)
  )

  expect_equal(
    biomass,
    expected
  )
})

test_that("species-level metrics sum to community-level metrics", {
  fish_data <- data.frame(
    operation_id = c(1, 1, 1, 2, 2),
    species_code = c("AAA", "AAA", "BBB", "AAA", "CCC"),
    weight_g = c(10, 20, 5, 30, 10)
  )

  operation <- data.frame(
    operation_id = c(1, 2),
    computed_surface = c(100, 50)
  )

  result <- compute_community_metrics(
    fish_data = fish_data,
    operation = operation
  )

  abundance_from_species <- purrr::map_dbl(
    result$abundance_by_species,
    \(x) sum(x$total_abundance)
  )

  biomass_from_species <- purrr::map_dbl(
    result$biomass_by_species,
    \(x) sum(x$total_biomass_g)
  )

  expect_equal(
    abundance_from_species,
    result$total_abundance
  )

  expect_equal(
    biomass_from_species,
    result$total_biomass_g
  )
})

test_that("surface-standardized metrics are NA when computed_surface is zero", {
  fish_data <- data.frame(
    operation_id = c(1, 1, 1),
    species_code = c("AAA", "AAA", "BBB"),
    weight_g = c(10, 20, 5)
  )

  operation <- data.frame(
    operation_id = 1,
    computed_surface = 0
  )

  expect_warning(
    result <- compute_community_metrics(
      fish_data = fish_data,
      operation = operation
    ),
    "1 operation\\(s\\).*computed_surface"
  )

  expect_equal(result$total_richness, 2)
  expect_equal(result$total_abundance, 3)
  expect_equal(result$total_biomass_g, 35)

  expect_true(is.na(result$richness_per_m2))
  expect_true(is.na(result$abundance_per_m2))
  expect_true(is.na(result$biomass_g_per_m2))

  expect_true(
    all(is.na(
      result$abundance_by_species[[1]]$abundance_per_m2
    ))
  )

  expect_true(
    all(is.na(
      result$biomass_by_species[[1]]$biomass_g_per_m2
    ))
  )
})

test_that("surface-standardized metrics are NA when computed_surface is missing", {
  fish_data <- data.frame(
    operation_id = c(1, 1),
    species_code = c("AAA", "BBB"),
    weight_g = c(10, 20)
  )

  operation <- data.frame(
    operation_id = 1,
    computed_surface = NA_real_
  )

  expect_warning(
    result <- compute_community_metrics(
      fish_data = fish_data,
      operation = operation
    ),
    "1 operation\\(s\\).*computed_surface"
  )

  expect_equal(result$total_richness, 2)
  expect_equal(result$total_abundance, 2)
  expect_equal(result$total_biomass_g, 30)

  expect_true(is.na(result$richness_per_m2))
  expect_true(is.na(result$abundance_per_m2))
  expect_true(is.na(result$biomass_g_per_m2))
})

test_that("surface-standardized metrics are NA when computed_surface is non-finite", {
  fish_data <- data.frame(
    operation_id = c(1, 1),
    species_code = c("AAA", "BBB"),
    weight_g = c(10, 20)
  )

  operation <- data.frame(
    operation_id = 1,
    computed_surface = Inf
  )

  expect_warning(
    result <- compute_community_metrics(
      fish_data = fish_data,
      operation = operation
    ),
    "1 operation\\(s\\).*computed_surface"
  )

  expect_equal(result$total_richness, 2)
  expect_equal(result$total_abundance, 2)
  expect_equal(result$total_biomass_g, 30)

  expect_true(is.na(result$richness_per_m2))
  expect_true(is.na(result$abundance_per_m2))
  expect_true(is.na(result$biomass_g_per_m2))
})

test_that("warning reports the number of operations with invalid sampled surface", {
  fish_data <- data.frame(
    operation_id = c(1, 2, 3),
    species_code = c("AAA", "BBB", "CCC"),
    weight_g = c(10, 20, 30)
  )

  operation <- data.frame(
    operation_id = c(1, 2, 3),
    computed_surface = c(100, 0, NA_real_)
  )

  expect_warning(
    result <- compute_community_metrics(
      fish_data = fish_data,
      operation = operation
    ),
    "2 operation\\(s\\).*Surface-standardized metrics"
  )

  expect_false(is.na(result$abundance_per_m2[result$operation_id == 1]))

  expect_true(
    all(is.na(
      result$abundance_per_m2[result$operation_id %in% c(2, 3)]
    ))
  )
})

test_that("missing weights generate a warning and are excluded only from biomass", {
  fish_data <- data.frame(
    operation_id = c(1, 1, 1),
    species_code = c("AAA", "AAA", "BBB"),
    weight_g = c(10, NA_real_, 5)
  )

  operation <- data.frame(
    operation_id = 1,
    computed_surface = 100
  )

  expect_warning(
    result <- compute_community_metrics(
      fish_data = fish_data,
      operation = operation
    ),
    "1 individual\\(s\\).*missing values.*retained in richness and abundance.*excluded from biomass"
  )

  expect_equal(result$total_richness, 2)

  # The individual with missing weight is still counted.
  expect_equal(result$total_abundance, 3)

  # Only available weights contribute to biomass.
  expect_equal(result$total_biomass_g, 15)

  expect_equal(result$richness_per_m2, 0.02)
  expect_equal(result$abundance_per_m2, 0.03)
  expect_equal(result$biomass_g_per_m2, 0.15)

  abundance <- result$abundance_by_species[[1]]

  expect_equal(
    abundance$total_abundance[
      abundance$species_code == "AAA"
    ],
    2
  )

  biomass <- result$biomass_by_species[[1]]

  expect_equal(
    biomass$total_biomass_g[
      biomass$species_code == "AAA"
    ],
    10
  )
})

test_that("zero weights generate a warning and contribute zero to biomass", {
  fish_data <- data.frame(
    operation_id = c(1, 1, 1),
    species_code = c("AAA", "AAA", "BBB"),
    weight_g = c(10, 0, 5)
  )

  operation <- data.frame(
    operation_id = 1,
    computed_surface = 100
  )

  expect_warning(
    result <- compute_community_metrics(
      fish_data = fish_data,
      operation = operation
    ),
    "1 individual\\(s\\).*zero values.*retained in richness and abundance.*contribute zero to biomass"
  )

  expect_equal(result$total_richness, 2)
  expect_equal(result$total_abundance, 3)
  expect_equal(result$total_biomass_g, 15)

  expect_equal(result$richness_per_m2, 0.02)
  expect_equal(result$abundance_per_m2, 0.03)
  expect_equal(result$biomass_g_per_m2, 0.15)
})

test_that("missing and zero weights are both reported", {
  fish_data <- data.frame(
    operation_id = c(1, 1, 1),
    species_code = c("AAA", "AAA", "BBB"),
    weight_g = c(NA_real_, 0, 5)
  )

  operation <- data.frame(
    operation_id = 1,
    computed_surface = 100
  )

  expect_warning(
    result <- compute_community_metrics(
      fish_data = fish_data,
      operation = operation
    ),
    "missing values.*zero values"
  )

  expect_equal(result$total_richness, 2)
  expect_equal(result$total_abundance, 3)
  expect_equal(result$total_biomass_g, 5)
})

test_that("multiple missing and zero weights are correctly reported", {
  fish_data <- data.frame(
    operation_id = c(1, 1, 1, 1, 1),
    species_code = c("AAA", "AAA", "BBB", "BBB", "CCC"),
    weight_g = c(NA_real_, NA_real_, 0, 0, 5)
  )

  operation <- data.frame(
    operation_id = 1,
    computed_surface = 100
  )

  expect_warning(
    result <- compute_community_metrics(
      fish_data = fish_data,
      operation = operation
    ),
    "2 individual\\(s\\).*missing values.*2 individual\\(s\\).*zero values"
  )

  expect_equal(result$total_richness, 3)
  expect_equal(result$total_abundance, 5)
  expect_equal(result$total_biomass_g, 5)
})

test_that("fish_data must be a data frame", {
  operation <- data.frame(
    operation_id = 1,
    computed_surface = 100
  )

  expect_error(
    compute_community_metrics(
      fish_data = c(1, 2, 3),
      operation = operation
    ),
    "fish_data must be a data frame"
  )
})

test_that("operation must be a data frame", {
  fish_data <- data.frame(
    operation_id = 1,
    species_code = "AAA",
    weight_g = 10
  )

  expect_error(
    compute_community_metrics(
      fish_data = fish_data,
      operation = c(1, 2, 3)
    ),
    "operation must be a data frame"
  )
})

test_that("required fish_data columns are checked", {
  fish_data <- data.frame(
    operation_id = 1,
    species_code = "AAA"
  )

  operation <- data.frame(
    operation_id = 1,
    computed_surface = 100
  )

  expect_error(
    compute_community_metrics(
      fish_data = fish_data,
      operation = operation
    ),
    "weight_g"
  )
})

test_that("required operation columns are checked", {
  fish_data <- data.frame(
    operation_id = 1,
    species_code = "AAA",
    weight_g = 10
  )

  operation <- data.frame(
    operation_id = 1
  )

  expect_error(
    compute_community_metrics(
      fish_data = fish_data,
      operation = operation
    ),
    "computed_surface"
  )
})

test_that("missing operation_id values in fish_data are rejected", {
  fish_data <- data.frame(
    operation_id = c(1, NA_integer_),
    species_code = c("AAA", "BBB"),
    weight_g = c(10, 20)
  )

  operation <- data.frame(
    operation_id = 1,
    computed_surface = 100
  )

  expect_error(
    compute_community_metrics(
      fish_data = fish_data,
      operation = operation
    ),
    "operation_id contains missing values"
  )
})

test_that("missing species_code values are rejected", {
  fish_data <- data.frame(
    operation_id = c(1, 1),
    species_code = c("AAA", NA_character_),
    weight_g = c(10, 20)
  )

  operation <- data.frame(
    operation_id = 1,
    computed_surface = 100
  )

  expect_error(
    compute_community_metrics(
      fish_data = fish_data,
      operation = operation
    ),
    "species_code contains missing values"
  )
})

test_that("duplicated operation_id values in operation are rejected", {
  fish_data <- data.frame(
    operation_id = 1,
    species_code = "AAA",
    weight_g = 10
  )

  operation <- data.frame(
    operation_id = c(1, 1),
    computed_surface = c(100, 100)
  )

  expect_error(
    compute_community_metrics(
      fish_data = fish_data,
      operation = operation
    ),
    "duplicated operation_id"
  )
})

test_that("operations missing from operation data are rejected", {
  fish_data <- data.frame(
    operation_id = c(1, 2),
    species_code = c("AAA", "BBB"),
    weight_g = c(10, 20)
  )

  operation <- data.frame(
    operation_id = 1,
    computed_surface = 100
  )

  expect_error(
    compute_community_metrics(
      fish_data = fish_data,
      operation = operation
    ),
    "No computed_surface found"
  )
})
