test_that("clean_station_aspe works with default inputs", {
  
  result <- clean_station_aspe(
    station = create_test_station(),
    ref_coordinates = create_test_ref()
  )
  expect_s3_class(result, "data.frame")
  expect_named(result, c("site_id", "x", "y"))
  expect_equal(nrow(result), 2)
})

test_that("clean_station_aspe handles custom CRS", {
  result <- clean_station_aspe(
    station = create_test_station(),
    ref_coordinates = create_test_ref(),
    crs_to = 2154)
  # Check that coordinates are in expected range for Lambert-93
  expect_true(all(result$x > 0 & result$x < 1300000))
  expect_true(all(result$y > 6000000 & result$y < 7200000))
})

test_that("clean_station_aspe works with custom data frames", {
  custom_station <- data.frame(
    site_id = c("C1", "C2"),
    typ_id = c(1, 1),
    x = c(10, 20),
    y = c(30, 40)
  )
  
  custom_ref <- data.frame(
    typ_id = 1,
    typ_code_epsg = 4326
  )
  
  result <- clean_station_aspe(
    station = custom_station,
    ref_coordinates = custom_ref
  )
  expect_equal(result$site_id, c("C1", "C2"))
})

test_that("clean_station_aspe handles both raw and clean column names", {
  # Test with raw (sta_ prefixed) columns
  raw_station <- create_test_station()
  ref <- create_test_ref()
  
  result_raw <- clean_station_aspe(station = raw_station, ref_coordinates = ref)
  
  # Test with clean columns
  clean_station <- raw_station %>%
    rename_with(~gsub("sta_", "", .x))
  
  result_clean <- clean_station_aspe(station = clean_station, ref_coordinates = ref)
  
  expect_equal(result_raw, result_clean)
})


test_that("clean_station_aspe validates inputs", {
  good_station <- create_test_station()
  good_ref <- create_test_ref()
  
  # Invalid station data
  expect_error(
    clean_station_aspe(
      station = "not a data.frame",
      ref_coordinates = good_ref
    ),
    "`station` must be a data frame"
  )
  
  # Missing required columns
  bad_station <- good_station %>% select(-sta_typ_id)
  expect_error(
    clean_station_aspe(
      station = bad_station,
      ref_coordinates = good_ref
    ),
    "missing required columns"
  )
  
  # Invalid CRS
  expect_error(
    clean_station_aspe(
      station = good_station,
      ref_coordinates = good_ref,
      crs_to = "invalid"),
    "`crs_to` must be a single numeric EPSG code"
  )
  expect_error(
    clean_station_aspe(
      station = good_station,
      ref_coordinates = good_ref,
      crs_to = 1),
    "Invalid CRS: 1"
  )

  expect_error(
    clean_station_aspe(
      station = good_station,
      ref_coordinates = good_ref,
      crs_to = c(4329, 2154)),
    "`crs_to` must be a single numeric EPSG code"
  )

})

test_that("clean_station_aspe handles empty inputs", {
  empty_station <- data.frame(
    sta_site_id = character(),
    sta_typ_id = integer(),
    sta_x = numeric(),
    sta_y = numeric()
  )
 
  empty_ref <- data.frame(
    typ_id = integer(),
    typ_code_epsg = integer()
  )
 
  result <- clean_station_aspe(
    station = empty_station,
    ref_coordinates = empty_ref
  )
  expect_equal(nrow(result), 0)
})

test_that("clean_station_aspe handles NA coordinates", {
  na_station <- create_test_station()
  na_station$sta_x[1] <- NA
 
  # Expect error bc sf::st_as_sf() does not handle missing coordinates
  expect_error(
      result <- clean_station_aspe(
        station = na_station,
        ref_coordinates = create_test_ref()
    ),
    "NA values detected in coordinate columns"
  )
  # Result object is not produced so I commented:
  #expect_true(is.na(result$x[1]))
})

test_that("end-to-end workflow works", {
  
  # Get raw data
  raw_station <- create_test_station()
  raw_ref <- create_test_ref()
  
  # Process data
  clean_data <- clean_station_aspe(
    station = raw_station,
    ref_coordinates = raw_ref,
    crs_to = 4326
  )
  
  # Verify output
  expect_s3_class(clean_data, "data.frame")
  expect_equal(nrow(clean_data), nrow(raw_station))
  expect_true(all(c("x", "y") %in% names(clean_data)))
})
