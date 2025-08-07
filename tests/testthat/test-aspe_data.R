
# Testing cleaning station data

#TODO: change function to take file path or dir plus file names
test_that("Function returns expected structure", {
  # Create minimal test files in tempdir()
  test_station <- data.frame(
    row.names = "S1",
    sta_site_id = "S1",
    sta_typ_id = 1,
    sta_x = 100,
    sta_y = 200
  )
  test_ref <- data.frame(
    row.names = 1,
    typ_code_epsg = 2154
  )
  
  # Write test files
  write.csv2(test_station, file.path(tempdir(), "station.csv"))
  write.csv2(test_ref, file.path(tempdir(), "ref_type_projection.csv"))
  
  result <- clean_station_aspe(
    file = file.path(tempdir(), "station.csv"),
    ref_file = file.path(tempdir(), "ref_type_projection.csv")
  )
  
  expect_s3_class(result, "data.frame")
  expect_named(result, c("site_id", "x", "y"))
  expect_type(result$site_id, "character")
  expect_type(result$x, "double")
  expect_type(result$y, "double")
})

test_that("CRS conversion works correctly", {
  # Setup similar to above
  result_4326 <- clean_station_aspe(crs_to = 4326)
  result_2154 <- clean_station_aspe(crs_to = 2154)
  
  # WGS84 (4326) should give lon/lat between -180/180 and -90/90
  expect_true(all(result_4326$x >= -180 & result_4326$x <= 180))
  expect_true(all(result_4326$y >= -90 & result_4326$y <= 90))
  
  # Lambert-93 (2154) should give coordinates in meters in French range
  expect_true(all(result_2154$x > 0 & result_2154$x < 1300000))
  expect_true(all(result_2154$y > 6000000 & result_2154$y < 7200000))
})
