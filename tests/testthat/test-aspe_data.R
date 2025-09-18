test_that("get_raw_station_aspe returns expected structure", {
  # Fake data that matches your real structure
  fake_station <- data.frame(
    sta_id = c("S1", "S2"),
    sta_code_sandre = c("001", "002"),
    sta_typ_id = c(1, 2),
    sta_coordonnees_x = c(100, 200),
    sta_coordonnees_y = c(300, 400),
    row.names = 1:2
  )

  # Override the internal read_raw_data function JUST for this test
  # local({
  #   # Temporarily replace the real function
  #   assignInNamespace(
  #     "read_raw_data",
  #     function(file_name) fake_station,
  #     ns = "fishdatabuilder"
  #   )
  #
  #   result <- get_raw_station_aspe()
  #   expect_s3_class(result, "data.frame")
  #   expect_equal(nrow(result), 2)
  #   expect_true("sta_id" %in% names(result))
  # })
})
