test_that("input validation works correctly", {
  # Test data frame validation
  expect_error(
    clean_description_operation_aspe(op_description = "not_a_df"),
    "must be a data frame"
  )
  expect_error(
    clean_description_operation_aspe(ref_isolation = "not_a_df"),
    "must be a data frame"
  )
  expect_error(
    clean_description_operation_aspe(ref_prospection = "not_a_df"),
    "must be a data frame"
  )

  # Test missing columns
  bad_op <- data.frame(wrong_col = 1:10)
  expect_error(
    clean_description_operation_aspe(op_description = bad_op),
    "missing required columns"
  )
  expect_error(
    clean_description_operation_aspe(ref_isolation = bad_op),
    "missing required columns"
  )
  expect_error(
    clean_description_operation_aspe(ref_prospection = bad_op),
    "missing required columns"
  )
})

test_that("clean_operation_aspe validates inputs strictly", {
  # Create good test data
  good_data <- list(
    op = data.frame(
      ope_id = 1, ope_pro_id = 1, ope_pop_id = 1,
      ope_date = "2023-01-01 12:00:00", ope_sans_poisson = "t",
      ope_surface_calculee = 100
    ),
    op_objective = data.frame(opo_ope_id = 1, opo_obj_id = 1),
    ref_objective = data.frame(obj_id = 1, obj_libelle = "test"),
    ref_protocol = data.frame(pro_id = 1, pro_libelle = "test"),
    sampling_point = data.frame(pop_id = 1, pop_sta_id = 101)
  )
  good <- clean_operation_aspe(
    op = good_data$op,
    op_objective = good_data$op_objective,
    ref_objective = good_data$ref_objective,
    ref_protocol = good_data$ref_protocol,
    sampling_point = good_data$sampling_point
  )
  # Test output
  expect_s3_class(good, "data.frame")
  expect_setequal(names(good),
    c("operation_id", "site_id", "date", "objective", "protocol",
    "without_fish", "computed_surface", "date_time")
  )

  # Test data frame validation
  expect_error(
    clean_operation_aspe(op = "not_a_df"),
    "must be a data frame"
  )
  expect_error(
    clean_operation_aspe(op_objective = "not_a_df"),
    "must be a data frame"
  )
  expect_error(
    clean_operation_aspe(ref_objective = "not_a_df"),
    "must be a data frame"
  )
  expect_error(
    clean_operation_aspe(ref_protocol = "not_a_df"),
    "must be a data frame"
  )
  expect_error(
    clean_operation_aspe(sampling_point = "not_a_df"),
    "must be a data frame"
  )
 
  # Test missing columns
  bad_df <- data.frame(wrong_col = 1)  # Missing required columns
  expect_error(
    clean_operation_aspe(op = bad_df),
    "missing required columns"
  )
  expect_error(
    clean_operation_aspe(ref_objective = bad_df),
    "missing required columns"
  )
  expect_error(
    clean_operation_aspe(ref_protocol = bad_df),
    "missing required columns"
  )
  expect_error(
    clean_operation_aspe(sampling_point = bad_df),
    "missing required columns"
  )

})
