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

