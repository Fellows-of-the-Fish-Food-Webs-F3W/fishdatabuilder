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

# Create test point dataset
create_test_point_data <- function() {
  # Point group data
  point_group <- data.frame(
    grp_id = 1:3,
    grp_tgp_id = c(1, 2, 1),
    grp_name = c("Group A", "Group B", "Group C")
  )
 
  # Reference data
  ref_point_group <- data.frame(
    tgp_id = 1:2,
    tgp_libelle = c("Type A", "Type B")
  )
 
  list(point_group = point_group, ref_point_group = ref_point_group)
}

# Mock translation function
mock_replacement_point_type_label <- function() {
  c("Type A" = "Translated Type A", "Type B" = "Translated Type B")
}

test_that("cleaning_point_group validates input data frames", {
  test_data <- create_test_point_data()
 
  # Test non-data.frame inputs
  expect_error(
    cleaning_point_group(point_group = "not_a_df"),
    "point_group must be a data frame"
  )
 
  expect_error(
    cleaning_point_group(ref_point_group = list(1, 2, 3)),
    "ref_point_group must be a data frame"
  )
})

test_that("cleaning_point_group validates required columns in point_group", {
  test_data <- create_test_point_data()
 
  # Test missing grp_tgp_id column
  bad_point_group <- test_data$point_group %>% dplyr::select(-grp_tgp_id)
 
  expect_error(
    cleaning_point_group(point_group = bad_point_group),
    "point_group missing grp_tgp_id column"
  )
})

test_that("cleaning_point_group validates required columns in ref_point_group", {
  test_data <- create_test_point_data()
 
  # Test missing tgp_id column
  bad_ref1 <- test_data$ref_point_group %>% dplyr::select(-tgp_id)
  expect_error(
    cleaning_point_group(ref_point_group = bad_ref1),
    "missing required columns: tgp_id"
  )
 
  # Test missing tgp_libelle column
  bad_ref2 <- test_data$ref_point_group %>% dplyr::select(-tgp_libelle)
  expect_error(
    cleaning_point_group(ref_point_group = bad_ref2),
    "missing required columns: tgp_libelle"
  )
 
  # Test missing both columns
  bad_ref3 <- data.frame(wrong_col = 1:2)
  expect_error(
    cleaning_point_group(ref_point_group = bad_ref3),
    "missing required columns: tgp_id, tgp_libelle"
  )
})

test_that("cleaning_point_group validates input data frames", {
  test_data <- create_test_point_data()
 
  # Test non-data.frame inputs
  expect_error(
    cleaning_point_group(point_group = "not_a_df"),
    "point_group must be a data frame"
  )
 
  expect_error(
    cleaning_point_group(ref_point_group = list(1, 2, 3)),
    "ref_point_group must be a data frame"
  )
})

test_that("cleaning_point_group validates required columns in point_group", {
  test_data <- create_test_point_data()
 
  # Test missing grp_tgp_id column
  bad_point_group <- test_data$point_group %>% dplyr::select(-grp_tgp_id)
 
  expect_error(
    cleaning_point_group(point_group = bad_point_group),
    "point_group missing grp_tgp_id column"
  )
})

test_that("cleaning_point_group validates required columns in ref_point_group", {
  test_data <- create_test_point_data()
 
  # Test missing tgp_id column
  bad_ref1 <- test_data$ref_point_group %>% dplyr::select(-tgp_id)
  expect_error(
    cleaning_point_group(ref_point_group = bad_ref1),
    "missing required columns: tgp_id"
  )
 
  # Test missing tgp_libelle column
  bad_ref2 <- test_data$ref_point_group %>% dplyr::select(-tgp_libelle)
  expect_error(
    cleaning_point_group(ref_point_group = bad_ref2),
    "missing required columns: tgp_libelle"
  )
 
  # Test missing both columns
  bad_ref3 <- data.frame(wrong_col = 1:2)
  expect_error(
    cleaning_point_group(ref_point_group = bad_ref3),
    "missing required columns: tgp_id, tgp_libelle"
  )
})

test_that("cleaning_point_group handles missing reference matches", {
  test_data <- create_test_point_data()
 
  # Point group with non-existent reference ID
  point_group_with_missing <- test_data$point_group
  point_group_with_missing$grp_tgp_id[3] <- 99  # ID that doesn't exist in reference
 
  result <- cleaning_point_group(
    point_group = point_group_with_missing,
    ref_point_group = test_data$ref_point_group
  )
 
  # Should have NA for missing reference
  expect_true(is.na(result$point_type[3]))
})

test_that("cleaning_point_group preserves all original columns", {
  test_data <- create_test_point_data()
 
  # Add extra columns to point group
  point_group_extra <- test_data$point_group %>%
    dplyr::mutate(extra_col = "test", another_col = 42)
 
  result <- cleaning_point_group(
    point_group = point_group_extra,
    ref_point_group = test_data$ref_point_group
  )
 
  # Should preserve all original columns except grp_tgp_id
  expect_named(result, c("grp_id", "point_type", "grp_name", "extra_col", "another_col"))
  expect_equal(result$extra_col, rep("test", 3))
  expect_equal(result$another_col, rep(42, 3))
})

test_that("cleaning_point_group maintains row order", {
  test_data <- create_test_point_data()
 
  # Shuffle point group rows
  point_group_shuffled <- test_data$point_group[sample(nrow(test_data$point_group)), ]
 
  result <- cleaning_point_group(
    point_group = point_group_shuffled,
    ref_point_group = test_data$ref_point_group
  )
 
  # Should maintain the shuffled order (not reorder by join)
  expect_equal(result$grp_id, point_group_shuffled$grp_id)
})
