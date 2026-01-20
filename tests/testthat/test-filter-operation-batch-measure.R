  # TODO: change default values for the test:
  # Write a wrapper function with new default? (based on test data)
  test_filter_operation_batch_measure <- function(
    operation = create_correct_filter_data()$operation,
    op_protocol_to_keep = c("complete", "partial_by_point",  "partial_over_bank"),
    op_objective_to_exclude = vec_op_objective_to_exclude(),
    oldest_sampling_date = "1995-01-01",
    omit_na_site = TRUE,
    point_group = create_correct_filter_data()$point_group,
    min_prop_point_group_on_bank = .999,
    ele_sampling = create_correct_filter_data()$ele_sampling,
    max_passage_number = 1,
    fish_batch = create_correct_filter_data()$fish_batch,
    ind_measure = create_correct_filter_data()$ind_measure
) {

    filter_operation_batch_measure(
      operation = operation,
      op_protocol_to_keep = op_protocol_to_keep,
      op_objective_to_exclude = op_objective_to_exclude,
      oldest_sampling_date = oldest_sampling_date,
      omit_na_site = omit_na_site,
      point_group = point_group,
      min_prop_point_group_on_bank = min_prop_point_group_on_bank,
      ele_sampling = ele_sampling,
      max_passage_number = max_passage_number,
      fish_batch = fish_batch,
      ind_measure = ind_measure
    )

  }

test_that("filter_operation_batch_measure validates data frame inputs", {
  
  # Test non-data.frame operation
  expect_error(
    test_filter_operation_batch_measure(operation = "not_a_df"),
    "`operation` must be a data frame"
  )
  
  # Test non-data.frame point_group
  expect_error(
    test_filter_operation_batch_measure(point_group = list(1, 2, 3)),
    "`point_group` must be a data frame"
  )
  
  # Test non-data.frame ele_sampling
  expect_error(
    test_filter_operation_batch_measure(ele_sampling = matrix(1:4, nrow = 2)),
    "`ele_sampling` must be a data frame"
  )
})

test_that("filter_operation_batch_measure validates protocol parameter", {
  test_data <- create_test_filter_data()
  
  # Test non-character protocol vector
  expect_error(
    test_filter_operation_batch_measure(op_protocol_to_keep = 123),
    "`op_protocol_to_keep` must be a non-empty character vector"
  )
  
  # Test empty protocol vector
  expect_error(
    test_filter_operation_batch_measure(op_protocol_to_keep = character(0)),
    "`op_protocol_to_keep` must be a non-empty character vector"
  )
})

test_that("filter_operation_batch_measure validates objective exclusion parameter", {
  test_data <- create_test_filter_data()
  
  # Test invalid type for objective exclusion
  expect_error(
    test_filter_operation_batch_measure(op_objective_to_exclude = 123),
    "`op_objective_to_exclude` must be a character or logical vector"
  )
  
  # Test list instead of vector
  expect_error(
    test_filter_operation_batch_measure(op_objective_to_exclude = list("A", "B")),
    "`op_objective_to_exclude` must be a character or logical vector"
  )
})

test_that("filter_operation_batch_measure validates date parameter", {
  test_data <- create_test_filter_data()
  
  # Test invalid date format
  expect_error(
    test_filter_operation_batch_measure(oldest_sampling_date = "not-a-date"),
    "`oldest_sampling_date` must be a valid date in YYYY-MM-DD format"
  )
  
  # Test malformed date
  expect_error(
    test_filter_operation_batch_measure(oldest_sampling_date = "2023-13-01"),
    "`oldest_sampling_date` must be a valid date in YYYY-MM-DD format"
  )
})

test_that("filter_operation_batch_measure validates logical parameters", {
  test_data <- create_test_filter_data()
  
  # Test invalid omit_na_site
  expect_error(
    test_filter_operation_batch_measure(omit_na_site = "yes"),
    "`omit_na_site` must be a single logical value"
  )
  
  # Test vector instead of single value
  expect_error(
    test_filter_operation_batch_measure(omit_na_site = c(TRUE, FALSE)),
    "`omit_na_site` must be a single logical value"
  )
})

test_that("filter_operation_batch_measure validates proportion parameter", {
  test_data <- create_test_filter_data()
  
  # Test non-numeric proportion
  expect_error(
    test_filter_operation_batch_measure(min_prop_point_group_on_bank = "high"),
    "`min_prop_point_group_on_bank` must be a numeric value between 0 and 1"
  )
  
  # Test proportion out of range
  expect_error(
    test_filter_operation_batch_measure(min_prop_point_group_on_bank = 1.5),
    "`min_prop_point_group_on_bank` must be a numeric value between 0 and 1"
  )
  
  expect_error(
    test_filter_operation_batch_measure(min_prop_point_group_on_bank = -0.1),
    "`min_prop_point_group_on_bank` must be a numeric value between 0 and 1"
  )
})

test_that("filter_operation_batch_measure validates passage number parameter", {
  test_data <- create_test_filter_data()
  
  # Test non-numeric passage number
  expect_error(
    test_filter_operation_batch_measure(max_passage_number = "first"),
    "`max_passage_number` must be a positive integer"
  )
  
  # Test negative passage number
  expect_error(
    test_filter_operation_batch_measure(max_passage_number = 0),
    "`max_passage_number` must be a positive integer"
  )
  
  expect_error(
    test_filter_operation_batch_measure(max_passage_number = -1),
    "`max_passage_number` must be a positive integer"
  )
})

test_that("filter_operation_batch_measure validates required columns in operation", {
  test_data <- create_test_filter_data()
  
  # Operation missing operation_id
  bad_operation <- test_data$operation %>% dplyr::select(-operation_id)
  expect_error(
    test_filter_operation_batch_measure(operation = bad_operation),
    "`operation` data missing required columns: operation_id"
  )
  
  # Operation missing multiple columns
  bad_operation2 <- test_data$operation %>% dplyr::select(operation_id)
  expect_error(
    test_filter_operation_batch_measure(operation = bad_operation2),
    "`operation` data missing required columns: protocol, date, site_id"
  )
})

test_that("filter_operation_batch_measure validates required columns in point_group", {
  test_data <- create_test_filter_data()
  
  # Point group missing grp_id
  bad_pg <- test_data$point_group %>% dplyr::select(-grp_id)
  expect_error(
    test_filter_operation_batch_measure(point_group = bad_pg),
    "`point_group` data missing required columns: grp_id"
  )
})

test_that("filter_operation_batch_measure validates required columns in ele_sampling", {
  test_data <- create_test_filter_data()
  
  # Elementary sampling missing prelevement_type
  bad_es <- test_data$ele_sampling %>% dplyr::select(-prelevement_type)
  expect_error(
    test_filter_operation_batch_measure(ele_sampling = bad_es),
    "`ele_sampling` data missing required columns: prelevement_type"
  )
})

test_that("filter_operation_batch_measure validates required columns in fish_batch", {
  test_data <- create_test_filter_data()
  
  # Fish batch missing operation_id
  bad_fb <- test_data$fish_batch %>% dplyr::select(-operation_id)
  expect_error(
    test_filter_operation_batch_measure(fish_batch = bad_fb),
    "`fish_batch` data missing required columns: operation_id"
  )
})

test_that("filter_operation_batch_measure validates required columns in ind_measure", {
  test_data <- create_test_filter_data()
  
  # Individual measurement missing prelevement_id
  bad_im <- test_data$ind_measure %>% dplyr::select(-prelevement_id)
  expect_error(
    test_filter_operation_batch_measure(ind_measure = bad_im),
    "`ind_measure` data missing required columns: prelevement_id"
  )
})

test_that("filter_operation_batch_measure generates warnings for data issues", {

  test_data <- create_correct_filter_data()
  test_data$operation[test_data$operation$site_id == 101, ]$site_id <- NA
  # Test warning for missing site_id
  expect_warning(
    test_filter_operation_batch_measure(
      # TODO: Add a missing site id
      operation = test_data$operation,
      op_protocol_to_keep = NULL,
      omit_na_site = TRUE
    ),
    "operations removed due to missing `site_id`"
  )
  
  # Test warning for missing protocols
  expect_warning(
    test_filter_operation_batch_measure(
      op_protocol_to_keep = c("nonexistent_protocol", "complete")
    ),
    "These requested protocols to keep are not found in data"
  )
  
  # Test warning for date filtering removing all data
  expect_warning(
    test_filter_operation_batch_measure(
      oldest_sampling_date = "2050-01-01"
    ),
    "No operations remain after date filtering"
  )
})

test_that("filter_operation_batch_measure warns about missing objectives", {
  expect_warning(
    test_filter_operation_batch_measure(
      op_objective_to_exclude = c("Nonexistent Objective", "Objective A")
    ),
    "These provided objectives to exclude are not found in ASPE"
  )
})


test_that("filter_operation_batch_measure handles empty inputs gracefully", {
  test_data <- create_correct_filter_data()
  
  # Create empty data frames with required columns
  empty_operation <- test_data$operation[0, ]
  empty_point_group <- test_data$point_group[0, ]
  empty_ele_sampling <- test_data$ele_sampling[0, ]
  empty_fish_batch <- test_data$fish_batch[0, ]
  empty_ind_measure <- test_data$ind_measure[0, ]
  
  # Should work without errors (but might produce warnings)
  expect_warning(
    result <- test_filter_operation_batch_measure(
      operation = empty_operation,
      point_group = empty_point_group,
      ele_sampling = empty_ele_sampling,
      fish_batch = empty_fish_batch,
      ind_measure = empty_ind_measure
    )
  )
  
  # Result should be empty data frames
  expect_equal(nrow(result$operation), 0)
  expect_equal(nrow(result$fish_batch), 0)
  expect_equal(nrow(result$ind_measure), 0)
})

test_that("filter_operation_batch_measure handles NULL parameters correctly", {
  test_data <- create_correct_filter_data()
  
  # Test with NULL date (should skip date filtering)
  expect_silent(
    result <- test_filter_operation_batch_measure(
      operation = test_data$operation,
      point_group = test_data$point_group,
      ele_sampling = test_data$ele_sampling,
      fish_batch = test_data$fish_batch,
      ind_measure = test_data$ind_measure,
      oldest_sampling_date = NULL
    )
  )
  
  # Test with NULL objective exclusion (should skip objective filtering)
  expect_silent(
    result <- test_filter_operation_batch_measure(
      operation = test_data$operation,
      point_group = test_data$point_group,
      ele_sampling = test_data$ele_sampling,
      fish_batch = test_data$fish_batch,
      ind_measure = test_data$ind_measure,
      op_objective_to_exclude = NULL
    )
  )
  
  # Test with NULL protocol (should skip protocol filtering)
  expect_silent(
    result <- filter_operation_batch_measure(
      operation = test_data$operation,
      point_group = test_data$point_group,
      ele_sampling = test_data$ele_sampling,
      fish_batch = test_data$fish_batch,
      ind_measure = test_data$ind_measure,
      op_protocol_to_keep = NULL
    )
  )
})
