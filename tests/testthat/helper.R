# Create test data frames
create_test_station <- function() {
  data.frame(
    sta_site_id = c("Paris1", "Paris2"),
    sta_typ_id = c(1, 2),
    sta_x = c(652709, 2.3522),
    sta_y = c(6861420, 48.8566),
    sta_other = c("A", "B")
  )
}

create_test_ref <- function() {
  data.frame(
    typ_id = 1:2,
    typ_code_epsg = c(2154, 4326),
    typ_name = c("Lambert-93", "WGS84")
  )
}


create_test_filter_data <- function() {
  list(
    operation = data.frame(
      operation_id = 1:5,
      protocol = c("complete", "partial_by_point", "partial_over_bank", "other", "complete"),
      date = as.Date(c("2000-01-01", "2001-02-01", "2002-03-01", "2003-04-01", "2004-05-01")),
      site_id = c(101, 102, NA, 104, 105),
      objective = I(list(c("A", "B"), "C", c("D", "E"), "F", "G"))
    ),
    point_group = data.frame(
      grp_id = 2:4,
      point_type = c("standard_point", "other", "standard_point"),
      grp_nombre_points_berge = c(0, 90, 100),
      grp_nombre = c(100, 100, 100)
    ),
    ele_sampling = data.frame(
      prelevement_id = 1:5,
      operation_id = 1:5,
      prelevement_type = c("passage", "point", "passage", "point", "passage"),
      passage_number = c(1, NA, 2, NA, 1)
    ),
    fish_batch = data.frame(
      prelevement_id = 1:4,
      operation_id = c(1, 2, 1, 2),
      species = c("trout", "salmon", "eel", "trout")
    ),
    ind_measure = data.frame(
      prelevement_id = 1:4,
      operation_id = 1:4,
      length = c(10, 15, 12, 18)
    )
  )
}

create_correct_filter_data <- function() {
  list(
    operation = data.frame(
      operation_id = 1:4,
      protocol = c("complete", "partial_by_point", "partial_over_bank", "other"),
      date = as.Date(c("2000-01-01", "2001-02-01", "2002-03-01", "2004-05-01")),
      site_id = c(101, 102, 104, 105),
      objective = I(list(c("A", "B"), "C", c("D", "E"), "F"))
    ),
    point_group = data.frame(
      grp_id = 2:4,
      point_type = c("standard_point", "other", "standard_point"),
      grp_nombre_points_berge = c(10, 5, 8),
      grp_nombre = c(10, 10, 10)
    ),
    ele_sampling = data.frame(
      prelevement_id = 1:4,
      operation_id = 1:4,
      prelevement_type = c("passage", "point", "passage", "point"),
      passage_number = c(1, NA, 2, NA)
    ),
    fish_batch = data.frame(
      prelevement_id = 1:4,
      operation_id = c(1, 2, 1, 2),
      species = c("trout", "salmon", "eel", "trout")
    ),
    ind_measure = data.frame(
      prelevement_id = 1:4,
      operation_id = 1:4,
      length = c(10, 15, 12, 18)
    )
  )
}
