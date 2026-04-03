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

# Test size data

create_test_length_data <- function() {
  # Species reference
  species_ref <- data.frame(
    species_code = c("PER", "TRF", "ALA", "LOF", "UNKNOWN"),
    latin_name = c("Perca fluviatilis", "Salmo trutta", "Alosa alosa", 
                   "Barbatula barbatula", "Unknownus species"),
    maximal_length_mm = c(600, 1000, 600, 120, 500)
  )
  
  # Fish batch data
  fish_batch <- data.frame(
    batch_id = c(1, 2, 3, 4, 5, 6),
    species_code = c("PER", "TRF", "ALA", "PER", "LOF", "UNKNOWN"),
    length_type = c("fork", "fork", "fork", "total", "fork", "fork"),
    batch_type = c("S/L", "S/L", "S/L", "S/L", "S/L", "S/L"),
    min_length = c(100, 200, 150, 120, 50, 80),
    max_length = c(150, 300, 200, 180, 70, 100)
  )
  
  # Individual measurements
  ind_measure <- data.frame(
    measure_id = 1:15,
    batch_id = c(1, 1, 1, 2, 2, 3, 3, 4, 4, 4, 5, 5, 6, 6, 6),
    species_code = c("PER", "PER", "PER", "TRF", "TRF", "ALA", "ALA",
                     "PER", "PER", "PER", "LOF", "LOF", "UNKNOWN", "UNKNOWN", "UNKNOWN"),
    size = c(110, 125, 140, 220, 280, 160, 190, 125, 130, 175, 55, 65, 85, 95, 110)
  )
  
  list(
    species_ref = species_ref,
    fish_batch = fish_batch,
    ind_measure = ind_measure
  )
}

# Mock conversion vector
test_conversion_vector <- function() {
  c(
    PER = 1.05,   # TL = 0 + 1.05 × FL
    TRF = 1.04,
    ALA = 1.09,
    LOF = 1.0
  )
}

# Mock FishBase data
mock_fishbase_length_length <- function() {
  data.frame(
    Species = c("Perca fluviatilis", "Salmo trutta", "Alosa alosa"),
    Length1 = c("TL", "TL", "TL"),
    Length2 = c("FL", "FL", "FL"),
    a = c(0, 0, 0),
    b = c(1.01, 1.02, 1.03),
    stringsAsFactors = FALSE
  )
}

