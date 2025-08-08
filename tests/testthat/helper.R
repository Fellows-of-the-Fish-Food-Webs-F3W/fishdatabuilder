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
