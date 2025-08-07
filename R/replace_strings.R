


#' Replacement vector to replace station.csv column names
#'
#'
#' @return named vector
#' @export
replacement_station_col <- function() {
  c(
    site_id = "id",
    "sandre_code" = "code_sandre",
    "insee_town_code" = "com_code_insee",
     "down_km_point" = "point_km_aval",
    "precise_location" = "localisation_precise",
    "body_water_code" = "code_national_masse_eau",
    "postgis" = "geometrie",
    "x" = "coordonnees_x",
    "y" = "coordonnees_y"
  )
}
