


#' Replacement vector to replace station.csv column names
#'
#'
#' @return named vector
#' @export
replacement_station_col <- function() {
  c(
    "site_id" = "id",
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
replacement_operation_col <- function() {
  c(
    operation_id = "id",
    site_id = "pop_sta_id",
    date = "date",
    objective = "obj_libelle",
    protocol = "pro_libelle",
    without_fish = "sans_poisson",
    computed_surface = "surface_calculee"
  )
}
replacement_operation_description_col <- function() {
  c(
    operation_id = "ope_id",
    time_fishing = "duree_peche",
    upstream_isolation = "upstream_isolation",
    downstream_isolation = "downstream_isolation",
    width_river = "largeur_lame_eau",
    width_left_shore = "largeur_rive_gauche",
    length_left_shore = "longueur_rive_gauche",
    width_right_shore = "largeur_rive_droite",
    length_right_shore = "longueur_rive_droite",
    length_sourced = "longueur",
    instant_temperature = "temperature_instantanee"
  )

}

replacement_prospection_label <- function() {
  c(
    by_foot = "A pied",
    by_boat = "En bateau",
    mixed = "Mixte",
    unknown = "Non renseigné"
  )
}
replacement_isolation_label <- function() {
  c(
    no_barrier = "Pas d’isolement",
    partial_barrier = "Seuil partiellement franchissable",
    total_barrier = "Obstacle infranchissable",
    net = "Filet",
    electric_dam = "Barrage électrique",
    others = "Autres"
  )
}
replacement_operation_protocol_label <- function(variables) {
    c(
    complete = "Pêche complète à un ou plusieurs passages",
    partial_by_point = "Pêche partielle par points (grand milieu)",
    partial_over_bank = "Pêche partielle sur berge"
  )
}


#' Reverse a Named Vector (Internal)
#'
#' An internal helper function that swaps the names and values of a named vector.
#' This is used internally for value-name lookups and is not exported for public use.
#'
#' @param x A named vector. If `NULL` (default), returns `NULL`.
#'
#' @return A new vector where:
#'   - The original values become the names
#'   - The original names become the values
#' 
#' @examples
#' \dontrun{
#' # Internal package usage only
#' vec <- c(a = "apple", b = "banana")
#' get_rev_vec_name_val(vec)
#' # Returns: 
#' #   apple   banana 
#' #    "a"      "b" 
#' }
#'
#' @keywords internal
#' @noRd
get_rev_vec_name_val <- function(x = NULL) {
  if (is.null(x)) return(NULL)
  if (!is.vector(x) || is.null(names(x))) {
    stop("Input must be a named vector", call. = FALSE)
  }
  y <- names(x)
  names(y) <- x
  return(y)
}
