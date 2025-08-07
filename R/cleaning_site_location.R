#' Clean and standardize station data from ASPE database
#'
#' This function processes station data by:
#' \itemize{
#'   \item Removing "sta_" prefixes from column names
#'   \item Joining with projection reference data
#'   \item Standardizing column names
#'   \item Converting all coordinates to a specified CRS
#'   \item Extracting X/Y coordinates from sf geometry
#' }
#'
#' @param file Character. Path to the station data file (CSV format). 
#'        Default is "station.csv".
#' @param ref_file Character. Path to the projection reference file (CSV format).
#'        Default is "ref_type_projection.csv".
#' @param crs_to Numeric. The target coordinate reference system (EPSG code) 
#'        for output coordinates. Default is 4326 (WGS84).
#'
#' @return A data frame with standardized station data containing:
#' \itemize{
#'   \item site_id - Station identifier
#'   \item x - Longitude or easting in target CRS
#'   \item y - Latitude or northing in target CRS
#' }
#'
#' @details The function expects the input files to be in CSV format with:
#' \itemize{
#'   \item First column as row names for both input files
#'   \item Station data should contain columns with "sta_" prefix
#'   \item Station data should contain a "typ_id" column for joining with reference data
#'   \item Station data should contain "x" "y" columns as coordinates
#'   \item Reference file should contain "typ_code_epsg" column with source CRS information
#' }
#'
#' @examples
#' \dontrun{
#' # Using default file names and WGS84 output
#' clean_data <- clean_station_aspe()
#' 
#' # With custom files and CRS
#' clean_data <- clean_station_aspe(
#'   file = "my_stations.csv",
#'   ref_file = "my_projections.csv",
#'   crs_to = 2154  # French RGF93 Lambert-93
#' )
#' }
#'
#' @importFrom dplyr rename_with left_join group_by mutate ungroup select rename
#' @importFrom tidyr nest unnest
#' @importFrom purrr map2
#' @importFrom sf st_as_sf st_transform st_coordinates
#' @importFrom utils read.csv2
#' @export
clean_station_aspe <- function(
  #TODO: Switch for data.frame like argument, see helper functions below
  file = "station.csv",
  ref_file = "ref_type_projection.csv",
  crs_to = 4326
  ) {

  station <- read.csv2(
    fishdatabuilder::get_optional_data_filepath(filename = file),
    row.names = 1)

  # remove variable prefix sta_
  station <- dplyr::rename_with(station, ~gsub("sta_", "", .x, fixed = TRUE))
  # Get reference of the coordinate systems (epsg)
  ref_coordinates <- read.csv2(
    fishdatabuilder::get_optional_data_filepath(filename = ref_file),
    row.names = 1)
  station <- station %>%
    dplyr::left_join(ref_coordinates, by = "typ_id")

  # Replace column names
  station <- rename(station,
    all_of(fishdatabuilder:::replacement_station_col()))

  # Convert all coodinates to the `crs_to` epsg projection
  convert_crs <- function (df = NULL, epsg = NULL, crs_to = NULL) {
    temp <- sf::st_as_sf(df, crs = epsg, coords = c("x", "y"))
    sf::st_transform(temp, crs = crs_to)
  }
  station <- station %>%
    dplyr::group_by(typ_code_epsg) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      data = purrr::map2(data, typ_code_epsg, convert_crs, crs_to = crs_to)
    ) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(cols = data) %>%
    dplyr::select(site_id, geometry)

  # Convert sf geometry column in X,Y column
  station <- station %>%
    cbind(., sf::st_coordinates(st_as_sf(.))) %>%
    dplyr::select(-geometry) %>%
    rename_with(., tolower)

  return(station)
}

read_raw_data <- function(file_name = NULL) {
  if (!file_name %in% list_available_files()) {
  stop(
    "`file_name` must be in the package cache.
    Please check file name with `list_available_files()` or
    `pkgfilecache::list_available(pkg_info = pkgfilecache::get_pkg_info(packagename = 'fishdatabuilder'))`
    ")
  }
  read.csv2(
    fishdatabuilder::get_optional_data_filepath(filename = file_name),
    row.names = 1
  )
}
get_raw_station_aspe <- function(file = "station.csv") {
  read_raw_data(file_name = file)
}
get_raw_ref_coordinates_station_aspe <- function(file = "ref_type_projection.csv") {
  read_raw_data(file_name = file)
}

