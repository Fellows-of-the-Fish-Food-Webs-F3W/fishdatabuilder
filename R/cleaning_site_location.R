#' Clean and standardize station data from ASPE database
#'
#' This function processes station data by:
#' \itemize{
#'   \item Removing "sta_" prefixes from column names (if present)
#'   \item Joining with projection reference data
#'   \item Standardizing column names
#'   \item Converting all coordinates to a specified CRS
#'   \item Extracting X/Y coordinates from geometry
#' }
#'
#' @param station A data frame containing station data. By default uses 
#'        `get_raw_station_aspe()` to retrieve standard station data.
#'        Expected columns:
#'        \itemize{
#'          \item sta_site_id or site_id (after renaming)
#'          \item sta_typ_id or typ_id (after renaming)
#'          \item sta_x or x (after renaming) - X coordinates
#'          \item sta_y or y (after renaming) - Y coordinates
#'        }
#' @param ref_coordinates A data frame containing coordinate reference system data.
#'        By default uses `get_raw_ref_coordinates_station_aspe()`.
#'        Expected columns:
#'        \itemize{
#'          \item typ_id - Matching station type IDs
#'          \item typ_code_epsg - EPSG codes for each type
#'        }
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
#' @examples
#' \dontrun{
#' # Using default data frames and WGS84 output
#' clean_data <- clean_station_aspe()
#' 
#' # With custom data frames and CRS
#' custom_stations <- data.frame(
#'   sta_site_id = c("S1", "S2"),
#'   sta_typ_id = c(1, 2),
#'   sta_x = c(100, 200),
#'   sta_y = c(300, 400)
#' )
#' 
#' custom_ref <- data.frame(
#'   typ_id = 1:2,
#'   typ_code_epsg = c(2154, 4326)
#' )
#' 
#' clean_data <- clean_station_aspe(
#'   station = custom_stations,
#'   ref_coordinates = custom_ref,
#'   crs_to = 2154  # French RGF93 Lambert-93
#' )
#' }
#'
#' @importFrom dplyr rename_with left_join group_by mutate ungroup select rename
#' @importFrom tidyr nest unnest
#' @importFrom purrr map2
#' @importFrom sf st_as_sf st_transform st_coordinates
#' @export
clean_station_aspe <- function(
  station = get_raw_station_aspe(),
  ref_coordinates = get_raw_ref_coordinates_station_aspe(),
  crs_to = 4326
  ) {

  # Check inputs
  ## Station data checks
  if (!is.data.frame(station)) {
    stop("`station` must be a data frame", call. = FALSE)
  }

  ## Reference data checks
  if (!is.data.frame(ref_coordinates)) {
    stop("`ref_coordinates` must be a data frame", call. = FALSE)
  }
  
  required_ref_cols <- c("typ_id", "typ_code_epsg")
  missing_ref_cols <- setdiff(required_ref_cols, names(ref_coordinates))
  
  if (length(missing_ref_cols) > 0) {
    stop(
      "Reference data is missing required columns: ",
      paste(missing_ref_cols, collapse = ", "), 
      call. = FALSE
    )
  }
  
  ## CRS validation
  if (!is.numeric(crs_to) || length(crs_to) != 1) {
    stop("`crs_to` must be a single numeric EPSG code", call. = FALSE)
  }
  valid_crs <- !is.na(suppressWarnings(sf::st_crs(crs_to)$input))
  if (!valid_crs) stop("Invalid CRS: ", crs_to)

  # remove variable prefix sta_
  station <- dplyr::rename_with(station, ~gsub("sta_", "", .x, fixed = TRUE))

  # Replace column names
  station <- rename(station,
    any_of(replacement_station_col()))


  required_st_cols <- c("typ_id", "x", "y")
  missing_st_cols <- setdiff(required_st_cols, names(station))
  if (length(missing_st_cols) > 0) {
    stop(
      "Station data is missing required columns: ",
      paste(missing_st_cols, collapse = ", "), 
      call. = FALSE
    )
  }

  # Get reference of the coordinate systems (epsg)
  station <- station %>%
    dplyr::left_join(ref_coordinates, by = "typ_id")

  ## Check for coordinates
  if (any(!sapply(station[c("x", "y")], is.numeric))) {
    stop("Coordinate columns (x, y) or (sta_x, sta_y)  must be numeric",
      call. = FALSE)
  }
  if (any(is.na(station[c("x", "y")]))) {
    stop("NA values detected in coordinate columns", call. = FALSE)
  }

  ## Check for matching typ_ids
  missing_ids <- setdiff(station$typ_id, ref_coordinates$typ_id)
  if (length(missing_ids) > 0) {
    stop(
      "The following typ_id values in station data are missing from reference: ",
      paste(unique(missing_ids), collapse = ", "),
      call. = FALSE
    )
  }

  if (nrow(station) == 0) {
    return(station)
  }

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

#' Get raw data from ASPE database
#'
#' Functions to retrieve unprocessed data from the package cache before any cleaning
#' or transformation is applied.
#'
#' @param file Character. Name of the data file in package cache. Each function
#'   has a sensible default for the specific data type it accesses.
#'
#' @return A data frame containing raw data with original column names and structure
#'   as provided in the ASPE database.
#'
#' @details
#' These functions provide access to various ASPE database tables:
#' \describe{
#'   \item{`get_raw_station_aspe`}{Station location data}
#'   \item{`get_raw_ref_coordinates_station_aspe`}{Coordinate reference system (CRS) data}
#'   \item{`get_raw_operation_aspe`}{Fishing operation metadata}
#'   \item{`get_ref_objective_operation_aspe`}{Operation objective reference table}
#'   \item{`get_ref_protocol_operation_aspe`}{Sampling protocol reference table}
#'   \item{`get_ref_isolation_operation_aspe`}{Barrier isolation type reference table}
#'   \item{`get_ref_prospection_method_operation_aspe`}{Prospection method reference table}
#'   \item{`get_objective_operation_aspe`}{Operation-objective relationship table}
#'   \item{`get_sampling_point_aspe`}{Sampling point location data}
#'   \item{`get_description_operation_aspe`}{Detailed fishing operation descriptions}
#' }
#'
#' @examples
#' \dontrun{
#' # Get station data
#' stations <- get_raw_station_aspe()
#' 
#' # Get operation data
#' operations <- get_raw_operation_aspe()
#' 
#' # Get reference tables
#' objectives <- get_ref_objective_operation_aspe()
#' protocols <- get_ref_protocol_operation_aspe()
#' 
#' # Use custom files
#' custom_stations <- get_raw_station_aspe(file = "custom_stations.csv")
#' }
#'
#' @seealso
#' - [clean_station_aspe()] for cleaned versions of station data
#' - [list_optional_files()] to see available files in cache
#' @name raw_data_accessors
#' @family raw data accessors
#' @importFrom utils read.csv2
NULL

# Individual function implementations with specific details

#' @rdname raw_data_accessors
#' @details For `get_raw_station_aspe()`: Retrieves station location data including coordinates.
#' Default file: `"station.csv"`
#' @export get_raw_station_aspe
get_raw_station_aspe <- function(file = "station.csv") {
  read_raw_data(file_name = file)
}

#' @rdname raw_data_accessors
#' @details For `get_raw_ref_coordinates_station_aspe()`: Retrieves EPSG code references for coordinate systems.
#' Default file: `"ref_type_projection.csv"`
#' @export get_raw_ref_coordinates_station_aspe
get_raw_ref_coordinates_station_aspe <- function(file = "ref_type_projection.csv") {
  read_raw_data(file_name = file)
}

#' @rdname raw_data_accessors
#' @details For `get_raw_operation_aspe()`: Retrieves fishing operation metadata.
#' Default file: `"operation.csv"`
get_raw_operation_aspe <- function(file = "operation.csv") {
  read_raw_data(file_name = file)
}

#' @rdname raw_data_accessors
#' @details For `get_ref_objective_operation_aspe()`: Retrieves operation objective reference data.
#' Default file: `"ref_objectif.csv"`
#' @export get_objective_operation_aspe
get_ref_objective_operation_aspe <- function(file = "ref_objectif.csv") {
  read_raw_data(file_name = file)
}

#' @rdname raw_data_accessors
#' @details For `get_ref_protocol_operation_aspe()`: Retrieves sampling protocol reference data.
#' Default file: `"ref_protocole.csv"`
#' @export get_ref_protocol_operation_aspe
get_ref_protocol_operation_aspe <- function(file = "ref_protocole.csv") {
  read_raw_data(file_name = file)
}

#' @rdname raw_data_accessors
#' @details For `get_ref_isolation_operation_aspe()`: Retrieves barrier isolation type reference data.
#' Default file: `"ref_isolement.csv"`
#' @export get_ref_isolation_operation_aspe
get_ref_isolation_operation_aspe <- function(file = "ref_isolement.csv") {
  read_raw_data(file_name = file)
}

#' @rdname raw_data_accessors
#' @details For `get_ref_prospection_method_operation_aspe()`: Retrieves prospection method reference data.
#' Default file: `"ref_moyen_prospection.csv"`
#' @export get_ref_prospection_method_operation_aspe
get_ref_prospection_method_operation_aspe <- function(file = "ref_moyen_prospection.csv") {
  read_raw_data(file_name = file)
}

#' @rdname raw_data_accessors
#' @details For `get_objective_operation_aspe()`: Retrieves operation-objective relationship data.
#' Default file: `"operation_objectif.csv"`
get_objective_operation_aspe <- function(file = "operation_objectif.csv") {
  read_raw_data(file_name = file)
}

#' @rdname raw_data_accessors
#' @details For `get_sampling_point_aspe()`: Retrieves sampling point location data.
#' Default file: `"point_prelevement.csv"`
#' @export get_sampling_point_aspe
get_sampling_point_aspe <- function(file = "point_prelevement.csv") {
  read_raw_data(file_name = file)
}

#' @rdname raw_data_accessors
#' @details For `get_description_operation_aspe()`: Retrieves detailed fishing operation descriptions.
#' Default file: `"operation_description_peche.csv"`
#' @export get_description_operation_aspe
get_description_operation_aspe <- function(file = "operation_description_peche.csv") {
  read_raw_data(file_name = file)
}

#' Read raw data from package cache (internal)
#'
#' Internal helper function to read CSV data from the package cache with validation.
#' Not intended for direct use by package users.
#'
#' @param file_name Character. Name of the file to read (must exist in package cache).
#' 
#' @return A data frame containing the read data
#' 
#' @keywords internal
#' @noRd
read_raw_data <- function(file_name = NULL) {
  if (!is.character(file_name) || length(file_name) != 1) {
    stop("`file_name` must be a single character value")
  }
  if (!file_name %in% fishdatabuilder::list_optional_files()) {
  stop(
    "`file_name` must be in the package cache.
    Please check file name with `list_optional_files()` or
    `pkgfilecache::list_available(pkg_info = pkgfilecache::get_pkg_info(packagename = 'fishdatabuilder'))`
    ")
  }
  read.csv2(
    fishdatabuilder::get_optional_filepath(filename = file_name),
    row.names = 1
  )
}
