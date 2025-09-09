#' Clean and standardize operation data from ASPE database
#'
#' Processes fishing operation data by joining objective and protocol information,
#' translating labels, standardizing column names, and enhancing with derived date fields.
#'
#' @param op A data frame containing operation data. By default uses
#'   `get_raw_operation_aspe()` to retrieve raw data. Expected to contain columns:
#'   `ope_id`, `ope_pro_id`, `ope_pop_id`, and date-related columns.
#' @param op_objective A data frame containing operation-objective relationships.
#'   By default uses `get_objective_operation_aspe()`. Expected to contain columns:
#'   `opo_ope_id`, `opo_obj_id`.
#' @param ref_objective A data frame containing objective reference data.
#'   By default uses `get_ref_objective_operation_aspe()`. Expected to contain columns:
#'   `obj_id`, `obj_libelle`.
#' @param ref_protocol A data frame containing protocol reference data.
#'   By default uses `get_ref_protocol_operation_aspe()`. Expected to contain columns:
#'   `pro_id`, `pro_libelle`.
#' @param sampling_point A data frame containing sampling point data.
#'   By default uses `get_sampling_point_aspe()`. Expected to contain columns:
#'   `pop_id`, `pop_sta_id`.
#'
#' @return A data frame with cleaned operation data containing:
#' \itemize{
#'   \item Standardized columns (without `ope_` prefix)
#'   \item Translated protocol labels to English
#'   \item Joined objective information
#'   \item Joined station IDs from sampling points
#'   \item Logical `without_fish` column
#'   \item Derived date fields (`date_time` as POSIXct, `date` as Date)
#' }
#'
#' @details
#' The function performs the following transformations:
#' \itemize{
#'   \item Joins operation objectives from reference tables
#'   \item Translates French protocol labels to English using `replacement_operation_protocol_label()` (internal function)
#'   \item Joins sampling point station information
#'   \item Removes `ope_` prefix from column names
#'   \item Converts `without_fish` from character ("t"/"f") to logical (TRUE/FALSE)
#'   \item Creates proper date/time fields using lubridate
#'   \item Selects and renames columns based on `replacement_operation_col()` (internal function)
#' }
#'
#' @examples
#' \dontrun{
#' # Using default data sources
#' clean_operations <- clean_operation_aspe()
#'
#' # With custom data frames
#' clean_ops <- clean_operation_aspe(
#'   op = my_operations,
#'   op_objective = my_objectives,
#'   ref_objective = my_obj_ref,
#'   ref_protocol = my_protocol_ref,
#'   sampling_point = my_sampling_points
#' )
#' }
#'
#' @importFrom dplyr rename_with left_join select mutate
#' @importFrom stringr str_replace_all
#' @importFrom lubridate ymd_hms date
#' @seealso
#' - [get_raw_operation_aspe()] for raw operation data
#' - [get_objective_operation_aspe()] for operation-objective relationships
#' - [get_ref_objective_operation_aspe()] for objective reference data
#' - [get_ref_protocol_operation_aspe()] for protocol reference data
#' - [get_sampling_point_aspe()] for sampling point data
#' @export
clean_operation_aspe <- function(
  op = get_raw_operation_aspe(),
  op_objective = get_objective_operation_aspe(),
  ref_objective = get_ref_objective_operation_aspe(),
  ref_protocol = get_ref_protocol_operation_aspe(),
  sampling_point = get_sampling_point_aspe()
  ) {

  # Input checks

  ## Operation data checks
  if (!is.data.frame(op)) {
    stop("`op` must be a data frame", call. = FALSE)
  }

  required_op_cols <- c("ope_id", "ope_pro_id", "ope_pop_id", "ope_date")
  missing_op_cols <- setdiff(required_op_cols, names(op))
  if (length(missing_op_cols) > 0) {
    stop(
      "Operation data is missing required columns: ",
      paste(missing_op_cols, collapse = ", "),
      call. = FALSE
    )
  }

  ## Operation-objective data checks
  if (!is.data.frame(op_objective)) {
    stop("`op_objective` must be a data frame", call. = FALSE)
  }

  required_opo_cols <- c("opo_ope_id", "opo_obj_id")
  missing_opo_cols <- setdiff(required_opo_cols, names(op_objective))
  if (length(missing_opo_cols) > 0) {
    stop(
      "Operation-objective data is missing required columns: ",
      paste(missing_opo_cols, collapse = ", "),
      call. = FALSE
    )
  }

  ## Reference objective checks
  if (!is.data.frame(ref_objective)) {
    stop("`ref_objective` must be a data frame", call. = FALSE)
  }

  required_obj_cols <- c("obj_id", "obj_libelle")
  missing_obj_cols <- setdiff(required_obj_cols, names(ref_objective))
  if (length(missing_obj_cols) > 0) {
    stop(
      "Objective reference is missing required columns: ",
      paste(missing_obj_cols, collapse = ", "),
      call. = FALSE
    )
  }

  ## Reference protocol checks
  if (!is.data.frame(ref_protocol)) {
    stop("`ref_protocol` must be a data frame", call. = FALSE)
  }

  required_pro_cols <- c("pro_id", "pro_libelle")
  missing_pro_cols <- setdiff(required_pro_cols, names(ref_protocol))
  if (length(missing_pro_cols) > 0) {
    stop(
      "Protocol reference is missing required columns: ",
      paste(missing_pro_cols, collapse = ", "),
      call. = FALSE
    )
  }

  ## Sampling point checks
  if (!is.data.frame(sampling_point)) {
    stop("`sampling_point` must be a data frame", call. = FALSE)
  }

  required_pop_cols <- c("pop_id", "pop_sta_id")
  missing_pop_cols <- setdiff(required_pop_cols, names(sampling_point))
  if (length(missing_pop_cols) > 0) {
    stop(
      "Sampling point data is missing required columns: ",
      paste(missing_pop_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Attach objective label to operation ids
  op_objective <- op_objective %>%
    dplyr::rename_with(., ~gsub("opo_", "", .x, fixed = TRUE)) %>%
    dplyr::left_join(ref_objective, dplyr::join_by(obj_id)) %>%
    dplyr::select(ope_id, obj_libelle)

  sampling_point <- sampling_point %>%
    dplyr::select(pop_id, pop_sta_id)

  # Translate protocol label into english
  ref_protocol <- ref_protocol %>%
    dplyr::mutate(pro_libelle = recode(pro_libelle,
      !!!get_rev_vec_name_val(replacement_operation_protocol_label())))

  # Get objectives and protocol labels + station id
  op <- op %>%
    dplyr::left_join(op_objective, dplyr::join_by(ope_id)) %>%
    dplyr::left_join(ref_protocol, dplyr::join_by(ope_pro_id == pro_id)) %>%
    dplyr::left_join(sampling_point, dplyr::join_by(ope_pop_id == pop_id)) %>%
    dplyr::rename_with(., ~gsub("ope_", "", .x, fixed = TRUE))

  # Select and remane columns
  op <- op %>%
    dplyr::select(all_of(replacement_operation_col())) %>%
    # without_fish variable into logical variable
    dplyr::mutate(
      without_fish = stringr::str_replace_all(
        without_fish,
        c("t" = "TRUE", "f" = "FALSE")
      ),
      without_fish = as.logical(without_fish)
    )
  # Add date in lubridate format
  op <- op %>%
    dplyr::mutate(
      date_time = lubridate::ymd_hms(date),
      date = lubridate::date(date_time)
    )
  # Add some info from desc table?
  op
}

#' Clean and standardize operation description data from ASPE database
#'
#' Processes fishing operation description data by translating isolation types and
#' prospection methods, joining reference data, and standardizing column names.
#'
#' @param op_description A data frame containing operation description data.
#'   By default uses `get_description_operation_aspe()` to retrieve raw data.
#'   Expected to contain columns: `odp_iso_id_amont`, `odp_iso_id_aval`, `odp_mop_id`.
#' @param ref_isolation A data frame containing isolation type reference data.
#'   By default uses `get_ref_isolation_operation_aspe()`.
#'   Expected to contain columns: `iso_id`, `iso_libelle`.
#' @param ref_prospection A data frame containing prospection method reference data.
#'   By default uses `get_ref_prospection_method_operation_aspe()`.
#'   Expected to contain columns: `mop_id`, `mop_libelle`.
#'
#' @return A data frame with cleaned operation description data containing:
#' \itemize{
#'   \item Standardized columns (without `odp_` prefix)
#'   \item Translated isolation types (upstream and downstream)
#'   \item Translated prospection methods
#'   \item Selected and renamed columns based on `replacement_operation_description_col()`
#' }
#'
#' @details
#' The function performs the following transformations:
#' \itemize{
#'   \item Translates French isolation labels to English using `replacement_isolation_label()`
#'   \item Translates French prospection method labels to English using `replacement_prospection_label()`
#'   \item Joins isolation and prospection reference data to operation descriptions
#'   \item Removes `odp_` prefix from column names
#'   \item Selects and renames columns based on the mapping from `replacement_operation_description_col()`
#' }
#'
#' @examples
#' \dontrun{
#' # Using default data sources
#' clean_descriptions <- clean_description_operation_aspe()
#'
#' # With custom data frames
#' custom_descriptions <- clean_description_operation_aspe(
#'   op_description = my_descriptions,
#'   ref_isolation = my_isolation_ref,
#'   ref_prospection = my_prospection_ref
#' )
#' }
#'
#' @importFrom dplyr mutate recode left_join select rename_with
#' @seealso
#' - [get_description_operation_aspe()] for raw data access
#' - [get_ref_isolation_operation_aspe()] for isolation reference data
#' - [get_ref_prospection_method_operation_aspe()] for prospection reference data
#' - [replacement_isolation_label()] for isolation type translations
#' - [replacement_prospection_label()] for prospection method translations
#' @export
clean_description_operation_aspe <- function(
  op_description = get_description_operation_aspe(),
  ref_isolation = get_ref_isolation_operation_aspe(),
  ref_prospection = get_ref_prospection_method_operation_aspe()
  ) {

  ## Operation description checks
  if (!is.data.frame(op_description)) {
    stop("`op_description` must be a data frame", call. = FALSE)
  }

  required_op_cols <- c("odp_iso_id_amont", "odp_iso_id_aval", "odp_mop_id")
  missing_op_cols <- setdiff(required_op_cols, names(op_description))
  if (length(missing_op_cols) > 0) {
    stop(
      "Operation description is missing required columns: ", 
      paste(missing_op_cols, collapse = ", "),
      call. = FALSE
    )
  }

  ## Reference isolation checks
  if (!is.data.frame(ref_isolation)) {
    stop("`ref_isolation` must be a data frame", call. = FALSE)
  }
  required_iso_cols <- c("iso_id", "iso_libelle")
  missing_iso_cols <- setdiff(required_iso_cols, names(ref_isolation))
  if (length(missing_iso_cols) > 0) {
    stop(
      "Isolation reference is missing required columns: ",
      paste(missing_iso_cols, collapse = ", "),
      call. = FALSE
    )
  }

  ## Reference prospection checks
  if (!is.data.frame(ref_prospection)) {
    stop("`ref_prospection` must be a data frame", call. = FALSE)
  }
  required_pros_cols <- c("mop_id", "mop_libelle")
  missing_pros_cols <- setdiff(required_pros_cols, names(ref_prospection))
  if (length(missing_pros_cols) > 0) {
    stop(
      "Prospection reference is missing required columns: ",
      paste(missing_pros_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Translate labels of isolation and prospection method
  ref_isolation <- ref_isolation %>%
    dplyr::mutate(
      iso_libelle = dplyr::recode(
        iso_libelle,
        !!!get_rev_vec_name_val(replacement_isolation_label())
      )
    )
  ref_prospection <- ref_prospection %>%
    dplyr::mutate(
      mop_libelle = dplyr::recode(
        mop_libelle,
        !!!get_rev_vec_name_val(replacement_prospection_label())
      )
    )

  # Add isolation and prospection label to operation description
  op_description <- op_description %>%
    dplyr::left_join(
      dplyr::select(ref_isolation, iso_id, upstream_isolation = iso_libelle),
      dplyr::join_by(odp_iso_id_amont == iso_id)
    ) %>%
    dplyr::left_join(
      dplyr::select(ref_isolation, iso_id, downstream_isolation = iso_libelle),
      dplyr::join_by(odp_iso_id_aval == iso_id)
    ) %>%
    dplyr::left_join(
      dplyr::select(ref_prospection, mop_id, prospection_method = mop_libelle),
      dplyr::join_by(odp_mop_id == mop_id)
    )

  # Sanatize, replace and translate column names
  op_description %>%
    rename_with(., ~gsub("odp_", "", .x, fixed = TRUE)) %>%
    select(all_of(replacement_operation_description_col()))
}
