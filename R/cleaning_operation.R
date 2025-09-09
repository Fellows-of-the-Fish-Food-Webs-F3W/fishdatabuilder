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
