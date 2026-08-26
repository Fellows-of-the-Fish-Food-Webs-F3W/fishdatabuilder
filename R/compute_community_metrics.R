#' Compute fish community metrics
#'
#' Computes community-level richness, abundance, and biomass for each sampling
#' operation, together with corresponding metrics standardized by sampled
#' surface area. Species-level abundance and biomass are returned as nested
#' tibbles.
#'
#' @param fish_data A data frame containing individual fish records. Must contain
#'   the columns `operation_id`, `species_code`, and `weight_g`. Each row is
#'   expected to represent one individual.
#' @param operation A data frame containing operation-level information. Must
#'   contain the columns `operation_id` and `computed_surface`, with
#'   `computed_surface` expressed in square metres.
#'
#' @return A tibble with one row per sampling operation and nine columns:
#'   \itemize{
#'     \item `operation_id`: sampling operation identifier.
#'     \item `total_richness`: number of unique species.
#'     \item `total_abundance`: total number of individuals.
#'     \item `total_biomass_g`: total biomass in grams. Missing individual
#'       weights are excluded from the sum.
#'     \item `richness_per_m2`: species richness divided by sampled surface area.
#'       Returns `NA` when sampled surface is missing, non-finite, or <= 0.
#'     \item `abundance_per_m2`: abundance divided by sampled surface area.
#'       Returns `NA` when sampled surface is missing, non-finite, or <= 0.
#'     \item `biomass_g_per_m2`: biomass divided by sampled surface area.
#'       Returns `NA` when sampled surface is missing, non-finite, or <= 0.
#'     \item `abundance_by_species`: nested tibble containing `species_code`,
#'       `total_abundance`, and `abundance_per_m2`.
#'     \item `biomass_by_species`: nested tibble containing `species_code`,
#'       `total_biomass_g`, and `biomass_g_per_m2`.
#'   }
#'
#' @details
#' Only operations represented in `fish_data` are returned.
#'
#' Missing values in `weight_g` are excluded from biomass calculations and
#' generate a warning. Zero values in `weight_g` are retained in biomass
#' calculations and also generate a warning.
#'
#' Total richness, abundance, and biomass are calculated independently of
#' sampled surface area. When `computed_surface` is missing, non-finite, or
#' less than or equal to zero, surface-standardized metrics are returned as
#' `NA`.
#'
#' @examples
#' fish_data <- tibble::tibble(
#'   operation_id = c(1, 1, 1, 2),
#'   species_code = c("AAA", "AAA", "BBB", "AAA"),
#'   weight_g = c(10, 15, NA, 0)
#' )
#'
#' operation <- tibble::tibble(
#'   operation_id = c(1, 2),
#'   computed_surface = c(100, 0)
#' )
#'
#' compute_community_metrics(
#'   fish_data = fish_data,
#'   operation = operation
#' )
#'
#' @importFrom dplyr arrange filter group_by left_join mutate n summarise select
#' @importFrom tidyr nest
#' @export
compute_community_metrics <- function(
    fish_data,
    operation
) {

  # Input validation ----------------------------------------------------------

  if (!is.data.frame(fish_data)) {
    stop("fish_data must be a data frame.", call. = FALSE)
  }

  if (!is.data.frame(operation)) {
    stop("operation must be a data frame.", call. = FALSE)
  }

  required_fish_cols <- c(
    "operation_id",
    "species_code",
    "weight_g"
  )

  missing_fish_cols <- setdiff(
    required_fish_cols,
    names(fish_data)
  )

  if (length(missing_fish_cols) > 0) {
    stop(
      "fish_data missing required columns: ",
      paste(missing_fish_cols, collapse = ", "),
      call. = FALSE
    )
  }

  required_operation_cols <- c(
    "operation_id",
    "computed_surface"
  )

  missing_operation_cols <- setdiff(
    required_operation_cols,
    names(operation)
  )

  if (length(missing_operation_cols) > 0) {
    stop(
      "operation missing required columns: ",
      paste(missing_operation_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (anyNA(fish_data$operation_id)) {
    stop(
      "fish_data$operation_id contains missing values.",
      call. = FALSE
    )
  }

  if (anyNA(fish_data$species_code)) {
    stop(
      "fish_data$species_code contains missing values.",
      call. = FALSE
    )
  }

  # Check fish weights --------------------------------------------------------

  n_missing_weight <- sum(is.na(fish_data$weight_g))
  n_zero_weight <- sum(fish_data$weight_g == 0, na.rm = TRUE)

  if (n_missing_weight > 0 || n_zero_weight > 0) {

    warning_parts <- character(0)

    if (n_missing_weight > 0) {
      warning_parts <- c(
        warning_parts,
        paste0(
          n_missing_weight,
          " missing value(s) in fish_data$weight_g will be excluded ",
          "from biomass calculations"
        )
      )
    }

    if (n_zero_weight > 0) {
      warning_parts <- c(
        warning_parts,
        paste0(
          n_zero_weight,
          " zero value(s) found in fish_data$weight_g"
        )
      )
    }

    warning(
      paste(warning_parts, collapse = "; "),
      ".",
      call. = FALSE
    )
  }

  # Prepare sampled surface information --------------------------------------

  operation_surface <- operation |>
    dplyr::select(
      operation_id,
      computed_surface
    ) |>
    dplyr::filter(
      operation_id %in% unique(fish_data$operation_id)
    )

  duplicated_operations <- operation_surface$operation_id[
    duplicated(operation_surface$operation_id)
  ]

  if (length(duplicated_operations) > 0) {
    stop(
      "operation contains duplicated operation_id values: ",
      paste(unique(duplicated_operations), collapse = ", "),
      call. = FALSE
    )
  }

  missing_operations <- setdiff(
    unique(fish_data$operation_id),
    operation_surface$operation_id
  )

  if (length(missing_operations) > 0) {
    stop(
      "No computed_surface found for operation_id: ",
      paste(missing_operations, collapse = ", "),
      call. = FALSE
    )
  }

  # Compute species-level metrics --------------------------------------------

  species_metrics <- fish_data |>
    dplyr::group_by(
      operation_id,
      species_code
    ) |>
    dplyr::summarise(
      total_abundance = dplyr::n(),
      total_biomass_g = sum(weight_g, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      operation_surface,
      by = "operation_id"
    ) |>
    dplyr::mutate(
      abundance_per_m2 = dplyr::if_else(
        !is.na(computed_surface) &
          is.finite(computed_surface) &
          computed_surface > 0,
        total_abundance / computed_surface,
        NA_real_
      ),
      biomass_g_per_m2 = dplyr::if_else(
        !is.na(computed_surface) &
          is.finite(computed_surface) &
          computed_surface > 0,
        total_biomass_g / computed_surface,
        NA_real_
      )
    ) |>
    dplyr::arrange(
      operation_id,
      species_code
    )

  # Create nested species-level abundance metrics ----------------------------

  abundance_by_species <- species_metrics |>
    dplyr::select(
      operation_id,
      species_code,
      total_abundance,
      abundance_per_m2
    ) |>
    tidyr::nest(
      abundance_by_species = c(
        species_code,
        total_abundance,
        abundance_per_m2
      ),
      .by = operation_id
    )

  # Create nested species-level biomass metrics -------------------------------

  biomass_by_species <- species_metrics |>
    dplyr::select(
      operation_id,
      species_code,
      total_biomass_g,
      biomass_g_per_m2
    ) |>
    tidyr::nest(
      biomass_by_species = c(
        species_code,
        total_biomass_g,
        biomass_g_per_m2
      ),
      .by = operation_id
    )

  # Compute community-level metrics ------------------------------------------

  community_metrics <- species_metrics |>
    dplyr::group_by(operation_id) |>
    dplyr::summarise(
      total_richness = dplyr::n(),
      total_abundance = sum(total_abundance),
      total_biomass_g = sum(total_biomass_g),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      operation_surface,
      by = "operation_id"
    ) |>
    dplyr::mutate(
      richness_per_m2 = dplyr::if_else(
        !is.na(computed_surface) &
          is.finite(computed_surface) &
          computed_surface > 0,
        total_richness / computed_surface,
        NA_real_
      ),
      abundance_per_m2 = dplyr::if_else(
        !is.na(computed_surface) &
          is.finite(computed_surface) &
          computed_surface > 0,
        total_abundance / computed_surface,
        NA_real_
      ),
      biomass_g_per_m2 = dplyr::if_else(
        !is.na(computed_surface) &
          is.finite(computed_surface) &
          computed_surface > 0,
        total_biomass_g / computed_surface,
        NA_real_
      )
    ) |>
    dplyr::left_join(
      abundance_by_species,
      by = "operation_id"
    ) |>
    dplyr::left_join(
      biomass_by_species,
      by = "operation_id"
    ) |>
    dplyr::select(
      operation_id,
      total_richness,
      total_abundance,
      total_biomass_g,
      richness_per_m2,
      abundance_per_m2,
      biomass_g_per_m2,
      abundance_by_species,
      biomass_by_species
    ) |>
    dplyr::arrange(operation_id)

  community_metrics
}
