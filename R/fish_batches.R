#' Sanitize and validate fish batch data from ASPE database
#'
#' Cleans fish batch data from ASPE standard format by filtering invalid records,
#' checking required columns, and validating batch types and measurements.
#' Ensures data quality before individual fish size generation.
#'
#' @param fish_batch A data frame containing fish batch data (from `clean_fish_batch()`).
#'   Expected columns: `batch_id`, `batch_type`, `number`, `species_code`, 
#'   `min_length`, `max_length`
#' @param ind_measure A data frame containing individual fish measurements 
#'   (from `clean_individual_measurement_aspe()`).
#'   Expected columns: `batch_id`, `size`
#' @param min_individuals_G Integer. Minimum number of individuals required for
#'   type "G" batches to generate reliable distributions. Default is 5.
#' @param min_individuals_SL Integer. Minimum number of measured individuals required
#'   for type "S/L" batches. Default is 10.
#' @param verbose Logical. If TRUE, prints messages about filtered records.
#'
#' @return A list containing:
#'   \itemize{
#'     \item `fish_batch`: Cleaned and validated batch data
#'     \item `ind_measure`: Filtered measurement data matching cleaned batches
#'     \item `filtering_log`: Data frame documenting filtered records
#'     \item `validation_issues`: Data frame with specific validation problems
#'   }
#'
#' @note
#' The function performs the following validations:
#' \itemize{
#'   \item Removes measurements without matching batch IDs
#'   \item Filters invalid batch types (only G, S/L, I, N are valid)
#'   \item Removes batches with invalid or missing fish counts (number <= 0 or NA)
#'   \item For type G: validates min/max are not NA, min_length <= max_length, 
#'         and sufficient sample size (>= min_individuals_G)
#'   \item For type S/L: ensures at least min_individuals_SL measured individuals
#'   \item For types I and N: verifies measured count matches batch count
#' }
#'
#' @examples
#' \dontrun{
#' # After running cleaning functions
#' fish_batch_clean <- clean_fish_batch()
#' ind_measure_clean <- clean_individual_measurement_aspe()
#' 
#' # Sanitize batch data
#' sanitized <- sanitize_batch_data(fish_batch_clean, ind_measure_clean)
#' 
#' # Inspect any issues
#' print(sanitized$validation_issues)
#' 
#' # Use with custom thresholds
#' sanitized_strict <- sanitize_batch_data(
#'   fish_batch_clean, 
#'   ind_measure_clean,
#'   min_individuals_G = 10,
#'   min_individuals_SL = 20
#' )
#' }
#'
#' @importFrom dplyr filter select left_join anti_join group_by summarise
#' @export
sanitize_batch_data <- function(
  fish_batch,
  ind_measure,
  min_individuals_G = 5,
  min_individuals_SL = 10,
  verbose = TRUE
) {
  # 1. Input validation --------------------------------------------------------
  if (!is.data.frame(fish_batch)) {
    stop("`fish_batch` must be a data frame", call. = FALSE)
  }

  if (!is.data.frame(ind_measure)) {
    stop("`ind_measure` must be a data frame", call. = FALSE)
  }

  # Check required columns in fish_batch
  required_batch_cols <- c("batch_id", "batch_type", "number", "species_code", 
    "min_length", "max_length")
  missing_batch_cols <- setdiff(required_batch_cols, names(fish_batch))
  if (length(missing_batch_cols) > 0) {
    stop(
      "`fish_batch` is missing required columns: ",
      paste(missing_batch_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Check required columns in ind_measure
  required_measure_cols <- c("batch_id", "size")
  missing_measure_cols <- setdiff(required_measure_cols, names(ind_measure))
  if (length(missing_measure_cols) > 0) {
    stop(
      "`ind_measure` is missing required columns: ",
      paste(missing_measure_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Initialize tracking objects
  filtering_log <- data.frame(
    step = character(),
    records_removed = integer(),
    reason = character(),
    stringsAsFactors = FALSE
  )

  validation_issues <- data.frame(
    batch_id = integer(),
    batch_type = character(),
    issue = character(),
    stringsAsFactors = FALSE
  )

  original_batch_nrow <- nrow(fish_batch)
  original_measure_nrow <- nrow(ind_measure)

  # 3. Filter measurements to match batches -----------------------------------
  ind_measure <- ind_measure |>
    dplyr::filter(batch_id %in% fish_batch$batch_id)

  if (verbose && nrow(ind_measure) < original_measure_nrow) {
    filtering_log <- rbind(filtering_log, data.frame(
      step = "measure_filtering",
      records_removed = original_measure_nrow - nrow(ind_measure),
      reason = "Removed measurements without matching batch IDs",
      stringsAsFactors = FALSE
    ))
  }

  # 4. Validate batch types ---------------------------------------------------
  valid_types <- c("G", "S/L", "I", "N")
  invalid_types <- fish_batch |>
    dplyr::filter(is.na(batch_type) | !(batch_type %in% valid_types))

  if (nrow(invalid_types) > 0) {
    if (verbose) {
      filtering_log <- rbind(filtering_log, data.frame(
        step = "type_validation",
        records_removed = nrow(invalid_types),
        reason = paste("Invalid batch types:", 
          paste(unique(invalid_types$batch_type), collapse = ", ")),
        stringsAsFactors = FALSE
      ))
    }

    validation_issues <- rbind(validation_issues, data.frame(
      batch_id = invalid_types$batch_id,
      batch_type = invalid_types$batch_type,
      issue = "Invalid batch type",
      stringsAsFactors = FALSE
    ))

    fish_batch <- fish_batch |>
      dplyr::filter(!is.na(batch_type) & batch_type %in% valid_types)
  }

  # 5. Validate fish count (number) -------------------------------------------
  invalid_nb <- fish_batch |>
    dplyr::filter(is.na(number) | number <= 0)

  if (nrow(invalid_nb) > 0) {
    if (verbose) {
      filtering_log <- rbind(filtering_log, data.frame(
        step = "count_validation",
        records_removed = nrow(invalid_nb),
        reason = "Invalid or missing fish count (nb <= 0 or NA)",
        stringsAsFactors = FALSE
      ))
    }

    validation_issues <- rbind(validation_issues, data.frame(
      batch_id = invalid_nb$batch_id,
      batch_type = invalid_nb$batch_type,
      issue = "Invalid or missing fish count",
      stringsAsFactors = FALSE
    ))

    fish_batch <- fish_batch |>
      dplyr::filter(!is.na(number) & number > 0)
  }

  # 6. Type-specific validation -----------------------------------------------

  ## Type G validation
  batch_G <- fish_batch |> dplyr::filter(batch_type == "G")
  if (nrow(batch_G) > 0) {
    # Check for NA min/max
    na_minmax <- batch_G |>
      dplyr::filter(is.na(min_length) | is.na(max_length))

    # Check min >= max
    min_ge_max <- batch_G |>
      dplyr::filter(!is.na(min_length) & !is.na(max_length) & min_length > max_length)

    # Check insufficient individuals
    low_n <- batch_G |>
      dplyr::filter(number < min_individuals_G)

    if (nrow(na_minmax) > 0) {
      validation_issues <- rbind(validation_issues, data.frame(
        batch_id = na_minmax$batch_id,
        batch_type = "G",
        issue = "NA in min_length or max_length",
        stringsAsFactors = FALSE
      ))
    }

    if (nrow(min_ge_max) > 0) {
      validation_issues <- rbind(validation_issues, data.frame(
        batch_id = min_ge_max$batch_id,
        batch_type = "G",
        issue = "min_length > max_length",
        stringsAsFactors = FALSE
      ))
    }

    if (nrow(low_n) > 0) {
      validation_issues <- rbind(validation_issues, data.frame(
        batch_id = low_n$batch_id,
        batch_type = "G",
        issue = paste("Number of individuals <", min_individuals_G),
        stringsAsFactors = FALSE
      ))
    }

    invalid_G_ids <- unique(c(na_minmax$batch_id, min_ge_max$batch_id, low_n$batch_id))

    if (length(invalid_G_ids) > 0) {
      if (verbose) {
        filtering_log <- rbind(filtering_log, data.frame(
          step = "type_G_validation",
          records_removed = length(invalid_G_ids),
          reason = "Batches failed type G validation",
          stringsAsFactors = FALSE
        ))
      }

      fish_batch <- fish_batch |>
        dplyr::filter(!(batch_type == "G" & batch_id %in% invalid_G_ids))
    }
  }

  ## Type S/L validation
  batch_SL <- fish_batch |> dplyr::filter(batch_type == "S/L")
  if (nrow(batch_SL) > 0) {
    # Check number of measured individuals
    measured_counts <- ind_measure |>
      dplyr::filter(batch_id %in% batch_SL$batch_id) |>
      dplyr::group_by(batch_id) |>
      dplyr::summarise(n_measured = dplyr::n(), .groups = "drop")

    low_measured <- batch_SL |>
      dplyr::left_join(measured_counts, by = "batch_id") |>
      dplyr::filter(is.na(n_measured) | n_measured < min_individuals_SL)

    if (nrow(low_measured) > 0) {
      validation_issues <- rbind(validation_issues, data.frame(
        batch_id = low_measured$batch_id,
        batch_type = "S/L",
        issue = paste("Measured individuals <", min_individuals_SL),
        stringsAsFactors = FALSE
      ))

      if (verbose) {
        filtering_log <- rbind(filtering_log, data.frame(
          step = "type_SL_validation",
          records_removed = nrow(low_measured),
          reason = paste("Measured individuals <", min_individuals_SL),
          stringsAsFactors = FALSE
        ))
      }

      fish_batch <- fish_batch |>
        dplyr::filter(!(batch_type == "S/L" & batch_id %in% low_measured$batch_id))
    }
  }

  ## Type I and N validation
  for (type in c("I", "N")) {
    batch_type <- fish_batch |> dplyr::filter(batch_type == type)
    if (nrow(batch_type) > 0) {
      measured_counts <- ind_measure |>
        dplyr::filter(batch_id %in% batch_type$batch_id) |>
        dplyr::group_by(batch_id) |>
        dplyr::summarise(n_measured = dplyr::n(), .groups = "drop")

      count_mismatch <- batch_type |>
        dplyr::left_join(measured_counts, by = "batch_id") |>
        dplyr::filter(n_measured != number | is.na(n_measured))

      if (nrow(count_mismatch) > 0) {
        validation_issues <- rbind(validation_issues, data.frame(
          batch_id = count_mismatch$batch_id,
          batch_type = type,
          issue = "Measured count does not match batch count",
          stringsAsFactors = FALSE
        ))

        if (verbose) {
          filtering_log <- rbind(filtering_log, data.frame(
            step = paste0("type_", type, "_validation"),
            records_removed = nrow(count_mismatch),
            reason = "Measured count does not match batch count",
            stringsAsFactors = FALSE
          ))
        }

        fish_batch <- fish_batch |>
          dplyr::filter(!(batch_type == type & batch_id %in% count_mismatch$batch_id))
      }
    }
  }

  # 7. Final filtering of measurements ----------------------------------------
  ind_measure <- ind_measure |>
    dplyr::filter(batch_id %in% fish_batch$batch_id)

  # 8. Summary report ---------------------------------------------------------
  if (verbose) {
    message("\n=== Batch Data Sanitization Summary ===")
    message("Original batches: ", original_batch_nrow)
    message("Valid batches retained: ", nrow(fish_batch))
    message("Removed: ", original_batch_nrow - nrow(fish_batch), " batches")
    message("\nOriginal measurements: ", original_measure_nrow)
    message("Valid measurements retained: ", nrow(ind_measure))
    message("Removed: ", original_measure_nrow - nrow(ind_measure), " measurements")

    if (nrow(validation_issues) > 0) {
      message("\nValidation issues by type:")
      print(table(validation_issues$batch_type, validation_issues$issue))
    }
  }

  # 9. Return results ---------------------------------------------------------
  list(
    fish_batch = fish_batch,
    ind_measure = ind_measure,
    filtering_log = filtering_log,
    validation_issues = validation_issues
  )
}


#' size from batch (AFB)
#' 
#' @param batch data.frame
#' @inheritParams gen_fish_from_batch
get_size_from_batch <- function(
  batch = NULL, id_var = NULL, batch_type_var = NULL, nb_var = NULL,
  min_var = NULL, max_var = NULL, species = NULL,
  measure = NULL, size_var = NULL,
  future_enabled = FALSE, ...){

  id_var <- rlang::enquo(id_var)
  id_var_chr <- rlang::quo_name(id_var)
  batch_type_var <- rlang::enquo(batch_type_var)
  batch_type_var_chr <- rlang::quo_name(batch_type_var)
  nb_var <- rlang::enquo(nb_var)
  nb_var_chr <- rlang::quo_name(nb_var)
  species <- rlang::enquo(species)
  species_var_chr <- rlang::quo_name(species)
  max_var <- rlang::enquo(max_var)
  max_var_chr <- rlang::quo_name(max_var)
  min_var <- rlang::enquo(min_var)
  min_var_chr <- rlang::quo_name(min_var)

  size_var <- rlang::enquo(size_var)
  size_var_chr <- rlang::quo_name(size_var)

  requireNamespace("truncdist", quietly = TRUE)

  # Check batch columns
  required_obj_cols <- c(
    id_var_chr, batch_type_var_chr, nb_var_chr,
    max_var_chr, min_var_chr, species_var_chr
  )
  missing_obj_cols <- setdiff(required_obj_cols, names(batch))
  if (length(missing_obj_cols) > 0) {
    stop(
      "`batch` is missing required columns: ",
      paste(missing_obj_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Check measure columns
  required_obj_cols <- c(
    id_var_chr, size_var_chr
  )
  missing_obj_cols <- setdiff(required_obj_cols, names(measure))
  if (length(missing_obj_cols) > 0) {
    stop(
      "`measure` is missing required columns: ",
      paste(missing_obj_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Filter surnumerous variable:
  batch <- batch %>%
    dplyr::select(!!id_var, !!batch_type_var, !!nb_var, !!species, !!min_var, !!max_var)
  measure <- measure %>%
    dplyr::select(!!id_var, !!size_var)

  # Filter surnumerous id in measure:
  if (any(! measure[[id_var_chr]] %in% batch[[id_var_chr]])) {
    measure <- measure %>%
      dplyr::filter(!!id_var %in% batch[[id_var_chr]])
    message("surnumerous batch in measure were removed")
  }

  # Filter incorrect batch:
  diff_batch_type <- c("G", "S/L", "N", "I")
  if (any(is.na(batch[[batch_type_var_chr]]) |
      any(!batch[[batch_type_var_chr]] %in% diff_batch_type))
    ) {
    batch <- batch %>%
      dplyr::filter(! is.na(!!batch_type_var) & !!batch_type_var %in% diff_batch_type)
    message("NA batch id and batch type has been filtered")
  }

  # Filter if effectif is not present:
  if (any(is.na(batch[[nb_var_chr]])) | any(!batch[[nb_var_chr]] > 0)) {
    batch <- batch %>%
      dplyr::filter( (!is.na(!!nb_var)) & !!nb_var > 0)
    message("Incorrect effectif has been filtered")
  }

  na_G <- batch %>%
    dplyr::filter(!!batch_type_var == "G" & (is.na(!!min_var) | is.na(!!max_var)))
  incorrect_G <-  batch %>%
    dplyr::filter(!!batch_type_var == "G" & !!min_var >= !!max_var)
  low_n_G <-  batch %>%
    dplyr::filter(!!batch_type_var == "G" & !!nb_var < 5)

  if (any(c(nrow(na_G), nrow(incorrect_G)) != 0)) {
    G_bad_id <- c(na_G[[id_var_chr]], incorrect_G[[id_var_chr]], low_n_G[[id_var_chr]])

    batch <- batch %>%
      dplyr::filter(!( !!id_var %in% G_bad_id))
    message("incorrect batch G have been filtered")
  }


  gen_fish_from_batch <- compiler::cmpfun(gen_fish_from_batch)
  batch$size_mm <- parallel::mcMap(gen_fish_from_batch,
	  id = batch[[id_var_chr]],
	  type = batch[[batch_type_var_chr]],
	  min_size = batch[[min_var_chr]],
	  max_size = batch[[max_var_chr]],
	  nb = batch[[nb_var_chr]],
	  MoreArgs = list(
	    ind_measure = measure,
	    ind_size = size_var_chr,
      id_var = id_var
	  )
	)

  # Filter and extract:
  output <- batch %>%
  dplyr::select(!!id_var, !!species, size_mm)

  output
}

#' Generate fish from fishing batch (AFB) 
#'
#'
#' @param id int id of the batch  
#' @param type character type of the batch (N, G, S/L, I)
#' @param min_size dbl minimum size of the batch 
#' @param max_size dbl maximum size of the batch 
#' @param nb int effectif of the batch 
#' @param ind_measure data.frame individual measurement of the fish 
#' @param ind_size variable name for the size in ind_measure 
#'
#' @details From fishing batch, we build generate the fish size individual
#'
#' @export
gen_fish_from_batch <- function (
  id = NULL, type = NULL,  min_size = NULL, max_size = NULL, nb = NULL,
  ind_measure = NULL, id_var = NULL, ind_size = NULL, ...) {

  # Promise:
  ind_size <- rlang::enquo(ind_size)
  id_var <- rlang::enquo(id_var)

  # Build by batch
  if (type == "G") {
    if (any(is.na(c(min_size, max_size)))) {
      warning_msg <- paste(
	"NA min_size or max_size in batch of type G number ",id,
	", batch put as NA\n", sep = ""
      )
      warning(warning_msg)
      batch <- rep(NA, nb)
    } else if (min_size >= max_size) {
      warning_msg <- paste(
    "min_size >= max_size in batch of type G number ",id,", batch put as NA\n",
    sep = "")
      warning(warning_msg)
      batch <- NA
    } else if (nb < 5) {
      warning_msg <- paste(
      "# of obs is inferior to 10 (actual # is,", length(nb),
      ") in batch type G number", id,".\n", "batch put as NA\n", sep = "")
      warning(warning_msg)
      batch <- rep(NA, nb)
    } else {
      avg <- (min_size + max_size) / 2
      sdt <- (max_size - min_size) * 1 / 4

      batch <- truncdist::rtrunc(n = nb, spec = "norm", a = min_size, b = max_size,
	mean = avg, sd = sdt)
      stopifnot(length(batch) == nb)
    }
  } else if (type == "S/L") {
    #Get size:
    mask <- which(ind_measure[[rlang::quo_name(id_var)]] == id)
    size <- ind_measure[mask, ][[rlang::quo_name(ind_size)]]
    size <- na.omit(size)
    stopifnot(is.na(size) | nrow(size) == 0)
    # Sanity check:
    if (length(size) < 10) {
      warning_msg <- paste(
      "# of obs is inferior to 10 (actual # is,", length(size),
      ") in batch type S/L number ", id,".\n", "batch put as NA\n", sep = "")
      warning(warning_msg)
      batch <- rep(NA, nb)
    } else {
    #Distribution parameters:
    avg <- mean(size)
    sdt <- sd(size)

    # Sample inside the 90% of the distribution probability:
    p05 <- quantile(size, 0.05)
    p95 <- quantile(size, 0.95)

    # Error if p05 >= p95
    if (p05 >= p95) {
      warning_msg <- paste(
      "p05 is equal or greater than p95 (p05 = ", p05, ", P95 = ", p95,
      ") in batch type S/L number ", id,".\n", "batch put as NA\n", sep = "")
      warning(warning_msg)
      batch <- rep(NA, nb)
    } else {
      batch <- truncdist::rtrunc(n = nb, spec = "norm", a = p05, b = p95,
	mean = avg, sd = sdt)
    }
    }
  } else if (type == "I") {
    # All individuals have been measured:
    mask <- which(ind_measure[[rlang::quo_name(id_var)]] == id)
    batch <- ind_measure[mask, ][[rlang::quo_name(ind_size)]]
    stopifnot(length(batch) == nb)
  } else if (type == "N") {
    # One big individual:
    mask <- which(ind_measure[[rlang::quo_name(id_var)]] == id)
    batch <- ind_measure[mask, ][[rlang::quo_name(ind_size)]]
    stopifnot(length(batch) == 1)
  }
  # Round to milimeters:
  round(batch)
}

#' Main function for batch check
#'
#' return a data.frame with batch id, species and the  
#'
#' @export
get_check_batch <- function(
  batch = NULL, id_var = NULL, batch_type_var = NULL, nb_var = NULL,
  min_var = NULL, max_var = NULL, species = NULL,
  measure = NULL, size_var = NULL,
  future_enabled = FALSE, ...){

  id_var <- rlang::enquo(id_var)
  id_var_chr <- rlang::quo_name(id_var)
  batch_type_var <- rlang::enquo(batch_type_var)
  batch_type_var_chr <- rlang::quo_name(batch_type_var)
  nb_var <- rlang::enquo(nb_var)
  nb_var_chr <- rlang::quo_name(nb_var)
  species <- rlang::enquo(species)
  max_var <- rlang::enquo(max_var)
  min_var <- rlang::enquo(min_var)

  size_var <- rlang::enquo(size_var)
  size_var_chr <- rlang::quo_name(size_var)

  # Filter surnumerous variable:
  batch <- batch %>%
    dplyr::select(!!id_var, !!batch_type_var, !!nb_var, !!species, !!min_var, !!max_var)
  measure <- measure %>%
    dplyr::select(!!id_var, !!size_var)

  # Filter surnumerous id in measure:
  if (any(! measure[[id_var_chr]] %in% batch[[id_var_chr]])) {
  measure <- measure %>%
      dplyr::filter(!!id_var %in% batch[[id_var_chr]])
    message("surnumerous batch in measure were removed")
  }

  check_batch <- compiler::cmpfun(check_batch)
  output <- batch %>%
    dplyr::mutate(
      size_mm =
      parallel::mcMap(check_batch,
	  id = !!id_var,
	  type = !!batch_type_var,
	  min_size = !!min_var,
	  max_size = !!max_var,
	  nb = !!nb_var,
	  MoreArgs = list(
	    ind_measure = measure,
	    ind_size = size_var_chr
	  )
	)
      ) %>%
  dplyr::select(!!id_var, !!species, size_mm) %>%
  tidyr::unnest(size_mm)

  output
}

#' Fish batch checker (AFB) 
#'
#' Check the quality of the batch 
#'
#' @inheritParams gen_fish_from_batch
#'
#' @details The function check if there are missing datas that can make the batch
#' invalid. The check are specific of each batch type. For batch "G", we check if
#' min or max size are missing, if min >= max size and the number of fish in the
#' batch is superior or equal to 10. For batch "S/L", we check if the number of measured
#' fishes is superior to 20 (standard protocol propose 30 be measured). In batch
#' "I", we check that the number of measured fishes correspond to the number of
#' fish recorded in the batch. For batch "N", we check if the number of measured
#' fish is equal to one.    
#' 
#' @return a list. Each element of the list correspond to a batch of a fishing
#' operation. If there is no problem in the batch, it returns NA. In the opposite
#' case, the function returns a character containing the type of error.
#' @export
check_batch <- function (
  id = NULL, type = NULL,  min_size = NULL, max_size = NULL, nb = NULL,
  ind_measure = NULL, ind_id = NULL, ind_size = NULL, ...) {


  # Build by batch
  if (type == "G") {
    if (any(is.na(c(min_size, max_size)))) {
      status <- paste("min_size or/and max_size is NA")
    } else if (min_size >= max_size) {
      status <- paste("min_size >= max_size")
    } else if (nb < 10) {
      status <- paste("# < 10")
    } else {
      status <- "good"
    }
  } else if (type == "S/L") {
    #Get size:
    mask <- which(ind_measure[[ind_id]] == id)
    size <- ind_measure[mask, ][[ind_size]]
    size <- na.omit(size)
    stopifnot(is.na(size) | nrow(size) == 0)
    # Sanity check:
    if (length(size) < 20) {
      status <- paste("# < 20 (excluding NA)")
    } else {
      status <- "good"
    }
  } else if (type == "I") {
    # All individuals have been measured:
    mask <- which(ind_measure[[ind_id]] == id)
    batch <- ind_measure[mask, ][[ind_size]]
    if (length(batch) == nb) {
      status <- "good"
    } else {
      status <- "# of measured fish do not match sample size"
    }
  } else if (type == "N") {
    # One big individual:
    mask <- which(ind_measure[[ind_id]] == id)
    batch <- ind_measure[mask, ][[ind_size]]
    if (length(batch) == nb) {
      status <- "good"
    } else {
      status <- "# of measured fish do not match sample size"
    }
  }
  status 
}
