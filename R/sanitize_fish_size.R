#' Convert fork length measurements to total length for fish species
#'
#' Converts fork length (FL) measurements to total length (TL) using species-specific
#' conversion coefficients derived from FishBase and manual sources.
#'
#' @param fish_batch A data frame containing fish batch data.
#' @param ind_measure A data frame containing individual fish measurements.
#' @param species_ref A data frame containing species reference data.
#' @param fishbase_length_length Optional data frame of pre-fetched FishBase
#'   length-length relationships. If `NULL`, fetches from FishBase automatically.
#' @param conversion_vector Named vector of manual conversion coefficients.
#'   Default uses `coefficients_fork2total()`.
#' @param convert_intercept_cm2mm Logical. If `TRUE`, converts FishBase intercepts
#'   from cm to mm. Default is `TRUE`.
#' @param manual_priority Logical. If `TRUE` (default), manual coefficients take
#'   precedence over FishBase. If `FALSE`, FishBase coefficients take precedence
#'   and manual coefficients are used only for species missing from FishBase.
#' @param verbose Logical. If `TRUE`, prints messages about coefficient sources.
#'   Default is `TRUE`.
#'
#' @return A list containing:
#'   \itemize{
#'     \item `ind_measure`: Updated individual measurements with converted lengths
#'     \item `fish_batch`: Updated batch data with converted lengths
#'     \item `conversion_log`: Data frame documenting applied conversions
#'     \item `unconverted_species`: Species codes that could not be converted
#'     \item `source_log`: Data frame documenting coefficient sources per species
#'   }
#'
#' @export
convert_fork_to_total <- function(
  fish_batch = clean_fish_batch(),
  ind_measure = clean_individual_measurement_aspe(),
  species_ref = cleaning_species_ref_aspe(),
  fishbase_length_length = NULL,
  conversion_vector = coefficients_fork2total(),
  convert_intercept_cm2mm = TRUE,
  manual_priority = TRUE,
  verbose = TRUE
) {
  # Input validation
  if (!is.data.frame(fish_batch)) stop("fish_batch must be a data frame")
  if (!is.data.frame(ind_measure)) stop("ind_measure must be a data frame")
  if (!is.data.frame(species_ref)) stop("species_ref must be a data frame")

  required_fish_batch_cols <- c(
    "batch_id", "species_code", "length_type",
    "min_length", "max_length")
  missing_cols <- setdiff(required_fish_batch_cols, names(fish_batch))
  if (length(missing_cols) > 0) {
    stop("fish_batch missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  required_ind_measure_cols <- c("measure_id", "batch_id", "species_code", "size")
  missing_cols <- setdiff(required_ind_measure_cols, names(ind_measure))
  if (length(missing_cols) > 0) {
    stop("ind_measure missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # A. Identify fork length measurements
  batch_fork_ids <- fish_batch |>
    dplyr::filter(length_type == "fork") |>
    dplyr::pull(batch_id)

  if (length(batch_fork_ids) == 0) {
    if (verbose) message("No fork length measurements found. Returning unchanged data.")
    return(list(
      ind_measure = ind_measure,
      fish_batch = fish_batch,
      conversion_log = data.frame(),
      unconverted_species = character(),
      source_log = data.frame()
    ))
  }

  # B. Get species with fork measurements
  species_fork <- fish_batch |>
    dplyr::filter(batch_id %in% batch_fork_ids) |>
    dplyr::distinct(species_code) |>
    dplyr::left_join(
      dplyr::select(species_ref, species_code, latin_name),
      by = "species_code"
    )

  # C. Fetch FishBase data if needed and not provided
  if (is.null(fishbase_length_length) && nrow(species_fork) > 0) {
    if (verbose) message("Fetching length-length relationships from FishBase...")
    if (!requireNamespace("rfishbase", quietly = TRUE)) {
      warning("Package 'rfishbase' is not installed. Only manual coefficients will be used.")
    } else {
      # Get only species that have manual coefficients? No, get all to compare
      species_list <- species_fork$latin_name[!is.na(species_fork$latin_name)]
      if (length(species_list) > 0) {
        fishbase_length_length <- tryCatch(
          rfishbase::length_length(species_list = species_list),
          error = function(e) {
            warning("Failed to fetch FishBase data: ", e$message)
            NULL
          }
        )
      }
    }
  }

  # D. Build conversion coefficients
  coeffs_result <- build_conversion_coefficients(
    species_fork = species_fork,
    fishbase_length_length = fishbase_length_length,
    conversion_vector = conversion_vector,
    convert_intercept_cm2mm = convert_intercept_cm2mm,
    manual_priority = manual_priority,
    verbose = verbose
  )

  # E. Apply conversion to individual measurements
  ind_measure_fork <- ind_measure |>
    dplyr::filter(batch_id %in% batch_fork_ids) |>
    dplyr::left_join(coeffs_result$coeffs, by = "species_code")
  ind_measure_fork <- rbind(
    ind_measure_fork |>
      dplyr::filter(!(is.na(b) | is.na(a))) |>
      dplyr::mutate(
        original_size = size,
        size = a + size * b
      ),
    ind_measure_fork |>
      dplyr::filter(is.na(b) | is.na(a)) |>
      dplyr::mutate(
        original_size = size,
      )
    )

  # F. Apply conversion to batch data
  batch_fork <- fish_batch |>
    dplyr::filter(batch_id %in% batch_fork_ids) |>
    dplyr::left_join(coeffs_result$coeffs, by = "species_code")
  batch_fork <- rbind(
    batch_fork |>
      dplyr::filter(!(is.na(b) | is.na(a))) |>
      dplyr::mutate(
        min_length_original = min_length,
        max_length_original = max_length,
        min_length = a + min_length * b,
        max_length = a + max_length * b,
        length_type = "total"
      ),
    batch_fork |>
      dplyr::filter(is.na(b) | is.na(a)) |>
      dplyr::mutate(
        min_length_original = min_length,
        max_length_original = max_length
      )
  )

  # G. Validate conversions
  validate_conversion(ind_measure_fork, batch_fork)
  
  # H. Replace original data with converted data
  ind_measure_updated <- ind_measure |>
    dplyr::filter(!batch_id %in% batch_fork_ids) |>
    dplyr::bind_rows(
      ind_measure_fork |>
        dplyr::select(-a, -b, -original_size)
    )
  
  fish_batch_updated <- fish_batch |>
    dplyr::filter(!batch_id %in% batch_fork_ids) |>
    dplyr::bind_rows(
      batch_fork |>
        dplyr::select(-a, -b, -min_length_original, -max_length_original)
    )
  
  # I. Return results
  list(
    ind_measure = ind_measure_updated,
    fish_batch = fish_batch_updated,
    conversion_log = coeffs_result$coeffs,
    unconverted_species = coeffs_result$missing_species,
    source_log = coeffs_result$sources
  )
}

#' Remove biologically impossible fish lengths
#'
#' Identifies and removes or flags fish length measurements that exceed known
#' maximum lengths for the species. Also reconciles batch-level length statistics
#' with individual measurements for S/L batches.
#'
#' @param ind_measure A data frame containing individual fish measurements.
#'   Must contain columns: `measure_id`, `batch_id`, `species_code`, `size`.
#' @param fish_batch A data frame containing fish batch data.
#'   Must contain columns: `batch_id`, `species_code`, `batch_type`, 
#'   `min_length`, `max_length`.
#' @param species_ref A data frame containing species reference data.
#'   Must contain columns: `species_code`, `maximal_length_mm`.
#' @param remove_outliers Logical. If `TRUE`, replaces impossible lengths with NA.
#'   If `FALSE`, only flags them. Default is `TRUE`.
#'
#' @return A list containing:
#'   \itemize{
#'     \item `ind_measure`: Updated individual measurements with flagged/removed outliers
#'     \item `fish_batch`: Updated batch data with reconciled length statistics
#'     \item `outliers`: Data frame of detected outliers
#'     \item `outlier_summary`: Summary statistics of removed values
#'   }
#'
#' @details
#' The function performs two main validation steps:
#' 
#' **1. Individual measurement validation**
#'   - Compares each measurement against species-specific maximum length
#'   - Flags or removes measurements exceeding the maximum
#' 
#' **2. Batch-level validation for S/L batches**
#'   - For batches with "S/L" type, recalculates min/max from individual measurements
#'   - Ensures batch-level statistics are consistent with individual data
#'   - Validates batch min/max against species maximum lengths
#'
#' @importFrom dplyr filter left_join group_by summarise mutate bind_rows
#' @importFrom rlang .data
#' @export
remove_impossible_lengths <- function(
  ind_measure,
  fish_batch,
  species_ref,
  remove_outliers = TRUE
) {
  # Input validation
  if (!is.data.frame(ind_measure)) stop("ind_measure must be a data frame")
  if (!is.data.frame(fish_batch)) stop("fish_batch must be a data frame")
  if (!is.data.frame(species_ref)) stop("species_ref must be a data frame")
  
  required_ind_cols <- c("measure_id", "batch_id", "species_code", "size")
  missing_ind <- setdiff(required_ind_cols, names(ind_measure))
  if (length(missing_ind) > 0) {
    stop("ind_measure missing columns: ", paste(missing_ind, collapse = ", "))
  }
  
  required_batch_cols <- c("batch_id", "species_code", "batch_type", 
                           "min_length", "max_length")
  missing_batch <- setdiff(required_batch_cols, names(fish_batch))
  if (length(missing_batch) > 0) {
    stop("fish_batch missing columns: ", paste(missing_batch, collapse = ", "))
  }
  
  required_ref_cols <- c("species_code", "maximal_length_mm")
  missing_ref <- setdiff(required_ref_cols, names(species_ref))
  if (length(missing_ref) > 0) {
    stop("species_ref missing columns: ", paste(missing_ref, collapse = ", "))
  }
  
  # A. Add max length reference to individual measurements
  ind_measure <- ind_measure |>
    dplyr::left_join(
      dplyr::select(species_ref, species_code, maximal_length_mm),
      by = "species_code"
    )
  
  # Check for missing max lengths
  missing_max <- ind_measure |> 
    dplyr::filter(is.na(maximal_length_mm)) |>
    dplyr::pull(species_code) |>
    unique()
  
  if (length(missing_max) > 0) {
    warning("Missing maximal length for species: ", 
            paste(missing_max, collapse = ", "))
  }
  
  # B. Identify outliers
  outliers <- ind_measure |>
    dplyr::filter(!is.na(size), !is.na(maximal_length_mm), 
                  size > maximal_length_mm)
  
  # C. Handle outliers
  if (remove_outliers) {
    ind_measure_cleaned <- ind_measure |>
      dplyr::mutate(
        size = dplyr::if_else(
          size > maximal_length_mm & !is.na(maximal_length_mm),
          NA_real_,
          size
        )
      )
    
    outlier_summary <- outliers |>
      dplyr::group_by(species_code) |>
      dplyr::summarise(
        n_outliers = dplyr::n(),
        mean_excess_mm = mean(size - maximal_length_mm, na.rm = TRUE)
      )
  } else {
    ind_measure_cleaned <- ind_measure
    outlier_summary <- data.frame()
  }
  
  # D. Reconcile S/L batches
  sl_batches <- fish_batch |>
    dplyr::filter(batch_type == "S/L")
  
  if (nrow(sl_batches) > 0) {
    # Recalculate min/max from individual measurements
    sl_batch_stats <- ind_measure_cleaned |>
      dplyr::filter(batch_id %in% sl_batches$batch_id) |>
      dplyr::group_by(batch_id) |>
      dplyr::summarise(
        min_size = min(size, na.rm = TRUE),
        max_size = max(size, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Update batch data
    sl_batches_updated <- sl_batches |>
      dplyr::left_join(sl_batch_stats, by = "batch_id") |>
      dplyr::mutate(
        min_length = min_size,
        max_length = max_size
      ) |>
      dplyr::select(-min_size, -max_size)
    
    fish_batch_updated <- fish_batch |>
      dplyr::filter(batch_type != "S/L") |>
      dplyr::bind_rows(sl_batches_updated)
  } else {
    fish_batch_updated <- fish_batch
  }
  
  # E. Validate batch-level outliers
  fish_batch_updated <- fish_batch_updated |>
    dplyr::left_join(
      dplyr::select(species_ref, species_code, maximal_length_mm),
      by = "species_code"
    )
  
  batch_outliers <- fish_batch_updated |>
    dplyr::filter(
      (max_length > maximal_length_mm & !is.na(max_length)) |
      (min_length > maximal_length_mm & !is.na(min_length))
    )
  
  if (nrow(batch_outliers) > 0) {
    warning("Found ", nrow(batch_outliers), " batches with impossible lengths")
  }
  
  # F. Return results
  list(
    ind_measure = ind_measure_cleaned |> dplyr::select(-maximal_length_mm),
    fish_batch = fish_batch_updated |> dplyr::select(-maximal_length_mm),
    outliers = outliers,
    outlier_summary = outlier_summary,
    batch_outliers = batch_outliers
  )
}

#' Validate fork-to-total length conversion results
#'
#' @keywords internal
#' @noRd
validate_conversion <- function(ind_measure_fork, batch_fork) {
  # Only warn if converted lengths are significantly smaller (allow for rounding)
  if (nrow(ind_measure_fork) > 0) {
    decreased <- ind_measure_fork$size < (ind_measure_fork$original_size - 0.01)
    if (any(decreased, na.rm = TRUE)) {
      warning("Some individual measurements decreased after conversion")
    }
  }
  
  if (nrow(batch_fork) > 0) {
    min_decreased <- batch_fork$min_length < (batch_fork$min_length_original - 0.01)
    if (any(min_decreased, na.rm = TRUE)) {
      warning("Some batch minimum lengths decreased after conversion")
    }
    
    max_decreased <- batch_fork$max_length < (batch_fork$max_length_original - 0.01)
    if (any(max_decreased, na.rm = TRUE)) {
      warning("Some batch maximum lengths decreased after conversion")
    }
  }
}

#' Build conversion coefficients from FishBase and manual sources
#'
#' Internal helper function to combine FishBase length-length relationships
#' with manual conversion coefficients.
#'
#' @param species_fork Data frame with species that have fork length measurements
#' @param fishbase_length_length Optional pre-fetched FishBase data
#' @param conversion_vector Named vector of manual conversion coefficients
#' @param convert_intercept_cm2mm Logical, convert intercept from cm to mm
#' @param manual_priority Logical. If TRUE (default), manual coefficients take
#'   precedence over FishBase. If FALSE, FishBase coefficients take precedence
#'   and manual coefficients are used only for species missing from FishBase.
#' @param verbose Logical. If TRUE, prints messages about coefficient sources.
#'
#' @return A list containing:
#'   \itemize{
#'     \item `coeffs`: Data frame with conversion coefficients
#'     \item `sources`: Data frame documenting source for each species
#'   }
#'
#' @keywords internal
#' @noRd
build_conversion_coefficients <- function(
  species_fork,
  fishbase_length_length,
  conversion_vector,
  convert_intercept_cm2mm = TRUE,
  manual_priority = TRUE,
  verbose = TRUE
) {
  # Get species names
  species_code_name <- species_fork |>
    dplyr::select(species_code, latin_name) |>
    dplyr::distinct()
  
  # Initialize results
  all_coeffs <- data.frame()
  source_log <- data.frame()
  
  # Process FishBase coefficients
  fb_coeffs <- NULL
  if (!is.null(fishbase_length_length) && nrow(fishbase_length_length) > 0) {
    fb_coeffs <- fishbase_length_length |>
      dplyr::filter(Length1 == "TL", Length2 == "FL") |>
      dplyr::group_by(Species) |>
      dplyr::summarise(a = mean(a), b = mean(b), .groups = "drop") |>
      dplyr::mutate(a = if (convert_intercept_cm2mm) a * 10 else a) |>
      dplyr::left_join(species_code_name, by = c("Species" = "latin_name")) |>
      dplyr::select(species_code, a, b) |>
      dplyr::filter(!is.na(species_code))
    
    # Add source information
    if (nrow(fb_coeffs) > 0) {
      fb_coeffs$source <- "FishBase"
    }
  }
  
  # Process manual coefficients
  manual_coeffs <- conversion_vector |>
    tibble::enframe(name = "species_code", value = "b") |>
    dplyr::mutate(
      species_code = as.character(species_code),
      b = as.double(b)
    ) |>
    dplyr::mutate(a = 0, source = "Manual")
 
  # Combine based on priority
  if (manual_priority) {
    # Manual takes precedence
    if (!is.null(fb_coeffs) && nrow(fb_coeffs) > 0) {
      # Start with FishBase
      all_coeffs <- fb_coeffs

      # Add manual coefficients (will override)
      if (!is.null(manual_coeffs) && nrow(manual_coeffs) > 0) {
        all_coeffs <- all_coeffs |>
          dplyr::rows_upsert(manual_coeffs, by = "species_code")
      }
    } else {
      all_coeffs <- manual_coeffs
    }
  } else {
    # FishBase takes precedence (manual fills gaps)
    if (!is.null(manual_coeffs) && nrow(manual_coeffs) > 0) {
      # Start with manual
      all_coeffs <- manual_coeffs

      # Add FishBase coefficients (will override)
      if (!is.null(fb_coeffs) && nrow(fb_coeffs) > 0) {
        all_coeffs <- all_coeffs |>
          dplyr::rows_upsert(fb_coeffs, by = "species_code")
      }
    } else {
      all_coeffs <- fb_coeffs
    }
  }
 
  # Document coefficient sources
  if (!is.null(fb_coeffs) && !is.null(manual_coeffs)) {
    overlapping <- intersect(fb_coeffs$species_code, manual_coeffs$species_code)
 
    if (length(overlapping) > 0 && verbose) {
      priority_msg <- if (manual_priority) "manual" else "FishBase"
      message(
        "Species with coefficients from both sources: ",
        paste(overlapping, collapse = ", "),
        ". Using ", priority_msg, " coefficients."
      )
    }
    
    # Create source log
    source_log <- data.frame(
      species_code = unique(c(fb_coeffs$species_code, manual_coeffs$species_code))
    ) |>
      dplyr::mutate(
        has_fishbase = species_code %in% fb_coeffs$species_code,
        has_manual = species_code %in% manual_coeffs$species_code,
        source_used = dplyr::case_when(
          has_manual & manual_priority ~ "Manual",
          has_fishbase & !manual_priority ~ "FishBase",
          has_fishbase & !has_manual ~ "FishBase",
          has_manual & !has_fishbase ~ "Manual",
          TRUE ~ "None"
        )
      )
  }
  
  # Check for missing species
  missing_species <- setdiff(species_fork$species_code, all_coeffs$species_code)
  if (length(missing_species) > 0 && verbose) {
    warning("No conversion coefficients for species: ", 
            paste(missing_species, collapse = ", "))
  }
  
  # Validate coefficients
  if (nrow(all_coeffs) > 0) {
    if (any(all_coeffs$b <= 0, na.rm = TRUE)) {
      warning("Non-positive slope coefficients detected")
    }
    if (any(all_coeffs$b > 1.2, na.rm = TRUE)) {
      warning("Unusually large slope coefficients found: ", 
        paste(all_coeffs$b[all_coeffs$b > 1.2], collapse = ", "))
    }
  }
  list(
    coeffs = all_coeffs,
    sources = source_log,
    missing_species = missing_species
  )
}

#' Conversion coefficients from fork length to total length for fish species
#'
#' Provides a named vector of conversion factors to estimate total length (TL)
#' from fork length (FL) for various fish species in the ASPE database. These
#' coefficients allow standardization of length measurements across species
#' for comparative analyses.
#'
#' @return A named numeric vector where:
#'   \itemize{
#'     \item Names are three-letter ASPE species codes
#'     \item Values are multiplication factors to convert fork length to total
#'           length (i.e., TL = coefficient × FL)
#'   }
#'
#' @details
#' The function returns conversion coefficients derived from:
#' \itemize{
#'   \item **FishBase morphometric data**: Coefficients calculated from length-length
#'         relationships where available
#'   \item **Expert judgment**: For species with minimal tail fork or where
#'         morphometric data are unavailable, a coefficient of 1.0 is used
#'         (fork length approximates total length)
#'   \item **Related species**: For some species, coefficients from closely related
#'         species are used when direct measurements are unavailable
#' }
#'
#' @section Conversion coefficients by species:
#' \describe{
#'   \item{ALA}{*Alosa alosa* — 1.0989 (TL = FL × 1.0989) — 
#'         [FishBase data](https://fishbase.se/physiology/MorphMetSummaryV2.php?picname=Alalo_u1.gif)}
#'   \item{LOF}{*Barbatula barbatula* — 1.0 — Tail rect, minor difference}
#'   \item{LPP}{*Lampetra planeri* — 1.0 — Tail rect, no difference — 
#'         [FishBase data](https://fishbase.se/physiology/MorphMetSummaryV2.php?picname=Lapla_u3.jpg)}
#'   \item{CHE}{*Squalius cephalus* — 1.0638 (TL = FL × 1.0638) — 
#'         [FishBase data](https://www.fishbase.se/physiology/MorphMetSummaryV2.php?picname=Lecep_u2.jpg)}
#'   \item{VAR}{*Leuciscus leuciscus* — 1.0881 — Coefficient from closely related
#'         *L. leuciscus* [FishBase data](https://fishbase.se/physiology/MorphMetSummaryV2.php?picname=Leleu_u1.jpg)}
#'   \item{MUP}{*Chelon ramada* — 1.070 — 
#'         [FishBase length-length data](https://www.fishbase.se/popdyn/LLRelationshipList.php?ID=4583)}
#'   \item{GTN}{*Neogobius melanostomus* — 1.0 — 
#'         [FishBase data](https://www.fishbase.se/physiology/MorphMetSummaryV2.php?picname=Nemel_u3.gif)}
#'   \item{TAC}{*Oncorhynchus mykiss* — 1.0183 — 
#'         [FishBase data](https://www.fishbase.se/physiology/MorphMetSummaryV2.php?picname=Onmyk_m1.jpg)}
#'   \item{PAP}{*Pachychilon pictum* — 1.0787 — 
#'         [FishBase data](https://www.fishbase.se/physiology/MorphMetSummaryV2.php?picname=Papic_u2.jpg)}
#'   \item{TOX}{*Leuciscus leuciscus* — 1.0881 — Coefficient from closely related
#'         *L. leuciscus* [FishBase data](https://fishbase.se/physiology/MorphMetSummaryV2.php?picname=Leleu_u1.jpg)}
#'   \item{PER}{*Perca fluviatilis* — 1.0493 — 
#'         [FishBase data](https://www.fishbase.se/physiology/MorphMetSummaryV2.php?picname=Peflu_u0.gif)}
#'   \item{GKS}{*Ponticola kessleri* — 1.0 — Tail rect, no difference — 
#'         [FishBase data](https://www.fishbase.se/physiology/MorphMetSummaryV2.php?picname=Nekes_u0.jpg)}
#'   \item{GDL}{*Gymnocephalus cernua* — 1.0 — Tail rect, no difference — 
#'         [FishBase photo](https://fishbase.se/photos/ThumbnailsSummary.php?ID=65128)}
#'   \item{SAN}{*Sander lucioperca* — 1.0684 — 
#'         [FishBase data](https://www.fishbase.se/physiology/MorphMetSummaryV2.php?picname=Saluc_u2.jpg)}
#'   \item{APR}{*Zingel asper* — 1.0 — Tail almost rect, no measured difference
#'         for *Zingel* genus in FishBase}
#' }
#'
#' @note
#' - Coefficients are derived from the relationship: TL = coefficient × FL
#' - For species where coefficient > 1, total length exceeds fork length (forked tail)
#' - For species where coefficient = 1, tail is straight or minimally forked
#' - Sources are hyperlinked in the documentation for verification
#' - Use with [dplyr::recode()] or similar to apply to data
#'
#' @examples
#' \dontrun{
#' # Get coefficients for all species
#' coeffs <- coefficients_fork2total()
#' 
#' # Apply conversion to a data frame
#' fish_data <- fish_data %>%
#'   dplyr::mutate(
#'     total_length = fork_length * coefficients_fork2total()[species_code]
#'   )
#' 
#' # Get coefficient for a specific species
#' coeff <- coefficients_fork2total()["PER"]  # Perca fluviatilis
#' 
#' # Convert only species with available coefficients
#' species_with_coeff <- names(coefficients_fork2total())
#' fish_to_convert <- fish_data %>%
#'   dplyr::filter(species_code %in% species_with_coeff)
#' }
#'
#' @seealso
#' - [rfishbase::morphometrics()] for accessing FishBase morphometric data
#' - [sanitize_species_code()] for species code standardization
#' @export
coefficients_fork2total <- function() {
  c(
    ALA = 1/.91,  # Fishbase: https://fishbase.se/physiology/MorphMetSummaryV2.php?picname=Alalo_u1.gif&genusname=Alosa&speciesname=alosa&id=101
    LOF =  1,     # Tail rect, minor difference
    LPP = 1,      # Tail rect no difference https://fishbase.se/physiology/MorphMetSummaryV2.php?picname=Lapla_u3.jpg&genusname=Lampetra&speciesname=planeri&id=4481
    CHE = 1/.940, # Fishbase: https://www.fishbase.se/physiology/MorphMetSummaryV2.php?picname=Lecep_u2.jpg&genusname=Squalius&speciesname=cephalus&id=4482
    # Fishbase taken from Leuciscus leuciscus which is very close:
    # https://fishbase.se/physiology/MorphMetSummaryV2.php?picname=Leleu_u1.jpg&genusname=Leuciscus&speciesname=leuciscus&id=4662
    VAR = 1/.919,
    MUP = 1.070,  # Chelon ramada is the valid synonym: https://www.fishbase.se/popdyn/LLRelationshipList.php?ID=4583&GenusName=Chelon&SpeciesName=ramada&fc=359
    GTN = 1.0,    # Fisbase: https://www.fishbase.se/physiology/MorphMetSummaryV2.php?picname=Nemel_u3.gif&genusname=Neogobius&speciesname=melanostomus&id=12019
    TAC = 1/.982, # Fishbase: https://www.fishbase.se/physiology/MorphMetSummaryV2.php?picname=Onmyk_m1.jpg&genusname=Oncorhynchus&speciesname=mykiss&id=239
    PAP = 1/.927, # Fishbase: https://www.fishbase.se/physiology/MorphMetSummaryV2.php?picname=Papic_u2.jpg&genusname=Pachychilon&speciesname=pictum&id=26627
    # Fishbase taken from Leuciscus leuciscus which is very close:
    # https://fishbase.se/physiology/MorphMetSummaryV2.php?picname=Leleu_u1.jpg&genusname=Leuciscus&speciesname=leuciscus&id=4662
    TOX = 1/.919,
    PER = 1/.953, # Fishbase: https://www.fishbase.se/physiology/MorphMetSummaryV2.php?picname=Peflu_u0.gif&genusname=Perca&speciesname=fluviatilis&id=358
    GKS = 1.0,    # Tail rect no difference: https://www.fishbase.se/physiology/MorphMetSummaryV2.php?picname=Nekes_u0.jpg&genusname=Ponticola&speciesname=kessleri&id=25977
    GDL = 1.0,    # Tail rect no difference: https://fishbase.se/photos/ThumbnailsSummary.php?ID=65128
    SAN = 1/.936, # https://www.fishbase.se/physiology/MorphMetSummaryV2.php?picname=Saluc_u2.jpg&genusname=Sander&speciesname=lucioperca&id=360
    APR = 1.0     # Tail almost rect, no difference measured for Zingel genra in Fishbase (Z. streber, Z. Zingel)
  )
}
#' Sanitize fish size measurements (deprecated)
#'
#' This function is deprecated. Please use the more focused functions:
#' \code{\link{convert_fork_to_total}} and \code{\link{remove_impossible_lengths}}.
#'
#' @param ... Arguments passed to the new functions
#' @export
sanatize_size_aspe <- function(
  fish_batch = clean_fish_batch(),
  ind_measure = clean_individual_measurement_aspe(),
  species_ref = cleaning_species_ref_aspe(),
  fishbase_length_length = NULL,
  conversion_vector = coefficients_fork2total(),
  convert_intercept_cm2mm = TRUE
) {
  .Deprecated(
    new = "convert_fork_to_total and remove_impossible_lengths",
    package = "yourpackage"
  )
  
  # Run conversion
  converted <- convert_fork_to_total(
    fish_batch = fish_batch,
    ind_measure = ind_measure,
    species_ref = species_ref,
    fishbase_length_length = fishbase_length_length,
    conversion_vector = conversion_vector,
    convert_intercept_cm2mm = convert_intercept_cm2mm
  )
  
  # Run outlier removal
  cleaned <- remove_impossible_lengths(
    ind_measure = converted$ind_measure,
    fish_batch = converted$fish_batch,
    species_ref = species_ref,
    remove_outliers = TRUE
  )
  
  # Return in original format
  list(
    ind_measure = cleaned$ind_measure,
    fish_batch = cleaned$fish_batch,
    fish_length_length = converted$conversion_log,
    over_sized_ind_measure_id = cleaned$outliers$measure_id,
    over_sized_fish_batch_id = cleaned$batch_outliers$batch_id
  )
}
