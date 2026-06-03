#'
#' Convert fish length to weight using species-specific coefficients
#'
#' Converts fish length measurements to weight using length-weight relationship
#' coefficients from FishBase, species conversion mappings, and default fallback
#' coefficients. Returns both the converted data and the coefficient table.
#'
#' @param fish_data A data frame containing fish measurements. Must have columns:
#'   `species_code` and `size_mm` (length in millimeters).
#' @param species_ref A data frame with species reference information. Must have
#'   columns: `species_code`, `latin_name`.
#' @param length_weight_table Optional pre-fetched FishBase length-weight table.
#'   If NULL, fetches from FishBase automatically.
#' @param species_conversion Named vector mapping species codes to conversion
#'   species. Names are original codes, values are codes with known coefficients.
#'   Default uses `species_code_conversion_4_length_weight()`.
#' @param default_coefficients Named vector of default length-weight coefficients
#'   for species without FishBase or conversion data. Default is `c(a = 0.01, b = 3.03)`.
#' @param verbose Logical. If TRUE, prints progress and warning messages.
#'
#' @return A list containing:
#'   \itemize{
#'     \item `fish_data`: Input data frame with added `weight_g` column
#'     \item `coefficients`: Data frame of coefficients used for each species
#'     \item `missing_species`: Species codes that used default coefficients
#'     \item `source_summary`: Count of species by coefficient source
#'   }
#'
#' @details
#' The length-weight relationship used is: \eqn{Weight(g) = a \times (Length(cm))^b}
#'
#' **Priority order for coefficients:**
#' \enumerate{
#'   \item FishBase direct match (averaged Type I linear regressions for TL)
#'   \item Species conversion (using closely related species)
#'   \item Default coefficients (0.01, 3.03 approximating W = 0.01 * L^3.03)
#' }
#'
#' **Note on units:**
#' - Input length must be in **millimeters** (converted to cm internally)
#' - Output weight is in **grams**
#' - FishBase coefficients are applied as-is (expect cm/g relationship)
#'
#' @examples
#' \dontrun{
#' # Prepare data
#' fish_individuals <- data.frame(
#'   species_code = c("PER", "TRF", "UNKNOWN"),
#'   size_mm = c(120, 250, 150)
#' )
#' 
#' species_ref <- data.frame(
#'   species_code = c("PER", "TRF"),
#'   latin_name = c("Perca fluviatilis", "Salmo trutta")
#' )
#' 
#' # Convert
#' result <- convert_length_to_weight(
#'   fish_data = fish_individuals,
#'   species_ref = species_ref
#' )
#' 
#' # View results
#' head(result$fish_data)
#' print(result$coefficients)
#' print(result$source_summary)
#' }
#'
#' @importFrom dplyr filter select group_by summarise left_join bind_rows
#' @importFrom stats setNames
#' @importFrom rfishbase length_weight
#' @export
convert_length_to_weight <- function(
  fish_data,
  species_ref,
  length_weight_table = NULL,
  species_conversion = species_code_conversion_4_length_weight(),
  default_coefficients = c(a = 0.01, b = 3.03),
  verbose = FALSE
) {
  # Input validation ----------------------------------------------------------
  if (!is.data.frame(fish_data)) {
    stop("fish_data must be a data frame", call. = FALSE)
  }
  
  required_cols <- c("species_code", "size_mm")
  missing_cols <- setdiff(required_cols, names(fish_data))
  if (length(missing_cols) > 0) {
    stop("fish_data missing required columns: ", 
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  
  if (!is.data.frame(species_ref)) {
    stop("species_ref must be a data frame", call. = FALSE)
  }
  
  required_ref_cols <- c("species_code", "latin_name")
  missing_ref_cols <- setdiff(required_ref_cols, names(species_ref))
  if (length(missing_ref_cols) > 0) {
    stop("species_ref missing required columns: ", 
         paste(missing_ref_cols, collapse = ", "), call. = FALSE)
  }
  
  # Prepare data --------------------------------------------------------------
  fish_data_orig <- fish_data
  unique_species <- unique(fish_data$species_code)
  
  # Get species with Latin names
  species_with_latin <- species_ref |>
    dplyr::filter(species_code %in% unique_species) |>
    dplyr::select(species_code, latin_name)
  
  if (verbose) {
    message("Processing ", length(unique_species), " unique species...")
  }
  
  # Fetch or use provided length-weight table ---------------------------------
  if (is.null(length_weight_table)) {
    if (verbose) message("Fetching length-weight data from FishBase...")
    
    if (!requireNamespace("rfishbase", quietly = TRUE)) {
      warning("rfishbase not installed. Only conversion and default coefficients will be used.",
              call. = FALSE)
      length_weight_table <- data.frame()
    } else {
      # Get Latin names for species without conversion first (optimization)
      latin_names <- species_with_latin$latin_name
      
      length_weight_table <- tryCatch({
        rfishbase::length_weight(
          species_list = latin_names,
          fields = NULL,
          server = c("fishbase", "sealifebase"),
          version = "latest",
          db = NULL
        )
      }, error = function(e) {
        warning("Failed to fetch FishBase data: ", e$message, call. = FALSE)
        data.frame()
      })
    }
  } else {
    if (!is.data.frame(length_weight_table)) {
      stop("length_weight_table must be NULL or a data frame", call. = FALSE)
    }
    required_ref_cols <- c("Species", "Type", "Number", "Method")
    missing_ref_cols <- setdiff(required_ref_cols, names(length_weight_table))
    if (length(missing_ref_cols) > 0) {
      stop("length_weight_table missing required columns: ",
        paste(missing_ref_cols, collapse = ", "), call. = FALSE)
    }

  }
  
  # Process FishBase coefficients ---------------------------------------------
  fb_coeffs <- length_weight_table |>
    dplyr::filter(!is.na(Number)) |>
    dplyr::filter(stringr::str_detect(Method, "[T|t]ype I linear regression")) |>
    dplyr::filter(Type == "TL") |>
    dplyr::group_by(Species) |>
    dplyr::summarise(
      a = mean(a, na.rm = TRUE),
      b = mean(b, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rename(latin_name = Species) |>
    dplyr::left_join(species_with_latin, by = "latin_name") |>
    dplyr::filter(!is.na(species_code)) |>
    dplyr::select(species_code, a, b) |>
    dplyr::mutate(source = "fishbase")
  
  # Process species conversion coefficients -----------------------------------
  conv_coeffs <- data.frame()
  if (length(species_conversion) > 0) {
    # Create mapping from original to converted species
    conv_map <- data.frame(
      species_code_orig = names(species_conversion),
      species_code_conv = unname(species_conversion),
      stringsAsFactors = FALSE
    )
    
    # Get coefficients for converted species
    conv_coeffs <- conv_map |>
      dplyr::left_join(fb_coeffs, by = c("species_code_conv" = "species_code")) |>
      dplyr::filter(!is.na(a) & !is.na(b)) |>
      dplyr::select(species_code = species_code_orig, a, b) |>
      dplyr::mutate(source = "conversion")
  }
  
  # Combine FishBase and converted coefficients
  all_coeffs <- dplyr::bind_rows(fb_coeffs, conv_coeffs)
  
  # Track missing species
  found_species <- unique(all_coeffs$species_code)
  missing_species <- setdiff(unique_species, found_species)
  
  # Add default coefficients for missing species
  if (length(missing_species) > 0) {
  n_missing_species <- length(missing_species)
    if (verbose) {
      message("Using default coefficients for: ",
        paste(missing_species, collapse = ", "))
    }
    default_coeffs <- data.frame(
      species_code = missing_species,
      a = rep(default_coefficients["a"], times = n_missing_species),
      b = rep(default_coefficients["b"], times = n_missing_species),
      source = "default",
      stringsAsFactors = FALSE
    )
    all_coeffs <- dplyr::bind_rows(all_coeffs, default_coeffs)
  }

  # Vectorized conversion -----------------------------------------------------
  # Create lookup vectors for fast matching
  a_lookup <- stats::setNames(all_coeffs$a, all_coeffs$species_code)
  b_lookup <- stats::setNames(all_coeffs$b, all_coeffs$species_code)
  
  # Convert length from mm to cm
  length_cm <- fish_data$size_mm / 10
  
  # Vectorized weight calculation: W(g) = a * (L_cm)^b
  fish_data$weight_g <- a_lookup[fish_data$species_code] * 
                        (length_cm ^ b_lookup[fish_data$species_code])
  
  # Source summary ------------------------------------------------------------
  source_summary <- all_coeffs |>
    dplyr::group_by(source) |>
    dplyr::summarise(n_species = dplyr::n(), .groups = "drop")
  
  if (verbose) {
    message("\n=== Length-Weight Conversion Summary ===")
    message("Total individuals converted: ", nrow(fish_data))
    message("Unique species processed: ", length(unique_species))
    message("\nCoefficient sources:")
    for (i in seq_len(nrow(source_summary))) {
      message("  - ", source_summary$source[i], ": ", 
              source_summary$n_species[i], " species")
    }
  }
  
  # Return results ------------------------------------------------------------
  list(
    fish_data = fish_data,
    coefficients = all_coeffs,
    missing_species = missing_species,
    source_summary = source_summary
  )
}

#' Species conversion for missing length-weight conversion coefficients in FishBase
#'
#' @keywords internal
#' @noRd
species_code_conversion_4_length_weight <- function() {
  c(
    CHE = "VAN", # Really close shape
    BLN = "VAN", # Separated from VAN recently
    SAT = "TRF", # Really close shape and Salmo genra 
    BLE = "CHA", # Same ecology and shape as Cottus gobio
    VAR = "VAN", # Really close fish, same genra
    PAP = "GAR", # called the Albanian roach
    GDL = "GTN", # Gobies
    GKS = "GTN", # Gobies
    MUP = "MGL", # Mullet
    LPM = "LPR", # Lamprey
    GOO = "GOU", # Sister species of occitan goujon
    CHP = "CHA", # Sister species of Cottus gobio
    APR = "CHA", # Similar shape but less flat
    MUD = "MGL", # Mullet
    LOB = "LOR", # Same genra, similar shape
    GBT = "GTN"  # Gobies
  )
}
