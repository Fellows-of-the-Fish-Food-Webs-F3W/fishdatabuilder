#' Validate fish species codes against reference and FishBase
#'
#' Performs two-level validation of fish species codes: first against the ASPE
#' species reference table, then against FishBase to verify taxonomic validity.
#'
#' @param species A character vector of species codes to validate (three-letter
#'   uppercase codes). Default example shows common codes "ABL" and "TRF".
#' @param species_ref A data frame containing species reference data with at least
#'   columns `species_code` and `latin_name`. By default uses 
#'   `cleaning_species_ref_aspe()`.
#'
#' @return Invisibly returns a list with validation results:
#' \itemize{
#'   \item `valid_codes`: Species codes that passed both checks
#'   \item `missing_in_ref`: Species codes not found in reference table
#'   \item `missing_in_fishbase`: Species codes with Latin names not validated in FishBase
#' }
#' The function is primarily called for its side effects (warnings).
#'
#' @details
#' The validation process occurs in two steps:
#' 
#' **Step 1: Reference table check**
#' Verifies that all provided species codes exist in the ASPE species reference
#' table. Missing codes trigger a warning with the problematic codes listed.
#'
#' **Step 2: FishBase validation**
#' For codes that pass step 1, retrieves their Latin names from the reference
#' table and validates them using `rfishbase::validate_names()`. This checks:
#' \itemize{
#'   \item Correct spelling of scientific names
#'   \item Taxonomic validity in FishBase
#'   \item Current accepted names (including synonym resolution)
#' }
#'
#' @note
#' - Requires an internet connection for FishBase validation
#' - FishBase validation can be slow for many species; consider caching results
#' - The function returns invisibly to allow programmatic use while primarily
#'   serving as a diagnostic tool
#'
#' @examples
#' \dontrun{
#' # Check specific species
#' check_aspe_fish_species(c("ABL", "TRF", "ANG"))
#' 
#' # Get all species from your data
#' all_species <- unique(fish_data$species_code)
#' check_aspe_fish_species(all_species)
#' 
#' # Capture validation results
#' results <- check_aspe_fish_species(c("ABL", "INVALID", "TRF"))
#' results$missing_in_ref  # Shows "INVALID"
#' }
#'
#' @importFrom dplyr filter pull
#' @importFrom rfishbase validate_names
#' @seealso
#' - [cleaning_species_ref_aspe()] for species reference data
#' - [rfishbase::validate_names()] for FishBase name validation
#' - [sanitize_species_code()] for cleaning species codes
#' @export
check_aspe_fish_species <- function(
  species = c("ABL", "TRF"),
  species_ref = cleaning_species_ref_aspe()
){
  # Input validation
  if (!is.character(species)) {
    stop("`species` must be a character vector", call. = FALSE)
  }
  
  if (!is.data.frame(species_ref)) {
    stop("`species_ref` must be a data frame", call. = FALSE)
  }
  
  required_ref_cols <- c("species_code", "latin_name")
  missing_ref_cols <- setdiff(required_ref_cols, names(species_ref))
  if (length(missing_ref_cols) > 0) {
    stop("`species_ref` missing required columns: ", 
         paste(missing_ref_cols, collapse = ", "), call. = FALSE)
  }

  species <- unique(species)
  
  # Initialize results list
  results <- list(
    valid_codes = character(),
    missing_in_ref = character(),
    missing_in_fishbase = character()
  )
  
  # Check all provided code are in ref:
  species_mask_ref <- species %in% species_ref$species_code
  results$missing_in_ref <- species[!species_mask_ref]
  
  if (!all(species_mask_ref)) {
    warning("Missing species in reference: ",
            paste(results$missing_in_ref, collapse = ", "),
            ". Please check provided species names.",
            call. = FALSE)
  }

  # Check species against FishBase
  species_valid <- species[species_mask_ref]
  if (length(species_valid) > 0) {
    species_ref_filtered <- species_ref |>
      dplyr::filter(species_code %in% species_valid)
    latin_names <- dplyr::pull(species_ref_filtered, latin_name)

    # Check if rfishbase is available
    if (!requireNamespace("rfishbase", quietly = TRUE)) {
      warning("Package 'rfishbase' is not installed. Skipping FishBase validation.",
              call. = FALSE)
      results$valid_codes <- species_valid
    } else {
      latin_names_valid <- tryCatch({
        rfishbase::validate_names(latin_names)
      }, error = function(e) {
        warning("FishBase validation failed: ", e$message, call. = FALSE)
        rep(NA, length(latin_names))
      })

      missing_fb <- is.na(latin_names_valid)
      results$missing_in_fishbase <- species_ref_filtered$species_code[missing_fb]
      results$valid_codes <- species_ref_filtered$species_code[!missing_fb]

      if (any(missing_fb)) {
        warning(
          "Missing species in FishBase: ",
          paste(latin_names[missing_fb], collapse = ", "),
          ", corresponding to ",
          paste(results$missing_in_fishbase, collapse = ", "),
          ". Please check reference table or the provided species.",
          call. = FALSE
        )
      }
    }
  } else {
    results$valid_codes <- character(0)
  }

  invisible(results)
}

#' Clean and standardize species codes in fish batch data
#'
#' Applies species code cleaning operations to fish batch data by removing
#' unwanted species codes and replacing undetermined/hybrid codes with
#' standardized ones.
#'
#' @param data A data frame containing fish batch data. By default uses 
#'   `get_fish_batch_aspe()`. Must contain the species code variable.
#' @param species_var The unquoted name of the column containing species codes.
#'   Default is `species_code`.
#' @param species_to_remove A character vector of species codes to filter out.
#'   By default uses `species_code_to_remove()`.
#' @param species_to_replace A named character vector mapping original codes
#'   to replacement codes. By default uses `species_code_to_replace()`.
#' @param species_ref A data frame containing species reference data.
#'   By default uses `cleaning_species_ref_aspe()`. (Currently unused but
#'   maintained for future validation.)
#'
#' @return A data frame with the same structure as input, but with:
#' \itemize{
#'   \item Rows containing species codes in `species_to_remove` removed
#'   \item Species codes in `species_var` replaced according to `species_to_replace`
#' }
#'
#' @details
#' The function performs two main operations:
#' \enumerate{
#'   \item **Filtering**: Removes all rows where the species code matches any
#'         code in `species_to_remove` (e.g., crayfish, undetermined codes)
#'   \item **Recoding**: Replaces species codes using the mapping provided in
#'         `species_to_replace` (e.g., "BBX" → "BBG", "LP?" → "LPP")
#' }
#'
#' The operations are performed in this order to ensure that removed species
#' are not inadvertently recoded first.
#'
#' @note
#' The function uses tidy evaluation ({{ }}) for flexible column specification,
#' making it work with different column names across datasets.
#'
#' @examples
#' \dontrun{
#' # Basic usage with defaults
#' cleaned_fish <- sanitize_species_code()
#'
#' # Custom usage with different column name
#' cleaned_fish <- sanitize_species_code(
#'   data = my_fish_data,
#'   species_var = spp_code
#' )
#'
#' # Custom removal and replacement lists
#' cleaned_fish <- sanitize_species_code(
#'   species_to_remove = c("XXX", "YYY"),
#'   species_to_replace = c("BAD" = "GOOD")
#' )
#' }
#'
#' @importFrom dplyr filter mutate
#' @importFrom stringr str_replace_all
#' @seealso
#' - [species_code_to_remove()] for default removal codes
#' - [species_code_to_replace()] for default replacement mapping
#' - [get_fish_batch_aspe()] for raw fish batch data
#' - [cleaning_species_ref_aspe()] for species reference data
#' @export
sanitize_species_code <- function(
  data = get_fish_batch_aspe(),
  species_var = species_code,
  species_to_remove = species_code_to_remove(),
  species_to_replace = species_code_to_replace(),
  species_ref = cleaning_species_ref_aspe()
){
  # Input validation
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame", call. = FALSE)
  }
  
  # Check if species_var exists in data
  species_col <- rlang::as_name(rlang::enquo(species_var))
  if (!species_col %in% names(data)) {
    stop("Column '", species_col, "' not found in data", call. = FALSE)
  }
  
  if (!is.character(species_to_remove) && !is.logical(species_to_remove)) {
    stop("`species_to_remove` must be a character or logical vector", call. = FALSE)
  }
 
  if (!is.character(species_to_replace) || is.null(names(species_to_replace))) {
    stop("`species_to_replace` must be a named character vector", call. = FALSE)
  }

  data <- data |>
    dplyr::filter(!{{species_var}} %in% species_to_remove) |>
    dplyr::mutate({{species_var}} := stringr::str_replace_all(
      {{species_var}},
      species_to_replace
    ))
  
  # Optional: Check if any replacements introduced NA or unexpected values
  if (any(is.na(data[[species_col]]))) {
    warning("NA values introduced in species codes after replacement", call. = FALSE)
  }
 
  data
}

#'
#' Replacement vector to fix species latin name
#'
#'
#' @return named vector
#' @export
species_latin_name_to_replace <- function() {
  c(
    Atherinapresbyter = "Atherina presbyter",
    "Gymnocephalus cernuus" = "Gymnocephalus cernua" # accepted name in fishbase
  )
}

#' List of species codes to remove from ASPE sampling data
#'
#' Generates a vector of species codes (three-letter uppercase codes) that can be 
#' filtered out from datasets, including non-fish species (primarily crayfish) 
#' and undetermined individuals that cannot be reliably assigned to a species.
#'
#' @param not_fish Logical. If `TRUE` (default), includes codes for non-fish species
#'   such as crayfish and crabs that are captured during fish sampling but should
#'   typically be excluded from fish community analyses.
#' @param not_determined Logical. If `TRUE` (default), includes codes for 
#'   undetermined individuals that lack information for a species-level identification.
#'
#' @param additional_codes Character vector. Additional species codes to be added the output
#'
#' @return A character vector of three-letter uppercase species codes to be 
#'   excluded from analyses. Returns an empty vector if both parameters are `FALSE`.
#'
#' @details
#' The function categorizes species codes into two groups:
#' 
#' **Non-fish species** (included when `not_fish = TRUE`):
#' \describe{
#'   \item{ASL}{Crayfish (unspecified)}
#'   \item{CRC}{Chinese crab}
#'   \item{ECR}{Undetermined crayfish}
#'   \item{OCL}{American crayfish}
#'   \item{PCC}{Louisiana crayfish}
#'   \item{OCI}{Crayfish "*Orconectes immunis*"}
#'   \item{PCV}{Crayfish "*Procambarus virginalis*"}
#'   \item{PFL}{Signal crayfish}
#' }
#' 
#' **Undetermined species** (included when `not_determined = TRUE`):
#' \describe{
#'   \item{COR}{Undetermined *Coregonus* (lake whitefish genus)}
#'   \item{SAL}{Undetermined Salmonidae (salmonid family)}
#' }
#'
#' @note
#' - These codes correspond to the ASPE database species coding system
#' - The function is designed to be used with [dplyr::filter()] or similar
#'   filtering operations to exclude unwanted species from analyses
#' - Future versions may include anadromous/catadromous species options
#'
#' @examples
#' \dontrun{
#' # Get all species to remove (default)
#' remove_codes <- species_code_to_remove()
#' 
#' # Remove only non-fish species, keep undetermined
#' remove_codes <- species_code_to_remove(not_determined = FALSE)
#' 
#' # Use in filtering operations
#' clean_fish_data <- fish_data %>%
#'   dplyr::filter(!species_code %in% species_code_to_remove())
#'   
#' # Keep all species (empty removal list)
#' keep_all <- species_code_to_remove(not_fish = FALSE, not_determined = FALSE)
#' }
#'
#' @seealso
#' - [clean_fish_batch()] for fish batch data cleaning
#' - [clean_individual_measurement_aspe()] for individual fish measurements
#' 
#' @return A character vector of species codes to remove
#' @export
species_code_to_remove <- function(
  not_fish = TRUE,
  not_determined = TRUE,
  additional_codes = NULL
) {
  # Input validation
  if (!is.logical(not_fish) || length(not_fish) != 1) {
    stop("`not_fish` must be a single logical value", call. = FALSE)
  }
  if (!is.logical(not_determined) || length(not_determined) != 1) {
    stop("`not_determined` must be a single logical value", call. = FALSE)
  }
  if (!is.null(additional_codes) & !all(is.character(additional_codes))) {
    stop("`additional_codes` must be a character vector", call. = FALSE)
  }

  output <- character(0)

  no_fish <- c(
    "ASL", # Crayfish,
    "CRC", # Chinese crab
    "ECR", # Indetermined crayfish
    "OCL", # American crayfish
    "PCC", # Louisiana crayfish
    "OCI", # Crayfish "Orconectes immunis"
    "PCV", # Crayfish "Procambarus virginalis"
    "PFL" # Signal crayfish Ecrevisse signal
  )

  undetermined <- c(
    "COR", # Not determined Coregonus, lake Geneva, other species not present in the dataset,
    "SAL" # undetermined Salmonidae, occurrence = 2
  )
  # TODO: add anadromous/catadromous species: i.e. species just migrating and that are not participating to the food-web,
  # But check because some small fishes can decide to not go to the sea

  if (not_fish) {
    output <- append(output, no_fish)
  }
  if (not_determined) {
    output <- append(output, undetermined)
  }
  if (!is.null(additional_codes)) {
    output <- append(output, additional_codes)
  }

  output
}

#' Get mapping of undetermined species codes to valid species codes
#'
#' Provides a named vector mapping undetermined, hybrid, or misclassified species codes
#' to their most likely valid species codes based on occurrence frequency, biological
#' plausibility, and expert knowledge. This mapping is designed to standardize species
#' identification in ASPE database records.
#'
#' @details
#' The function returns a named character vector where:
#' * **Names** are the original codes found in the database (undetermined, hybrid, 
#'   or alternative classifications)
#' * **Values** are the standardized species codes to replace them with
#'
#' The mapping decisions are based on several criteria:
#' \itemize{
#'   \item **Occurrence frequency**: When multiple species are possible, the most
#'         frequently occurring species in the dataset is selected
#'   \item **Biological plausibility**: Closest related species based on morphology
#'         or ecology
#'   \item **Expert knowledge**: Species-specific decisions informed by fisheries
#'         biologists
#'   \item **Monotypic genera**: When only one species exists in the genus for the
#'         study area
#' }
#'
#' @section Mapping Categories:
#' 
#' **Undetermined genus/species** (codes ending with X):
#' \describe{
#'   \item{BBX → BBG}{Undetermined black bass to *Micropterus salmoides* (closest in maximum length)}
#'   \item{BLX → BRB}{Undetermined *Blicca* to *Blicca bjoerkna* (only species in genus)}
#'   \item{BRX → BRE}{Undetermined *Abramis* to *Abramis brama* (only species in genus)}
#'   \item{ALX → ALA}{Undetermined *Alosa* to *Alosa alosa* (higher occurrence)}
#'   \item{BAX → BAF}{Undetermined *Barbus* to *Barbus barbus* (10× higher occurrence)}
#'   \item{CAX → CAS}{Undetermined *Carassius* to *Carassius carassius* (highest occurrence)}
#'   \item{GOX → GOU}{Undetermined *Gobio* to *Gobio gobio* (100× higher occurrence)}
#'   \item{PHX → VAI}{Undetermined *Phoxinus* to *Phoxinus phoxinus* (5000× higher occurrence)}
#'   \item{LOX → LOF}{Undetermined *Barbatula* to *Barbatula barbatula* (only species in genus)}
#' }
#'
#' **Undetermined species with question marks** (uncertain identification):
#' \describe{
#'   \item{LP? → LPP}{Undetermined lamprey to *Lampetra planeri* (100× higher occurrence)}
#'   \item{MU? → MUP}{Undetermined mullet to *Liza ramada* (10× higher occurrence)}
#' }
#'
#' **Petromyzontidae (lamprey) variants:**
#' \describe{
#'   \item{LPX → LPP}{Undetermined lamprey to *Lampetra planeri* (100× higher occurrence)}
#' }
#'
#' **Cyprinid hybrids and variants:**
#' \describe{
#'   \item{HBG → GAR}{Hybrid bream/roach to roach (*Rutilus rutilus*)}
#'   \item{BRG → BRE}{Hybrid bream-roach to bream (*Abramis brama*) (n=4)}
#'   \item{HYC → GAR}{Undetermined cyprinid hybrid to roach (n=20)}
#'   \item{CYP → GAR}{Juvenile cyprinid to roach}
#' }
#'
#' **Carassius (crucian carp) variants:**
#' \describe{
#'   \item{CAG → CAS}{Silver crucian carp to crucian carp (*Carassius carassius*)}
#'   \item{CAA → CAS}{Golden or silver crucian carp to crucian carp}
#'   \item{CAR → CAS}{Silver carp to crucian carp}
#'   \item{CAD → CAS}{Golden crucian carp to crucian carp}
#' }
#'
#' **Carp (Cyprinus) variants:**
#' \describe{
#'   \item{CMI → CCO}{Mirror carp to common carp (*Cyprinus carpio*)}
#'   \item{CCU → CCO}{Leather carp to common carp}
#' }
#'
#' **Trout (Salmo) variants:**
#' \describe{
#'   \item{TRL → TRF}{Lake trout to river trout (*Salmo trutta fario*)}
#'   \item{TRM → TRF}{Sea trout to river trout}
#' }
#'
#' **Other corrections:**
#' \describe{
#'   \item{RUB → GAR}{Italian roach to roach (n=1)}
#' }
#'
#' @return A named character vector where:
#' \itemize{
#'   \item Names are the original species codes to be replaced
#'   \item Values are the standardized species codes to use instead
#' }
#'
#' @note
#' - The function is designed to be used with [dplyr::recode()] or similar
#'   recoding functions
#' - Mapping decisions are based on ASPE database occurrences and expert knowledge
#' - These replacements help standardize datasets for analysis while maintaining
#'   biological meaningfulness
#' - Some codes (e.g., BBX, BLX) represent undetermined individuals at genus level
#'
#' @examples
#' \dontrun{
#' # Get the replacement mapping
#' replacement_map <- species_code_to_replace()
#' 
#' # Use in data cleaning
#' clean_species_data <- fish_data %>%
#'   dplyr::mutate(
#'     species_code = dplyr::recode(species_code, !!!species_code_to_replace())
#'   )
#' 
#' # Check which codes will be replaced
#' original_codes <- names(species_code_to_replace())
#' replacement_codes <- species_code_to_replace()
#' }
#'
#' @seealso
#' - [species_code_to_remove()] for codes to exclude entirely
#' - [clean_fish_batch()] for fish batch data cleaning
#' - [clean_individual_measurement_aspe()] for individual fish measurements
#'
#' @return A named character vector mapping original codes to replacement codes
#' @export
species_code_to_replace <- function() {
  c(
    "BBX" = "BBG", # Undetermined black bass to B. salmonides (closest in maximum length in ASPE species table)
    "BLX" = "BRB", # The only Blicca species in the ASPE species table
    "BRX" = "BRE", # The only Abramis in the ASPE species table
    "ALX" = "ALA", # Undetermined Alosa to A. alosa, slightly highest occurrence Alosa
    "BAX" = "BAF", # Undetermined Barbus sp. to B. barbus, occurrence 10 times higher than BAM
    "CAX" = "CAS", # Undetermined Carassius sp. to C. carassius, highest occurrence
    "GOX" = "GOU", # Undetermined  Gobio sp. to G. gobio, 100 times higher occurrence than the other
    "HBG" = "GAR", # Hybrid bream / roach
    "PHX" = "VAI", # Undetermined Phoxinus sp. to common Phoxinus, occurrence 5000 times higher than the other
    "LOX" = "LOF", # Undetermined Barbatula sp. to B. barbatula, the only species of the Barbatula genus in the dataset
    "LP\\?" = "LPP", # Undetermined Petromyzontidae to Lampetra planeri, occurrence 100 times higher than the other lamprey species in the dataset
    "LPX" = "LPP", # Undetermined Petromyzontidae to Lampetra planeri, occurrence 100 times higher than the other lamprey species in the dataset
    "MU\\?" = "MUP", # Undetermined Mugilidae to Liza ramada, occurrence 10 times higher than the other mullet in the dataset
    "CMI" = "CCO", # Mirror carp to common carp
    "CCU" = "CCO", # Leather carp to common carp
    "RUB" = "GAR", # Italian roach to roach (n=1)
    "CYP" = "GAR", # Juvenile cyprinid to roach
    "CAG" = "CAS", # Silver crucian carp to crucian carp
    "CAA" = "CAS", # Golden or silver crucian carp to crucian carp
    "CAR" = "CAS", # Silver carp to crucian carp
    "CAD" = "CAS", # Golden crucian carp to crucian carp
    "BRG" = "BRE", # Hybrid bream-roach to bream (n=4)
    "HYC" = "GAR", # Undetermined cyprinid hybrid to roach (n=20)
    "TRL" = "TRF", # Lake trout to river trout
    "TRM" = "TRF"  # Sea trout to river trout
  )
}
