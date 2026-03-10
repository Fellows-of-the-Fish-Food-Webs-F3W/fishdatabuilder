#' Replacement vector to fix species latin name
#'
#'
#' @return named vector
#' @export
species_latin_name_to_replace <- function() {
  c(
    Atherinapresbyter = "Atherina presbyter"
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
#' remove_codes <- species_to_remove()
#' 
#' # Remove only non-fish species, keep undetermined
#' remove_codes <- species_to_remove(not_determined = FALSE)
#' 
#' # Use in filtering operations
#' clean_fish_data <- fish_data %>%
#'   dplyr::filter(!species_code %in% species_to_remove())
#'   
#' # Keep all species (empty removal list)
#' keep_all <- species_to_remove(not_fish = FALSE, not_determined = FALSE)
#' }
#'
#' @seealso
#' - [clean_fish_batch()] for fish batch data cleaning
#' - [clean_individual_measurement_aspe()] for individual fish measurements
#' 
#' @return A character vector of species codes to remove
#' @export
species_to_remove <- function(
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
#' - [species_to_remove()] for codes to exclude entirely
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
    "LP?" = "LPP", # Undetermined Petromyzontidae to Lampetra planeri, occurrence 100 times higher than the other lamprey species in the dataset
    "LPX" = "LPP", # Undetermined Petromyzontidae to Lampetra planeri, occurrence 100 times higher than the other lamprey species in the dataset
    "MU?" = "MUP", # Undetermined Mugilidae to Liza ramada, occurrence 10 times higher than the other mullet in the dataset
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
species_code_to_replace <- function() {
  c(
    "BBX" = "BBG", #Indetermined black bass to B. salmonides (closest in maximum length in ASPE species table)
    "BLX" = "BRB", # The only Blicca species in the ASPE species table
    "BRX" = "BRE", # The only Abramis in the ASPE species table
    "ALX" = "ALA", # Indetermined Alosa to A. alosa, slighly highest occurence Alosa
    "BAX" = "BAF", # Indetermined Barbus sp. to B. barbus, occurence 10 times higher than BAM
    "CAX" = "CAS", # Indetermined Carassius sp. to C. carassius, highest occurence
    "GOX" = "GOU", # Indetermined  Gobio sp. to G. gobio, 100 times higher occurence than the other
    "HBG" = "GAR", #  Hybrid  Breme / Gardon
    "PHX" = "VAI", # Undetermined Phoxinus sp. to common Phoxinus, occurence 5000 times higher than the other
    "LOX" = "LOF", # Undetermined Barbatula sp. to B. barbatula, the only species of the Barbatula genra in the dataset
    "LP?" = "LPP", # Undetermined Petromyzontidae to Lampetra planeri, occurence 100 times higher than the other Lemprey species in the dataset
    "LPX" = "LPP", # Undetermined Petromyzontidae to Lampetra planeri, occurence 100 times higher than the other Lemprey species in the dataset
    "MU?" = "MUP", # Undetermined Mugilidae to Liza ramada, occurence 10 times higher than the other Mullet in the dataset
    "CMI" = "CCO", #"Carpe_miroir"<-"Carpe_commune"
    "CCU" = "CCO", #"Carpe_cuir"<-"Carpe_commune"
    "RUB" = "GAR", #"Gardon_italien"<-"Gardon" n=1
    "CYP" = "GAR", #"Juvenile_cyp"<-"Gardon"
    "CAG" = "CAS", #"Carassin_argente"<-"Carassin"
    "CAA" = "CAS", #Carassin_dore_ou_argente"<-"Carassin"
    "CAR" = "CAS", #"Carpe_argentee"<-"Carassin"
    "CAD" = "CAS", #"Carassin_dore"<-"Carassin"
    "BRG" = "BRE", #"Hybride_breme-gardon"<-"Breme" n=4
    "HYC" = "GAR", #"Hybrides_de_cyprinides"<-"Gardon"n=20
    "TRL" = "TRF", #"Truite_de_lac"<-"Truite_de_riviere"
    "TRM" = "TRF"  #"Truite_de_mer"<-"Truite_de_riviere"
  )
}
