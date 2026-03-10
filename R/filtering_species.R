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

species_replacement_list <- function() {
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
    "LPX" = "LPP", #"Lamproie"<-"Lamproie_de_planer" n=1047
    "TRL" = "TRF", #"Truite_de_lac"<-"Truite_de_riviere"
    "TRM" = "TRF"  #"Truite_de_mer"<-"Truite_de_riviere"
  )
}
