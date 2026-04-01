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
