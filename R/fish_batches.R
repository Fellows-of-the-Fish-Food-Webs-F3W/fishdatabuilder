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
