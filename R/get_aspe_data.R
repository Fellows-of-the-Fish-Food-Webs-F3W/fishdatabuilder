################################################################################
#                          Download ASPE from Zenodo                           #
# Structure taken from the pkgfilecache's vignette:
# https://cran.r-project.org/web/packages/pkgfilecache/vignettes/pkgfilecache.html
#
################################################################################

#' @title Download optional data for the package
#'
#' @description Ensure that the optional data is available locally in the
#' package cache. Will try to download the data only if it is not available.
#'
#' @return Named list. The list has entries: "available": vector of strings. The
#' names of the files that are available in the local file cache. You can access
#' them using get_optional_data_file(). "missing": vector of strings. The names
#' of the files that this function was unable to retrieve.
#'
#' @export
download_optional_data <- function() {
  pkg_info <- pkgfilecache::get_pkg_info("fishdatabuilder")

  # How the files should be called in the local package file cache
  local_filenames <- c("zenodo_aspe_csv.zip")

  # Remote URLs where to download files from
  urls <- c("https://zenodo.org/records/8099409/files/2023_06_22_aspe_csv.zip?download=1")
  # MD5 checksums. Optional but recommended
  md5sums <- c("b025601f66baeac85ca41ed09fad3bc0")

  # Download the files in the package cache
  cfiles <- pkgfilecache::ensure_files_available(
    pkg_info,
    local_filenames,
    urls,
    md5sums = md5sums
  )
  cfiles$file_status <- NULL
  return(cfiles)
}


#' @title Extract optional data for the package
#'
#' @description Extract
#' package cache. Will try to download the data only if it is not available.
#'
#' @return logical vector indicating if the files have been successfully moved
#' in the main folder of the package cache
#'
#' @export
extract_optional_data <- function() {
  # Get info about the zip file
  pkg_info <- pkgfilecache::get_pkg_info("fishdatabuilder")
  local_filenames <- c("zenodo_aspe_csv.zip")

  # Get the path of the zip file in the package cache
  file_path <- pkgfilecache::get_filepath(pkg_info,
    local_filenames,
    mustWork = TRUE)

  # Extract and remove the zip file
  unzip(file_path, exdir = dirname(file_path))
  file.remove(file_path)

  # Move the files from the csv/ folder to the parent folder
  current_file_names <- list.files(
    paste0(dirname(file_path), "/csv/"),
    full.names = TRUE)
  new_file_names <- paste0(
    dirname(file_path), "//",
    list.files(paste0(dirname(file_path), "/csv/"))
  )
  file.rename(
    from = current_file_names,
    to = new_file_names
  )
  # Remove the csv folder
  unlink(
    paste0(pkgfilecache::get_cache_dir(pkg_info), "/csv"),
    recursive = TRUE
  )
}


#' @title Get file names available in package cache
#'
#' @description Get file names of optional data files which are available in the
#' local package cache. You can access these files with
#' get_optional_data_file().
#'
#' @return vector of strings. The file names available, relative to the package
#' cache.
#'
#' @export
list_optional_data <- function() {
  pkg_info <- pkgfilecache::get_pkg_info("fishdatabuilder")
  return(pkgfilecache::list_available(pkg_info))
}


#' @title Access a single file from the package cache by its file name.
#'
#' @param filename, string. The filename of the file in the package cache.
#'
#' @param mustWork, logical. Whether an error should be created if the file does
#' not exist. If mustWork=FALSE and the file does not exist, the empty string is
#' returned.
#'
#' @return string. The full path to the file in the package cache. Use this in
#' your application code to open the file.
#'
#' @export
get_optional_data_filepath <- function(filename, mustWork = TRUE) {
  pkg_info <- pkgfilecache::get_pkg_info("fishdatabuilder")
  return(pkgfilecache::get_filepath(pkg_info, filename, mustWork = mustWork))
}


#' @title Delete all data in the package cache.
#'
#' @return integer. The return value of the unlink() call: 0 for success, 1 for
#' failure. See the unlink() documentation for details.
#'
#' @export
delete_all_optional_data <- function() {
  pkg_info <- pkgfilecache::get_pkg_info("fishdatabuilder")
  return(pkgfilecache::erase_file_cache(pkg_info))
}

