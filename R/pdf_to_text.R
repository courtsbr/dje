#' Converts a PDF file to text (wraps a call to Poppler's `pdftotext` function)
#'
#' @param file Path to file
#' @param new_file Path to file where to save converted text (not used if
#' `return_text = TRUE`)
#' @param start_pg Page where conversion should start (defaults to `NULL`,
#' equivalent to first page)
#' @param end_pg Page where conversion should end (defaults to `NULL`,
#' equivalent to last page)
#' @param raw Whether conversion should use Poppler's `-raw`
#' @param return_text If `TRUE` this function will return the converted
#' text as a character vector, and if `FALSE` (the default) it will
#' return the path to a `.txt` file containing the converted text
#'
#' @return A character vector either with the converted text or with
#' the path to the new file
#'
#' @export
pdf_to_text <- function(file, new_file = NULL, start_pg = NULL,
                     end_pg = NULL, raw = TRUE, return_text = FALSE) {

  # Check if file is all good
  if (!file.exists(file)) { stop(paste("Coudn't find", file)) }
  if (!stringr::str_detect(file, '.pdf')) { stop(paste(file, "isn't a PDF")) }

  # Create name of new file if necessary
  new_file <- ifelse(
    is.null(new_file),
    stringr::str_replace(file, ".pdf$", ".txt"),
    normalizePath(new_file, mustWork = FALSE))

  # Generate command
  command <- stringr::str_c(
    "pdftotext", file, ifelse(raw, "-raw", ""),
    ifelse(!is.null(start_pg), paste("-f", start_pg), ""),
    ifelse(!is.null(end_pg), paste("-l", end_pg), ""),
    new_file, sep = " ")

  # Run command
  res <- system(command, intern = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)

  # Remove file if necessary
  if (return_text) {
    out = readr::read_file(new_file)
    file.remove(new_file)
  } else {
    out <- normalizePath(new_file)
  }

  return(out)
}

#' Converts a directory of DJE PDFs to text (using [pdf_to_text()])
#'
#' @param path Path to directory containing files to be converted
#' @param new_path Path to directory where to save converted files (defaults
#' to `NULL`, simply appending "_txt" to `path`)
#' @param ... Parameters passed on to [pdf_to_text()]
#'
#' @return A character vector with the paths the new files
#'
#' @seealso [pdf_to_text()]
#'
#' @export
dje_to_text <- function(path = ".", new_path = NULL, ...){

  # Check if directory is all good
  if (!dir.exists(path)) { stop(paste("Coudn't find", path)) }
  if (length(list.files(path)) == 0) { stop(paste(path, "is empty")) }

  # Create name of new directory if necessary
  new_path <- ifelse(
    is.null(new_path),
    stringr::str_c(normalizePath(path), "_txt"),
    normalizePath(new_path, mustWork = FALSE))

  # Gather files to convert and new files to create
  files <- path %>%
    dir(recursive = TRUE, full.names = TRUE) %>%
    normalizePath()
  new_files <- files %>%
    stringr::str_replace(normalizePath(path), new_path) %>%
    stringr::str_replace(".pdf$", ".txt")

  # Create new directory mirroring old one
  purrr::walk(dirname(new_files), dir.create,
              recursive = TRUE, showWarnings = FALSE)

  # Parallel mcmapply
  out <- parallel::mcmapply(pdf_to_text, files, new_files, ...)

  return(out)
}
