#' Converts a pdf file to text
#'
#' Wraps a call to poppler's pdftotext function.
#'
#'@param a pdf file to convert
#'@param first_pg page where the convertion should start. defaults to NA, equivalent to 1.
#'@param last_pg page where the convertion should end. defaults to NA, equivalent to the last page.
#'@param r should the convertiong uses poppler's -raw?
#'@param keep_file should we keep the file after convertion?
#'@param return_string should this functions return a string?
#'@param new_file name of the output file
#'
#' @export
pdf2text <- function(a, first_pg = NA, last_pg = NA, r = F, keep_file = T,
                     return_string = F, new_file = 'repo.txt'){

  if(!file.exists(a))stop(sprintf("Coudn't find %s.", a))

  if(stringi::stri_detect(a,fixed = '.pdf')){
    sprintf('pdftotext %s %s%s%s%s',
            a,
            ifelse(r,'-raw ',' '),
            ifelse(!is.na(first_pg),paste('-f',first_pg,''),' '),
            ifelse(!is.na(last_pg),paste('-l',last_pg,''),' '),
            new_file) %>%
      system()
    if(!keep_file){file.remove(new_file)}
    if(return_string){
      texto = readr::read_file(new_file)
      return(texto)
    }
  } else {
    stop(sprintf("%s should be a pdf file.", a))
  }
}

#' Converts a list of dje pdf files to text
#'
#'@param files list of files to convert. defaults to NULL, when path should be used.
#'@param path directory containing pdf files (and only them) to convert. works with folders.
#'@param ex_dir folder to save the files
#'@param ... other param passed to pdf2text
#'
#'@export
dje2text <- function(files = NULL, path = NULL, ex_dir = 'dje_txt/', ...){

  #troubleshooting
  if(is.null(path)){
    if(is.null(a))stop("files and path are null.")

    if(any(!file.exists(a))){
      stop("All files should exist.")
    }
    files2conv <- a
  } else {
    if(!is.null(files)) cat("Using path over files.")
    if(!dir.exists(path)) stop("path does not exists.")

    files2conv <- list.files(path, recursive = T)
    files2conv_fn <- list.files(path, recursive = T, full.names = T)

    if(length(files2conv) == 0) stop("path is empty.")
  }

  suppressWarnings(dir.create(sprintf("%s%s", ex_dir, dirname(files2conv))))

  files2conv %>%
    stringr::str_replace_all("\\.pdf$","\\.txt") %>%
    stringr::str_c(ex_dir, .) %>%
    purrr::walk2(files2conv_fn, ~pdf2text(a = .y, new_file = .x, ...))
}
