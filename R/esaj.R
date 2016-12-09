#' Baixa diarios oficiais
#'
#' Acessa os Diarios de Justica Eletronicos dos Tribunais de Justiça e baixa
#' os arquivos em PDF.
#'
#' @param tj character vector indicando o Tribunal. Atualmente funciona com
#' TJSP, TJAC, TJAL, TJAM, TJMS, TJRN, TJSC, TJCE, TJBA. Default \code{'TJSP'}.
#' @param dates Date vector ou character vector em YYYY-MM-DD com as datas
#' que se deseja baixar. Default \code{Sys.Date()}.
#' @param path pasta onde os arquivos serao gravados. Para cada data, uma pasta
#' sera criada e os arquivos PDF serao salvos nessa pasta. Default \code{'data-raw/dje_pdf'}
#' @param verbose imprimir mensagens? Default \code{FALSE}.
#'
#' @return \code{tibble} com diagnostico dos resultados.
#'
#' @examples
#' \dontrun{
#' dir.create('data-raw/dje_pdf', recursive = TRUE, showWarnings = FALSE)
#' tjsp_dje <- dje(dates = Sys.Date() - 0:3)
#' table(tjsp_dje$result)
#'
#' # --------------------------------------------------------------------------
#' tjal_dje <- dje(tj = 'TJAL', dates = Sys.Date() - 0:3)
#' tjam_dje <- dje(tj = 'TJAM', dates = Sys.Date() - 0:3)
#' tjce_dje <- dje(tj = 'TJCE', dates = Sys.Date() - 0:3)
#' tjba_dje <- dje(tj = 'TJBA', dates = Sys.Date() - 0:3)
#' tjms_dje <- dje(tj = 'TJMS', dates = Sys.Date() - 0:3)
#' tjsc_dje <- dje(tj = 'TJSC', dates = Sys.Date() - 0:3)
#' tjrn_dje <- dje(tj = 'TJRN', dates = Sys.Date() - 0:3)
#' tjac_dje <- dje(tj = 'TJAC', dates = Sys.Date() - 0:3)
#' }
#' @export
dje <- function(tj = 'TJSP', dates = Sys.Date(), path = 'data-raw/dje_pdf',
                verbose = FALSE) {
  f <- sprintf('dje_%s', tolower(tj))
  eval(call(f, dates, path, verbose))
}

#' @rdname dje
#'
#' @param from,to Date vector ou character vector em formato YYYY-MM-DD.
#' @inheritParams dje
#'
#' @export
dje_range <- function(from, to, tj = 'TJSP', path = 'data-raw/dje_pdf',
                      verbose = FALSE) {
  dates <- seq(as.Date(from), as.Date(to), by = 1)
  dje(tj, dates, path, verbose)
}

dje_tjsp <- function(dates, path, verbose = FALSE) {
  u <- 'http://www.dje.tjsp.jus.br/cdje/downloadCaderno.do?'
  pastas <- sprintf('%s/tjsp_dje_%s', path, sort(dates))
  invisible(sapply(pastas, dir.create, showWarnings = FALSE, recursive = TRUE))
  f <- dplyr::failwith(tibble::tibble(result = 'erro'), download_arq)
  d <- expand.grid(date = dates, caderno = as.character(c(11:15, 18)),
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
    tibble::tibble() %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(date_link = format(as.Date(date), '%d/%m/%Y'),
                  link = sprintf('%sdtDiario=%s&cdCaderno=%s', u, date_link, caderno),
                  arq = sprintf('%s/tjsp_dje_%s_%s.pdf', rep(pastas, each = 6), caderno, date)) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::group_by(date, caderno, date_link, link, arq) %>%
    dplyr::do(f(.$link, .$arq, verbose)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, caderno, link, arq, result)
  return(d)
}

download_arq <- function(u, a, verbose = FALSE) {
  if (file.exists(a)) {
    if (verbose) cat('\narquivo ',  a, ' ja existe!\n')
    return(tibble::tibble(result = 'exists'))
  }
  if (verbose) cat('\nbaixando ', a, '...', sep = '')
  res <- tryCatch({
    r <- suppressWarnings({
      httr::GET(u, httr::write_disk(a, overwrite = TRUE),
                httr::config(ssl_verifypeer = FALSE))
    })
    ct <- httr::headers(r)[['content-type']]
    ct <- ifelse(is.null(ct), 'application', ct)
    if (httr::status_code(r) == 200 && stringr::str_detect(ct, 'application')) {
      if (verbose) cat('OK!\n')
      return(tibble::tibble(result = 'ok'))
    }
  }, error = function(e) as.character(e))
  if (stringr::str_detect(res, 'Timeout')) {
    if (verbose) cat('ERRO!\n')
    return(tibble::tibble(result = 'timeout'))
  }
  if (verbose) cat('ERRO!\n')
  return(tibble::tibble(result = 'nao tem dje'))
}

