library(tidyverse)
download_cadernos <- function(dt_diario) {
  u_cadernos <- "https://www.dje.tjsp.jus.br/cdje/getListaDeCadernos.do"
  parm <- list(dtDiario = format(dt_diario, "%d/%m/%Y"))
  r_caderno <- httr::GET(u_cadernos, query = parm,
                         httr::config(ssl_verifypeer = FALSE))
  txt <-  httr::content(r_caderno, "text")
  if (txt == "DATA SEM DIARIO") return(tibble::tibble(tem_diario = FALSE))
  txt %>%
    stringr::str_squish() %>%
    stringr::str_replace_all("(?<= |\\{)([^: {]+)(?=:)", '"\\1"') %>%
    stringr::str_replace_all("'", '"') %>%
    jsonlite::fromJSON() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(tem_diario = TRUE)
}
download_secoes <- function(data) {
  u_secoes <- "https://www.dje.tjsp.jus.br/cdje/getListaDeSecoes.do"
  parm_secao <- list("cdVolume" = data$cdVolume,
                     "nuDiario" = data$nuDiario,
                     "cdCaderno" = data$cdCaderno)
  r_secoes <- httr::GET(u_secoes, query = parm_secao,
                        httr::config(ssl_verifypeer = FALSE))
  result <- r_secoes %>%
    httr::content("text") %>%
    stringr::str_squish() %>%
    stringr::str_remove_all('"') %>%
    stringr::str_replace_all("[IVX]:", "I") %>%
    stringr::str_replace_all("(?<= |\\{)([^: {]+)(?=:)", '"\\1"') %>%
    stringr::str_replace_all("'", '"') %>%
    jsonlite::fromJSON()
  result[[2]]$num_json <- result[[1]]
  tibble::as_tibble(result[[2]])
}
download_esqueleto_dje_ <- function(dt_diario) {
  dt_diario <- as.Date(dt_diario)
  cadernos <- download_cadernos(dt_diario)
  if (any(!cadernos$tem_diario)) return(cadernos)
  cadernos %>%
    dplyr::select(cdVolume, nuDiario, cdCaderno) %>%
    purrr::transpose() %>%
    purrr::map_dfr(download_secoes, .id = "id_caderno") %>%
    dplyr::inner_join(cadernos, c("cdVolume", "nuDiario", "cdCaderno"))
}

#' Downloads DJE structure
#'
#' Downloads DJE structure from TJSP site
#'
#' @param dt_diario vector of dates in YYYY-MM-DD format.
#'
#' @return tibble containing all section names and pages. Only for TJSP.
#'
#' @export
download_skeleton_dje <- function(dt_diario) {
  dt_diario %>%
    abjutils::pvec(download_esqueleto_dje_) %>%
    dplyr::mutate(dt_diario = dt_diario) %>%
    dplyr::select(dt_diario, output) %>%
    tidyr::unnest(output) %>%
    janitor::clean_names()
}

