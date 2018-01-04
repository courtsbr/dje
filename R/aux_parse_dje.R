#' @export
header_style <- function(date = 'Publicação: [a-zç]+-feira, [0-9]+ de [a-z]+ de 201[0-9]',
             document_type = 'Diário da Justiça Eletrônico - Caderno Judicial - [0-9]ª Instância',
             location = 'Campo Grande, Ano [XVI]+ - Edição [0-9]+',
             separation = "\n\n"){
  str_c(c(date, document_type, location), collapse = separation)
}

#' @export
footer_style <- function(){
  'Publicação Oficial do Tribunal de Justiça do Estado de Mato Grosso do Sul - Lei Federal nº 11.419/06, art. 4º.'
}

#' @export
break_text <- function(text_chr, patterns){
  positions <- purrr::map_int(patterns, regexpr, text = text_chr)
  names(positions) <- patterns
  sort(positions[positions > 0])
}

#' @export
remove_header <- function(text_chr, header_regex = header_style()){
  first_pattern <- stringr::str_extract(text_chr, header_regex)

  if(is.na(first_pattern)){
    message("Could not find any headers.")
    return(text_chr)
  }

  clean_text <- stringr::str_replace_all(text_chr, first_pattern, "")
}

#' @export
remove_footer <- function(text_chr, footer_regex = footer_style()){
  clean_text <- stringr::str_replace_all(text_chr, footer_regex, "")
}

#' @export
find_index <- function(text_chr, index_regex = "\nSUMÁRIO\n"){
  position <- regexpr(index_regex, text_chr)

  summary_chr <- stringr::str_sub(text_chr, position[1], -1)
}

#' @export
break_chunk <- function(txt_chunk){
  txt_chunk %>%
    stringr::str_split(paste0("(?=(\nProcesso ", cnj_format, "))"))
}

#' @export
parse_docket_entry <- function(raw_docket_entry){
  raw_docket_entry %>%
    stringr::str_split(pattern = "(?=\n(Herdeir[oa]:|Impugd[oa]:|Exqte:|Credor Hip:|Infratora?:|A\\. Fato:|Alimtd[oa]:|Alimtte:|Réu?:|Indiciad[ao]:|Exectd[ao]:|Reclamte:|Imptte:|Embargte:|Exeqte:|Reqte:|Impugte:|Denunciad[ao]:|Menor:|Embargd[ao]:|Re[qp]d[oa]:|Agravd[ao]:|Querelada:|Cessionári[ao]?:|Recorrente:|Reclamd[ao]:|Adotante:|Interesd[oa]\\.?:|Opte:|Invtante:|Autora?:))") %>%
    dplyr::first() %>%
    {if(length(.) != 2){
      c(paste(., collapse = ""), NA)
    } else {
      .
    }} %>%
    purrr::set_names(c("lawsuit_id", "docket")) %>%
    as.list()
}

