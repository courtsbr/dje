header_style <- function(date = 'Publica\u00e7\u00e3o: [a-z\u00e7]+-feira, [0-9]+ de [a-z]+ de 201[0-9]',
             document_type = 'Di\u00e1rio da Justi\u00e7a Eletr\u00f4nico - Caderno Judicial - [0-9]\u00aa Inst\u00e2ncia',
             location = 'Campo Grande, Ano [XVI]+ - Edi\u00e7\u00e3o [0-9]+',
             sep = "\n\n") {
  stringr::str_c(c(date, document_type, location), collapse = sep)
}

footer_style <- function(){
  'Publica\u00e7\u00e3o Oficial do Tribunal de Justi\u00e7a do Estado de Mato Grosso do Sul - Lei Federal n\u00ba 11.419/06, art. 4\u00ba.'
}

break_text <- function(text_chr, patterns){
  positions <- purrr::map_int(patterns, regexpr, text = text_chr)
  names(positions) <- patterns
  sort(positions[positions > 0])
}

remove_header <- function(text_chr, header_regex = header_style()){
  first_pattern <- stringr::str_extract(text_chr, header_regex)

  if(is.na(first_pattern)){
    message("Could not find any headers.")
    return(text_chr)
  }

  clean_text <- stringr::str_replace_all(text_chr, first_pattern, "")
}

remove_footer <- function(text_chr, footer_regex = footer_style()){
  clean_text <- stringr::str_replace_all(text_chr, footer_regex, "")
}

find_index <- function(text_chr, index_regex = "\nSUM\u00c1RIO\n"){
  position <- regexpr(index_regex, text_chr)

  summary_chr <- stringr::str_sub(text_chr, position[1], -1)
}

break_chunk <- function(txt_chunk) {
  cnj_format <- stringr::regex("[0-9]{7}\\-[0-9]{2}\\.[0-9]{4}\\.8\\.12\\.[0-9]{4}")
  txt_chunk %>%
    stringr::str_split(paste0("(?=(\nProcesso ", cnj_format, "))"))
}

parse_docket_entry <- function(raw_docket_entry){
  raw_docket_entry %>%
    stringr::str_split(pattern = "(?=\n(Herdeir[oa]:|Impugd[oa]:|Exqte:|Credor Hip:|Infratora?:|A\\. Fato:|Alimtd[oa]:|Alimtte:|R\u00e9u?:|Indiciad[ao]:|Exectd[ao]:|Reclamte:|Imptte:|Embargte:|Exeqte:|Reqte:|Impugte:|Denunciad[ao]:|Menor:|Embargd[ao]:|Re[qp]d[oa]:|Agravd[ao]:|Querelada:|Cession\u00e1ri[ao]?:|Recorrente:|Reclamd[ao]:|Adotante:|Interesd[oa]\\.?:|Opte:|Invtante:|Autora?:))") %>%
    dplyr::first() %>%
    {if(length(.) != 2) c(paste(., collapse = ""), NA) else .} %>%
    purrr::set_names(c("lawsuit_id", "docket")) %>%
    as.list()
}

