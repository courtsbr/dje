parse_dje_tjms <- function(text_file){

  txt <- readr::read_file(text_file)



  clean_text <- txt %>%

    remove_header() %>%

    remove_footer()



  sumario <- clean_text %>%

    find_index() %>%

    stringr::str_split("(SUM\u00c1RIO|\b)\n") %>%

    dplyr::first() %>%

    stringr::str_subset("Juizado|Vara") %>%

    stringr::str_trim()



  courts <- sumario %>%

    paste0("\n", ., "\n") %>%

    break_text(clean_text, .) %>%

    append(-1)



  d <- purrr::map(1:(length(courts)-1),

                  ~stringr::str_sub(clean_text, courts[.x], courts[.x+1])) %>%

    purrr::map(~break_chunk(.x) %>%

                 dplyr::first() %>%

                 purrr::map_df(parse_docket_entry)) %>%

    purrr::map2(names(courts)[-length(courts)],

                ~dplyr::mutate(.x, location = .y)) %>%

    dplyr::bind_rows() %>%

    dplyr::filter(stringr::str_detect(lawsuit_id, "Processo"))

}

#' Parse DJE file
#' parse TJSP file obtained from [dje_to_text()].
#' @param text_file path of the file to parse.
#' @return tibble containing lawsuit and court information
#' @export

parse_dje_tjsp <- function(text_file) {
  text_file = "/home/giovanni/Desktop/dje/tjsp_dje_2017-10-02_txt/tjsp_18_2017-10-02.txt"
  txt <- readr::read_file(text_file)
  cnj_format_sp <- stringr::regex("[0-9]{7}\\-[0-9]{2}\\.[0-9]{4}\\.8\\.26\\.[0-9]{4}")
  clean_text <- stringr::str_remove_all(txt, "Publica\u00e7\u00e3o Oficial do Tribunal de Justi\u00e7a do Estado de S\u00E3o Paulo - Lei Federal n\u00ba 11.419/06, art. 4\u00ba\n") %>%
    stringr::str_remove_all("Disponibiliza\u00e7\u00e3o: [a-z\u00e7]+-feira, [0-9]+ de [a-z]+ de 201[0-9] ") %>%
    stringr::str_remove_all("Di\u00e1rio da Justi\u00e7a Eletr\u00f4nico - Caderno Judicial .+") %>%
    stringr::str_remove_all("S\u00E3o Paulo, Ano XI - Edi\u00e7\u00e3o [0-9]+ [0-9]\n") %>%
    stringr::str_remove_all("RELA\u00c7\u00c3O DOS FEITOS .+\\n?[A-Z\u0020]*[0-9]{2}\u002F[0-9]{2}\u002F[0-9]{4}") %>%
    stringr::str_remove_all("RELA\u00c7\u00c3O DE CARTAS .+\\n?[A-Z\u0020]*[0-9]{2}\u002F[0-9]{2}\u002F[0-9]{4}") %>%
    stringr::str_remove_all("[A-Z\u0020]*[0-9]{2}\u002F[0-9]{2}\u002F[0-9]{4}\n") %>%
    stringr::str_remove_all("[\\´\u000C\\']") %>%
    paste0("@fim_do_texto@")

  classify_content <- function(raw_content) {
    dplyr::case_when(
      stringr::str_detect(raw_content, "Distribuidor") ~ "D",
      stringr::str_detect(raw_content, "[XVI ]+-|F\u00f3rum|^[\u00c7A-Z\u00c3\u00c2\u00c1\u00cd\u00d3\u00da\u00c9\u00ca ]+$") ~ "C",
      TRUE ~ "Outros")
  }

  index <- clean_text %>%
    find_index(index_regex = "\nSUM\u00c1RIO\nTRIBUNAL") %>%
    stringr::str_split("SUM\u00c1RIO|\n") %>%
    dplyr::first() %>%
    stringr::str_trim() %>%
    stringr::str_subset("^[XVI ]+-|F\u00f3rum|^[\u00c7A-Z\u00c3\u00c2\u00c1\u00cd\u00d3\u00da\u00c9\u00ca [0-9] -]+$|Vara|Anexo|Distribuidor|Juizado|Fiscais|Criminal|C\u00edvel|Col\u00E9gio|J\u00FAri|Inf\u00E2ncia|Execu\u00E7\u00F5es|Centro|Peti\uE7\u00F5es|DEECRIM|Fam\u00EDlia") %>%
    stringr::str_remove("[ 0-9]+$") %>%
    tibble::as.tibble() %>%
    purrr::set_names("valor") %>%
    dplyr::mutate(tipo = classify_content(valor))

  clean_breaks <- function(x, y) {
    if (stringr::str_detect(x[[1]][length(x[[1]])], "Distribuidor")) {
      tab2 <- tibble::tibble(valor = ifelse(is.na(y), "@fim_do_texto@", y))
      x <- dplyr::bind_rows(x, tab2)
    } else {
      return(x)
    }
  }

  breaks <- index %>%
    dplyr::filter((tipo == "D") | (dplyr::lag(tipo) == "D") | (tipo == "C" & dplyr::lead(tipo) != "C")) %>%
    dplyr::filter(is.na(dplyr::lead(tipo)) | !(tipo == "C" & dplyr::lead(tipo) == "C") | stringr::str_to_upper(valor) == valor) %>%
    dplyr::mutate(classe = ifelse(tipo == "C" & !is.na(dplyr::lead(tipo)), valor, NA), valor = paste0("\n", valor, "\n")) %>%
    tidyr::fill(classe) %>%
    dplyr::mutate(classe = paste0("\n", classe, "\n")) %>%
    dplyr::select(classe, valor) %>%
    dplyr::group_by(classe) %>%
    tidyr::nest(.key = "valor") %>%
    dplyr::mutate(valor = purrr::map2(valor, dplyr::lead(classe), clean_breaks)) %>%
    dplyr::rowwise() %>%
    dplyr::filter(nrow(valor) > 1) %>%
    dplyr::ungroup()

  breaks_counties <- clean_text %>%
    break_text(breaks$classe) %>%
    append(-1)

  cut_text <- function(x, y) {
    if (breaks_counties[x+1] == -1){
      text <- clean_text %>%
        stringr::str_sub(breaks_counties[x], breaks_counties[x+1])
    } else {
      text <- clean_text %>%
        stringr::str_sub(breaks_counties[x],
                         breaks_counties[x + 1] + nchar(names(breaks_counties[x + 1])) - 1)
    }

    if(length(y$valor) == 3) {
      lim_inf <- text %>%
        break_text(y$valor[2])
      points <- text %>%
        stringr::str_sub(lim_inf, -1) %>%
        break_text(y$valor[2:3])
      r <- stringr::str_sub(stringr::str_sub(text, lim_inf, -1), points[1], points[2])
    } else {
      distribuidor <- which(stringr::str_detect(y$valor, "Distribuidor"))
      r <- character(length(distribuidor))
      for(i in 1:length(distribuidor)) {
        if(i == 1){
          points <- text %>%
            break_text(y$valor[distribuidor[1]:(distribuidor[1] + 1)])
          r[i] <- stringr::str_sub(text, points[1], points[2])
        } else {
          lim_inf <- text %>%
            break_text(y$valor[distribuidor[i]])
          points <- text %>%
            stringr::str_sub(lim_inf, -1) %>%
            stringr::str_c("\n") %>%
            break_text(y$valor[distribuidor[i]:(distribuidor[i]+1)])
          r[i] <- stringr::str_sub(stringr::str_sub(text, lim_inf, -1),
                                 points[1], points[2])
        }
      }
      return(r)
    }
  }

  inner_breaks <- purrr::map2(seq_along(breaks$classe), breaks$valor, cut_text) %>%
    unlist() %>%
    stringr::str_split(pattern = stringr::regex(paste0("(?=PROCESSO ?:(\u0020)*", cnj_format_sp, ")"), ignore_case = TRUE)) %>%
    purrr::map(~stringr::str_split(.x, pattern = "(?=\nPROCESSO\n)") %>% rlang::squash_chr())  # RAW DANDO ERRADO

  d <- breaks %>%
    tidyr::unnest(valor) %>%
    dplyr::filter(stringr::str_detect(valor, "Distribuidor"))  %>%
    dplyr::mutate(processos = inner_breaks) %>%
    tidyr::unnest(processos) %>%
    dplyr::filter(stringr::str_detect(processos,"PROCESSO"))

  readr::write_csv(d, "/home/giovanni/Desktop/dje/tjsp_dje_2017-10-02_txt/d2.csv")
}
