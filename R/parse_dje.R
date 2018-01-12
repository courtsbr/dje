parse_dje_tjms <- function(text_file){
  clean_text <- text_file %>%
    remove_header() %>%
    remove_footer()

  sumario <- clean_text %>%
    find_index() %>%
    str_split("(SUMÁRIO|\b)\n") %>%
    first() %>%
    str_subset("Juizado|Vara") %>%
    str_trim()

  courts <- sumario %>%
    paste0("\n", ., "\n") %>%
    break_text(clean_text, .) %>%
    append(-1)

  d <- purrr::map(1:(length(courts)-1),
                  ~str_sub(clean_text, courts[.x], courts[.x+1])) %>%
    purrr::map(~break_chunk(.x) %>%
                 first() %>%
                 purrr::map_df(parse_docket_entry)) %>%
    purrr::map2(names(courts)[-length(courts)], ~mutate(.x, location = .y)) %>%
    bind_rows() %>%
    filter(str_detect(lawsuit_id, "Processo"))
}

parse_dje_tjsp <- function(text_file){

  # clean_text <- text_file %>%
  #   remove_header() %>%
  #   remove_footer()

  clean_text <- paste0(text_file, "@fim_do_texto@")

  classify_content <- function(raw_content){
    case_when(
      str_detect(raw_content, "Distribuidor") ~ "D",
      str_detect(raw_content, "[XVI ]+-|Fórum|^[ÇA-ZÃÂÁÍÓÚÉÊ ]+$") ~ "C",
      TRUE ~ "Outros")
  }

  index <- clean_text %>%
    find_index(index_regex = "\nSUMÁRIO\nTRIBUNAL") %>%
    str_split("SUMÁRIO|\n") %>%
    first() %>%
    str_trim() %>%
    str_subset("^[XVI ]+-|Fórum|^[ÇA-ZÃÂÁÍÓÚÉÊ -]+$|Vara|Anexo|Distribuidor|Juizado|Fiscais|Criminal|Cível") %>%
    as.tibble() %>%
    set_names("valor") %>%
    mutate(tipo = classify_content(valor))

  funcao <- function(x, y){
    if(str_detect(x[[1]][length(x[[1]])], "Distribuidor")){
      x <- bind_rows(x, tibble(valor = ifelse(is.na(y), "@fim_do_texto@", y)))
    } else {
      x
    }
  }

  breaks <- index %>%
    filter((tipo == "D")|(lag(tipo) == "D")|(tipo == "C" & lead(tipo) != "C")) %>%
    filter(is.na(lead(tipo))|!(tipo == "C" & lead(tipo) == "C")) %>%
    mutate(classe = ifelse(tipo == "C" & !is.na(lead(tipo)), valor, NA),
           valor = paste0("\n", valor, "\n")) %>%
    fill(classe) %>%
    mutate(classe = paste0("\n", classe, "\n")) %>%
    select(classe, valor) %>%
    group_by(classe) %>%
    nest(.key = "valor") %>%
    mutate(valor = purrr::map2(valor, lead(classe), funcao))

  breaks_counties <- clean_text %>%
    break_text(breaks$classe) %>%
    append(-1)

  cut_text <- function(x,y){

    if(breaks_counties[x+1] == -1){
      text <- clean_text %>%
        str_sub(breaks_counties[x], breaks_counties[x+1])
    } else {
      text <- clean_text %>%
        str_sub(breaks_counties[x], breaks_counties[x+1] + nchar(breaks_counties[x+1])-1)
    }

    print(y$valor[1])
    if(length(y$valor) == 3){

      lim_inf <- text %>%
        break_text(y$valor[2])

      points <- text %>%
        str_sub(lim_inf, -1) %>%
        break_text(y$valor[2:3])

      str_sub(str_sub(text, lim_inf, -1), points1[1], points1[2])
    } else {

      distribuidor <- which(str_detect(y$valor, "Distribuidor"))

      r <- character(length(distribuidor))

      for(i in 1:length(distribuidor)){
        if(i == 1){
          points <- text %>%
            break_text(y$valor[distribuidor[1]:(distribuidor[1]+1)])

          r[i] <- str_sub(text, points[1], points[2])
        } else {
          lim_inf <- text %>%
            break_text(y$valor[distribuidor[i]])

          points <- text %>%
            str_sub(lim_inf, -1) %>%
            break_text(y$valor[distribuidor[i]:(distribuidor[i]+1)])

          r[i] <- str_sub(str_sub(text, lim_inf, -1), points[1], points[2])

        }
      }

      r
      # points1 <- text %>%
      #   break_text(y$valor[distribuidor[1]:(distribuidor[1]+1)])
      #
      # lim_inf <- text %>%
      #   break_text(y$valor[distribuidor[2]])
      #
      # points2 <- text %>%
      #   str_sub(lim_inf, -1) %>%
      #   break_text(y$valor[distribuidor[2]:(distribuidor[2]+1)])
      #
      # c(str_sub(text, points1[1], points1[2]),
      #   str_sub(str_sub(text, lim_inf, -1), points2[1], points2[2]))
    }
  }

  inner_breaks <- purrr::map2(1:length(breaks$classe), breaks$valor,
                              cut_text) %>%
    unlist() %>%
    str_split(pattern = paste0("(?=PROCESSO ?:", cnj_format_sp, ")"))

  d <- breaks %>%
    unnest(valor) %>%
    filter(str_detect(valor, "Distribuidor"))  %>%
    mutate(processos = inner_breaks) %>%
    unnest(processos)
}
