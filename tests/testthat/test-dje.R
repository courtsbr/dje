context("dje")

n_arqs <- tibble::tibble(
  tj = c('TJSP'),
  n_arqs = c(6)
)

expect_dje <- function(tj = 'TJSP', date = "2017-08-07") {
  download_dje(tj = tj, dates = date, path = '.')
  path <- sprintf('%s_dje_%s', tolower(tj), date)
  arqs <- dir(path, full.names = TRUE)
  pdfs <- sapply(arqs, readr::read_lines, n_max = 1L)
  v <- n_arqs[n_arqs$tj == tj,][['n_arqs']]
  val <- all(stringr::str_detect(pdfs, 'PDF')) && length(arqs) == v
  expect(val, 'Nao tem o numero certo de arquivos ou nao sao PDF.')
}

test_that("baixa tjsp de hoje", {
  tj <- 'TJSP'
  date <- Sys.Date()
  exp <- expect_dje(tj, date)
  unlink(sprintf('%s_dje_%s', tolower(tj), date), recursive = TRUE)
  invisible(exp)
})
