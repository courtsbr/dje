library(dje)
context("dje")

test_that("download_dje() is able to download a DJE from SP", {

  # Download DJE
  res <- download_dje("TJSP", "2017-08-07", tempdir())

  # Tests
  expect_length(res$file, 6)
  expect_true(all(res$date == "2017-08-07"))
  expect_equal(res$booklet, c(11:15, 18))
  expect_true(all(res$result == "OK"))
})

test_that("download_dje() is able to download a DJE from MS", {

  # Download DJE
  res <- download_dje("TJMS", "2017-08-07", tempdir())

  # Tests
  expect_length(res$file, 4)
  expect_true(all(res$date == "2017-08-07"))
  expect_equal(res$booklet, 1:4)
  expect_true(all(res$result == "OK"))
})
