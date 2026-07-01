test_that("get_pseudobnf() validates release argument", {
  expect_error(
    get_pseudobnf(release = c("latest", "other")),
    "must be a non-empty character string"
  )
  expect_error(
    get_pseudobnf(release = ""),
    "must be a non-empty character string"
  )
  expect_error(
    get_pseudobnf(release = 123),
    "must be a non-empty character string"
  )
})

test_that("get_pseudobnf() validates dir_path exists", {
  expect_error(
    get_pseudobnf(dir_path = "/nonexistent/path"),
    "Directory does not exist"
  )
})

test_that("get_pseudobnf() downloads the latest BNF code information CSV", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("nhsbsa")

  dir <- withr::local_tempdir()
  path <- get_pseudobnf(dir_path = dir, quiet = TRUE)

  expect_true(file.exists(path))
  expect_match(basename(path), "\\.csv$")
})
