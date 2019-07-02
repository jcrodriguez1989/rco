context("opt_common_subexpr")

test_that("common subexpr empty code", {
  code <- paste(
    "",
    sep = "\n"
  )
  opt_code <- opt_common_subexpr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "",
    sep = "\n"
  ))
})

test_that("simple common subexpr elimination", {
  code <- paste(
    "a <- b * c + g",
    "d <- b * c * e",
    "foo <- function() {",
    "  a <- b * c + g",
    "  d <- b * c * e",
    "}",
    sep = "\n"
  )
  opt_code <- opt_common_subexpr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "",
    sep = "\n"
  ))
})

test_that("common subexpr elimination with spaces", {
  code <- paste(
    " a  <-  b  *  c",
    "d <- b * c * e",
    sep = "\n"
  )
  opt_code <- opt_common_subexpr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "",
    sep = "\n"
  ))
})

test_that("common subexpr elimination with loop", {
  code <- paste(
    "a <- 1",
    "b <- 2",
    "res <- 0",
    "while (a < 5) {",
    "  res <- a + b",
    "  a <- a + 1",
    "}",
    sep = "\n"
  )
  opt_code <- opt_common_subexpr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "",
    sep = "\n"
  ))
})
