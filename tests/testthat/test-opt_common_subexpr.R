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
    "a <- 1 * 2 + 3",
    "b <- 1 * 2 * 3",
    "foo <- function(x) {",
    "  c <- 2 * x + 3",
    "  d <- 2 * x * 3",
    "}",
    "e <- 1 * 2 - 3",
    sep = "\n"
  )
  opt_code <- opt_common_subexpr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "cs_1 <- 1 * 2",
    "a <- cs_1 + 3",
    "b <- cs_1 * 3",
    "foo <- function(x) {",
    "  cs_1 <- 2 * x",
    "  c <- cs_1 + 3",
    "  d <- cs_1 * 3",
    "}",
    "e <- cs_1 - 3",
    sep = "\n"
  ))
})

test_that("common subexpr elimination with precedence", {
  code <- paste(
    "a <- 1 * 2 + 3",
    "b <- 4 * 2 + 3",
    "c <- 1 * (2 + 3)",
    "d <- 4 * (2 + 3)",
    "e <- 2 + 3",
    sep = "\n"
  )
  opt_code <- opt_common_subexpr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "a <- 1 * 2 + 3",
    "b <- 4 * 2 + 3",
    sep = "\n"
  ))
})

test_that("common subexpr elimination with spaces", {
  code <- paste(
    " a  <-  1  *  2",
    "b <- 1 * 2 * 3",
    sep = "\n"
  )
  opt_code <- opt_common_subexpr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "cs_1 <- 1  *  2",
    "a  <-  cs_1",
    "b <- cs_1 * 3",
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
