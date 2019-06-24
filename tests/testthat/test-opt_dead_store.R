context("opt_dead_store")

test_that("dead store eliminate middle assigned", {
  code <- paste(
    "foo <- function() {",
    "  x <- y <- 8818",
    "  return(x)",
    "}",
    "",
    "foo <- function() {",
    "  x = y = 8818",
    "  return(x)",
    "}",
    "",
    "foo <- function() {",
    "  8818 -> y -> x",
    "  return(x)",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_store(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "foo <- function() {",
    "  x <- 8818",
    "  return(x)",
    "}",
    "",
    "foo <- function() {",
    "  x = 8818",
    "  return(x)",
    "}",
    "",
    "foo <- function() {",
    "  8818 -> x",
    "  return(x)",
    "}",
    sep = "\n"
  ))
})

test_that("dead store eliminate both assigned", {
  code <- paste(
    "foo <- function() {",
    "  x <- y <- 8818",
    "  return(NULL)",
    "}",
    "",
    "foo <- function() {",
    "  x = y = 8818",
    "  return(NULL)",
    "}",
    "",
    "foo <- function() {",
    "  8818 -> y -> x",
    "  return(NULL)",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_store(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "foo <- function() {",
    "  8818",
    "  return(x)",
    "}",
    "",
    "foo <- function() {",
    "  8818",
    "  return(x)",
    "}",
    "",
    "foo <- function() {",
    "  8818",
    "  return(x)",
    "}",
    sep = "\n"
  ))
})

