context("opt_dead_store")

test_that("dead store empty code", {
  code <- paste(
    "",
    sep = "\n"
  )
  opt_code <- opt_dead_store(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "",
    sep = "\n"
  ))
})

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
    "  return(NULL)",
    "}",
    "",
    "foo <- function() {",
    "  8818",
    "  return(NULL)",
    "}",
    "",
    "foo <- function() {",
    "  8818 ",
    "  return(NULL)",
    "}",
    sep = "\n"
  ))
})

test_that("dead function store", {
  code <- paste(
    "foo <- function() {",
    "  x <- function() { FALSE }",
    "  return(NULL)",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_store(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "foo <- function() {",
    "  function() { FALSE }",
    "  return(NULL)",
    "}",
    sep = "\n"
  ))
})

test_that("dead store function in function", {
  code <- paste(
    "foo <- function() {",
    "  a <- 3",
    "  bar <- function() {",
    "    a <- 4",
    "    5",
    "  }",
    "  a + 4",
    "  res <- bar",
    "  return(res)",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_store(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "foo <- function() {",
    "  a <- 3",
    "  bar <- function() {",
    "    4",
    "    5",
    "  }",
    "  a + 4",
    "  res <- bar",
    "  return(res)",
    "}",
    sep = "\n"
  ))
})

test_that("dont dead store <<-", {
  code <- paste(
    "foo <- function() {",
    "  a <<- 3",
    "  3 ->> b",
    "  return(NULL)",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_store(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "foo <- function() {",
    "  a <<- 3",
    "  3 ->> b",
    "  return(NULL)",
    "}",
    sep = "\n"
  ))
})
