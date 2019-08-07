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
    "  return()",
    "}",
    "",
    "foo <- function() {",
    "  x = y = 8818",
    "  return()",
    "}",
    "",
    "foo <- function() {",
    "  8818 -> y -> x",
    "  return()",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_store(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "foo <- function() {",
    "  8818",
    "  return()",
    "}",
    "",
    "foo <- function() {",
    "  8818",
    "  return()",
    "}",
    "",
    "foo <- function() {",
    "  8818 ",
    "  return()",
    "}",
    sep = "\n"
  ))
})

test_that("dead function store", {
  code <- paste(
    "foo <- function() {",
    "  x <- function() { FALSE }",
    "  return()",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_store(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "foo <- function() {",
    "  function() { FALSE } ",
    "  return()",
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
    "  return()",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_store(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "foo <- function() {",
    "  a <<- 3",
    "  3 ->> b",
    "  return()",
    "}",
    sep = "\n"
  ))
})

test_that("eliminate twice dead store", {
  code <- paste(
    "foo <- function() {",
    "  a <- 0",
    "  a <- 1",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_store(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "foo <- function() {",
    "  0",
    "  1",
    "}",
    sep = "\n"
  ))
})

test_that("dead store eliminate in loop", {
  code <- paste(
    "foo <- function() {",
    "  a <- 0",
    "  i <- 0",
    "  while (i < 1000) {",
    "    i <- i + 1",
    "    a <- i^2",
    "  }",
    "  return(i)",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_store(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "foo <- function() {",
    "  0",
    "  i <- 0",
    "  while (i < 1000) {",
    "    i <- i + 1",
    "    i^2",
    "  }",
    "  return(i)",
    "}",
    sep = "\n"
  ))
})

test_that("dead store eliminate lists", {
  code <- paste(
    "foo <- function() {",
    "  a <- list()",
    "  a$zero <- 0",
    "  b <- 1:3",
    "  a[b] <- 1:3",
    "  c <- 1:3",
    "  return()",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_store(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "foo <- function() {",
    "  a <- list()",
    "  a$zero <- 0",
    "  b <- 1:3",
    "  a[b] <- 1:3",
    "  1:3",
    "  return()",
    "}",
    sep = "\n"
  ))
})

test_that("dead store dont eliminate pkg name", {
  code <- paste(
    "foo <- function() {",
    "  stats <- 8818",
    "  stats::acf(4)",
    "  return()",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_store(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "foo <- function() {",
    "  8818",
    "  stats::acf(4)",
    "  return()",
    "}",
    sep = "\n"
  ))
})

test_that("dead store dont eliminate :=", {
  code <- paste(
    "foo <- function() {",
    "  data <- data[,RandomVariableForImputation:=NULL]",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_store(list(code))$codes[[1]]
  expect_equal(opt_code, code)
})

test_that("dead store dont eliminate mixing assigners", {
  code <- paste(
    "foo <- function() {",
    "  a <- 3",
    "  a = 3",
    "  a <<- 3",
    "  3 ->> a",
    "  3 -> a",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_store(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "foo <- function() {",
    "  3",
    "  3",
    "  a <<- 3",
    "  3 ->> a",
    "  3 ",
    "}",
    sep = "\n"
  ))
})
