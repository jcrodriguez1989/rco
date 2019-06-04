context("opt_constant_propagation")

test_that("correctly constant propagate", {
  code <- paste(
    "x = 3",
    "y = x + 3",
    "x <- ( -170 )",
    "rm_everything <- function() {",
    "  y <- x",
    "  env <- .GlobalEnv",
    "  rm(list=ls(envir = env), envir = env)",
    "}",
    "rm_everything()",
    "x <- -170",
    "x <- x + 4",
    "j <- 120 + 5 * 10",
    "i <- { 170 }",
    "y <- x + 124",
    "x <- 124",
    "y <- x + 124",
    "z <- i - 124",
    "w <- -x + i",
    "aa <- function(y = 4, x = y + 34, z = y) {",
    "  j <- x",
    "  x <- x",
    "  x = 3",
    "  y <- x",
    "  y",
    "}",
    sep = "\n")
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]; cat(opt_code)
  expect_equal(opt_code, paste(
    "x = 3",
    "y = 3 + 3",
    "x <- ( -170 )",
    "rm_everything <- function() {",
    "  y <- x",
    "  env <- .GlobalEnv",
    "  rm(list=ls(envir = env), envir = env)",
    "}",
    "rm_everything()",
    "x <- -170",
    "x <- -170 + 4",
    "j <- 120 + 5 * 10",
    "i <- { 170 }",
    "y <- x + 124",
    "x <- 124",
    "y <- 124 + 124",
    "z <- 170 - 124",
    "w <- -124 + 170",
    "aa <- function(y = 4, x = y + 34, z = y) {",
    "  j <- x",
    "  x <- x",
    "  x = 3",
    "  y <- 3",
    "  y",
    "}",
    sep = "\n"
  ))
})

test_that("constant propagate in while", {
  code <- paste(
    "x <- 1",
    "i <- 0",
    "n <- 100",
    "res <- 0",
    "while (i <n) {",
    "  j <- 3",
    "  k <- j + 3",
    "  res <- i * n + x",
    "  x <- x + 1",
    "  i <- i + 1",
    "}",
    "l <- j + 3",
    sep = "\n")
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]; cat(opt_code)
  expect_equal(opt_code, paste(
    "x <- 1",
    "i <- 0",
    "n <- 100",
    "res <- 0",
    "while (i <100) {",
    "  j <- 3",
    "  k <- 3 + 3",
    "  res <- i * 100 + x",
    "  x <- x + 1",
    "  i <- i + 1",
    "}",
    "l <- 3 + 3",
    sep = "\n"
  ))

  env1 <- new.env()
  eval(parse(text = code), envir = env1)
  env2 <- new.env()
  eval(parse(text = opt_code), envir = env2)
  expect_equal(as.list(env1), as.list(env2))
})

test_that("constant propagate in strange while", {
  code <- paste(
    "i <- 0",
    "n <- 10",
    "while ((x <- i+1) && (y <- 2) && i < n) {",
    "  i <- i+1",
    "}",
    "z <- y",
    sep = "\n")
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]; cat(opt_code)
  expect_equal(opt_code, paste(
    "i <- 0",
    "n <- 10",
    "while ((x <- i+1) && (y <- 2) && i < 10) {",
    "  i <- i+1",
    "}",
    "z <- 2",
    sep = "\n"
  ))

  env1 <- new.env()
  eval(parse(text = code), envir = env1)
  env2 <- new.env()
  eval(parse(text = opt_code), envir = env2)
  expect_equal(as.list(env1), as.list(env2))
})

test_that("constant propagate in strange while 2", {
  code <- paste(
    "c <- 0",
    "while ((a <- 1) && (b <- a + 1) && c < 10) {",
    "  c <- c + a + b",
    "}",
    "z <- a",
    sep = "\n")
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]; cat(opt_code)
  expect_equal(opt_code, paste(
    "c <- 0",
    "while ((a <- 1) && (b <- 1 + 1) && c < 10) {",
    "  c <- c + 1 + b",
    "}",
    "z <- 1",
    sep = "\n"
  ))

  env1 <- new.env()
  eval(parse(text = code), envir = env1)
  env2 <- new.env()
  eval(parse(text = opt_code), envir = env2)
  expect_equal(as.list(env1), as.list(env2))
})

test_that("constant propagate in repeat", {
  code <- paste(
    "i <- 0",
    "j <- 4",
    "n <- 10",
    "repeat {",
    "  x <- j + i",
    "  i <- i + 1",
    "  if (i > n) {",
    "    break",
    "  }",
    "}",
    sep = "\n")
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]; cat(opt_code)
  expect_equal(opt_code, paste(
    "i <- 0",
    "j <- 4",
    "n <- 10",
    "repeat {",
    "  x <- 4 + i",
    "  i <- i + 1",
    "  if (i > 10) {",
    "    break",
    "  }",
    "}",
    sep = "\n"
  ))

  env1 <- new.env()
  eval(parse(text = code), envir = env1)
  env2 <- new.env()
  eval(parse(text = opt_code), envir = env2)
  expect_equal(as.list(env1), as.list(env2))
})

test_that("constant propagate in for", {
  code <- paste(
    "x <- 1",
    "i <- 6",
    "for (i in 1:10) {",
    "  j <- i",
    "  k <- x",
    "  y <- 3",
    "}",
    "l <- y",
    sep = "\n")
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]; cat(opt_code)
  expect_equal(opt_code, paste(
    "x <- 1",
    "i <- 6",
    "for (i in 1:10) {",
    "  j <- i",
    "  k <- 1",
    "  y <- 3",
    "}",
    "l <- 3",
    sep = "\n"
  ))

  env1 <- new.env()
  eval(parse(text = code), envir = env1)
  env2 <- new.env()
  eval(parse(text = opt_code), envir = env2)
  expect_equal(as.list(env1), as.list(env2))
})

test_that("constant propagate in strange for", {
  code <- paste(
    "i <- 0",
    "x <- 1",
    "for (i in {",
    "  x <- 2",
    "  y <- 3",
    "  1:10",
    "  }) {",
    "  j <- i",
    "  k <- x",
    "}",
    "l <- y",
    sep = "\n")
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]; cat(opt_code)
  expect_equal(opt_code, paste(
    "i <- 0",
    "x <- 1",
    "for (i in {",
    "  x <- 2",
    "  y <- 3",
    "  1:10",
    "  }) {",
    "  j <- i",
    "  k <- 2",
    "}",
    "l <- 3",
    sep = "\n"
  ))

  env1 <- new.env()
  eval(parse(text = code), envir = env1)
  env2 <- new.env()
  eval(parse(text = opt_code), envir = env2)
  expect_equal(as.list(env1), as.list(env2))
})

test_that("constant propagate in strange for 2", {
  code <- paste(
    "c <- 0",
    "for (i in {",
    "  a <- 1",
    "  b <- a + 2",
    "  1:10",
    "  }) {",
    "  c <- c + a + b",
    "}",
    sep = "\n")
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]; cat(opt_code)
  expect_equal(opt_code, paste(
    "c <- 0",
    "for (i in {",
    "  a <- 1",
    "  b <- 1 + 2",
    "  1:10",
    "  }) {",
    "  c <- c + 1 + b",
    "}",
    sep = "\n"
  ))

  env1 <- new.env()
  eval(parse(text = code), envir = env1)
  env2 <- new.env()
  eval(parse(text = opt_code), envir = env2)
  expect_equal(as.list(env1), as.list(env2))
})

test_that("constant propagate in function", {
  code <- paste(
    "x <- 1",
    "j <- 2",
    "foo <- function() {",
    "  x <- 3",
    "  y <- x * 3",
    "  j <- 4",
    "  return(x)",
    "}",
    "j <- j",
    "y <- x",
    sep = "\n")
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]; cat(opt_code)
  expect_equal(opt_code, paste(
    "x <- 1",
    "j <- 2",
    "foo <- function() {",
    "  x <- 3",
    "  y <- 3 * 3",
    "  j <- 4",
    "  return(3)",
    "}",
    "j <- 2",
    "y <- 1",
    sep = "\n"
  ))

  env1 <- new.env()
  eval(parse(text = code), envir = env1)
  env2 <- new.env()
  eval(parse(text = opt_code), envir = env2)
  env1 <- as.list(env1)
  env2 <- as.list(env2)

  expect_equal(names(env1), names(env1))
  expect_equal(env1$res, env2$res)
})

test_that("constant propagate with double assign", {
  code <- paste(
    "(a <- (b <- -1))",
    "c <- a",
    "d <- (e <- b)",
    "d <- (e <- a)",
    "d <- a <- b",
    "",
    "(a = (b = -1))",
    "c <- a",
    "d = (e = b)",
    "d = (e = a)",
    "d = a = b",
    "",
    "((-1 -> b) -> a)",
    "a -> c",
    "(b -> e) -> d",
    "(a -> e) -> d",
    "b -> a -> d",
    sep = "\n")
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]; cat(opt_code)
  expect_equal(opt_code, paste(
    "(a <- (b <- -1))",
    "c <- -1",
    "d <- (e <- -1)",
    "d <- (e <- -1)",
    "d <- a <- -1",
    "",
    "(a = (b = -1))",
    "c <- -1",
    "d = (e = -1)",
    "d = (e = -1)",
    "d = a = -1",
    "",
    "((-1 -> b) -> a)",
    "-1 -> c",
    "(-1 -> e) -> d",
    "(-1 -> e) -> d",
    "-1 -> a -> d",
    sep = "\n"
  ))

  env1 <- new.env()
  eval(parse(text = code), envir = env1)
  env2 <- new.env()
  eval(parse(text = opt_code), envir = env2)
  env1 <- as.list(env1)
  env2 <- as.list(env2)

  expect_equal(names(env1), names(env1))
  expect_equal(env1$res, env2$res)
})

test_that("constant propagate recursively", {
  code <- paste(
    "a <- -1",
    "b <- a",
    "c <- b",
    sep = "\n")
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]; cat(opt_code)
  expect_equal(opt_code, paste(
    "a <- -1",
    "b <- -1",
    "c <- -1",
    sep = "\n"
  ))

  env1 <- new.env()
  eval(parse(text = code), envir = env1)
  env2 <- new.env()
  eval(parse(text = opt_code), envir = env2)
  env1 <- as.list(env1)
  env2 <- as.list(env2)

  expect_equal(names(env1), names(env1))
  expect_equal(env1$res, env2$res)
})
