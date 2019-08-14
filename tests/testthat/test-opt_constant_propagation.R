context("opt_constant_propagation")

test_that("correctly constant propagate", {
  code <- paste(
    "x = 3",
    "y = x + 3",
    "xl <- FALSE",
    "yl <- xl",
    "xn <- NA",
    "yn <- xn",
    "xnu <- NULL",
    "ynu <- xnu",
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
    sep = "\n"
  )
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "x = 3",
    "y = 3 + 3",
    "xl <- FALSE",
    "yl <- FALSE",
    "xn <- NA",
    "yn <- NA",
    "xnu <- NULL",
    "ynu <- NULL",
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

test_that("correctly propagate constant strings", {
  code <- paste(
    'xs1 <- ""',
    "ys1 <- xs1",
    "xs2 <- ''",
    "ys2 <- xs2",
    "xs3 <- \"Instituto 'ACC'.\"",
    "ys3 <- xs3",
    'xs4 <- \'Instituto "ACC".\'',
    "ys4 <- xs4",
    sep = "\n"
  )
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    'xs1 <- ""',
    'ys1 <- ""',
    "xs2 <- ''",
    'ys2 <- ""',
    "xs3 <- \"Instituto 'ACC'.\"",
    "ys3 <- \"Instituto 'ACC'.\"",
    'xs4 <- \'Instituto "ACC".\'',
    'ys4 <- "Instituto \\"ACC\\"."',
    sep = "\n"
  ))

  env_orig <- new.env()
  eval(parse(text = code), envir = env_orig)
  env_opt <- new.env()
  eval(parse(text = opt_code), envir = env_opt)

  expect_equal(env_orig, env_opt)
})

test_that("correctly propagate to string-named variables", {
  code <- paste(
    '"a" <- "Instituto"',
    "b <- a",
    '"c" <- a',
    sep = "\n"
  )
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    '"a" <- "Instituto"',
    'b <- "Instituto"',
    '"c" <- "Instituto"',
    sep = "\n"
  ))

  env_orig <- new.env()
  eval(parse(text = code), envir = env_orig)
  env_opt <- new.env()
  eval(parse(text = opt_code), envir = env_opt)

  expect_equal(env_orig, env_opt)
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
    sep = "\n"
  )
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]
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
    "l <- j + 3",
    sep = "\n"
  ))

  env_orig <- new.env()
  eval(parse(text = code), envir = env_orig)
  env_opt <- new.env()
  eval(parse(text = opt_code), envir = env_opt)

  expect_equal(env_orig, env_opt)
})

test_that("constant propagate in while with function call", {
  code <- paste(
    "n <- 10",
    "while (i < n) {",
    "  rm('n')",
    "}",
    "l <- n + 3",
    "",
    "n <- 11",
    "while (i < n) {",
    "  i <- i + n",
    "}",
    "l <- n + 3",
    "",
    sep = "\n"
  )
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "n <- 10",
    "while (i < 10) {",
    "  rm('n')",
    "}",
    "l <- n + 3",
    "",
    "n <- 11",
    "while (i < 11) {",
    "  i <- i + 11",
    "}",
    "l <- 11 + 3",
    sep = "\n"
  ))
})

test_that("constant propagate in strange while", {
  code <- paste(
    "i <- 0",
    "n <- 10",
    "while ((x <- i+1) && (y <- 2) && i < n) {",
    "  i <- i+1",
    "}",
    "z <- y",
    sep = "\n"
  )
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "i <- 0",
    "n <- 10",
    "while ((x <- i+1) && (y <- 2) && i < 10) {",
    "  i <- i+1",
    "}",
    "z <- y",
    sep = "\n"
  ))

  env_orig <- new.env()
  eval(parse(text = code), envir = env_orig)
  env_opt <- new.env()
  eval(parse(text = opt_code), envir = env_opt)

  expect_equal(env_orig, env_opt)
})

test_that("constant propagate in strange while 2", {
  code <- paste(
    "c <- 0",
    "while ((a <- 1) && (b <- a + 1) && c < 10) {",
    "  c <- c + a + b",
    "}",
    "z <- a",
    sep = "\n"
  )
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "c <- 0",
    "while ((a <- 1) && (b <- 1 + 1) && c < 10) {",
    "  c <- c + 1 + b",
    "}",
    "z <- a",
    sep = "\n"
  ))

  env_orig <- new.env()
  eval(parse(text = code), envir = env_orig)
  env_opt <- new.env()
  eval(parse(text = opt_code), envir = env_opt)

  expect_equal(env_orig, env_opt)
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
    sep = "\n"
  )
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]
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

  env_orig <- new.env()
  eval(parse(text = code), envir = env_orig)
  env_opt <- new.env()
  eval(parse(text = opt_code), envir = env_opt)

  expect_equal(env_orig, env_opt)
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
    sep = "\n"
  )
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "x <- 1",
    "i <- 6",
    "for (i in 1:10) {",
    "  j <- i",
    "  k <- 1",
    "  y <- 3",
    "}",
    "l <- y",
    sep = "\n"
  ))

  env_orig <- new.env()
  eval(parse(text = code), envir = env_orig)
  env_opt <- new.env()
  eval(parse(text = opt_code), envir = env_opt)

  expect_equal(env_orig, env_opt)
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
    sep = "\n"
  )
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]
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
    "l <- y",
    sep = "\n"
  ))

  env_orig <- new.env()
  eval(parse(text = code), envir = env_orig)
  env_opt <- new.env()
  eval(parse(text = opt_code), envir = env_opt)

  expect_equal(env_orig, env_opt)
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
    sep = "\n"
  )
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]
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

  env_orig <- new.env()
  eval(parse(text = code), envir = env_orig)
  env_opt <- new.env()
  eval(parse(text = opt_code), envir = env_opt)

  expect_equal(env_orig, env_opt)
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
    sep = "\n"
  )
  opt_code <- opt_constant_propagation(
    list(code),
    in_fun_call = TRUE
  )$codes[[1]]
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

  env_orig <- new.env()
  eval(parse(text = code), envir = env_orig)
  env_opt <- new.env()
  eval(parse(text = opt_code), envir = env_opt)

  env_orig <- as.list(env_orig)
  env_opt <- as.list(env_opt)

  expect_equal(names(env_orig), names(env_opt))
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
    "c = a",
    "d = (e = b)",
    "d = (e = a)",
    "d = a = b",
    "",
    "((-1 -> b) -> a)",
    "a -> c",
    "(b -> e) -> d",
    "(a -> e) -> d",
    "b -> a -> d",
    sep = "\n"
  )
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "(a <- (b <- -1))",
    "c <- -1",
    "d <- (e <- -1)",
    "d <- (e <- -1)",
    "d <- a <- -1",
    "",
    "(a = (b = -1))",
    "c = -1",
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

  env_orig <- new.env()
  eval(parse(text = code), envir = env_orig)
  env_opt <- new.env()
  eval(parse(text = opt_code), envir = env_opt)

  expect_equal(env_orig, env_opt)
})

test_that("constant propagate recursively", {
  code <- paste(
    "a <- -1",
    "b <- a",
    "c <- b",
    sep = "\n"
  )
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "a <- -1",
    "b <- -1",
    "c <- -1",
    sep = "\n"
  ))

  env_orig <- new.env()
  eval(parse(text = code), envir = env_orig)
  env_opt <- new.env()
  eval(parse(text = opt_code), envir = env_opt)

  expect_equal(env_orig, env_opt)
})

test_that("dont propagate if/else variables", {
  code <- paste(
    "x <- 1",
    "if (bool) {",
    "  y <- x",
    "  y <- 2",
    "}",
    "z <- y",
    "z <- x",
    "",
    "x <- 1",
    "y <- 2",
    "if (bool) {",
    "  z <- x",
    "  x <- 2",
    "} else {",
    "  z <- y",
    "  y <- 1",
    "}",
    "z <- y",
    "z <- x",
    sep = "\n"
  )
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "x <- 1",
    "if (bool) {",
    "  y <- 1",
    "  y <- 2",
    "}",
    "z <- y",
    "z <- 1",
    "",
    "x <- 1",
    "y <- 2",
    "if (bool) {",
    "  z <- 1",
    "  x <- 2",
    "} else {",
    "  z <- 2",
    "  y <- 1",
    "}",
    "z <- y",
    "z <- x",
    sep = "\n"
  ))
})

test_that("propagate in if condition", {
  code <- paste(
    "x <- 8818",
    "if (x < y) {",
    "  x <- x + 1",
    "}",
    "y <- x + 8818",
    sep = "\n"
  )
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "x <- 8818",
    "if (8818 < y) {",
    "  x <- 8818 + 1",
    "}",
    "y <- x + 8818",
    sep = "\n"
  ))
})

test_that("dont propagate to <<-", {
  code <- paste(
    "x <- 1",
    "lapply(1:5, function() { x <<- x + 1 })",
    sep = "\n"
  )
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]
  expect_equal(opt_code, code)
})

test_that("constant propagate function call", {
  code <- paste(
    "x <- 1",
    "sum(x)",
    "lapply(1:5, function(w) {",
    "  y <- 1",
    "  z <- x + y",
    "})",
    sep = "\n"
  )
  opt_code <- opt_constant_propagation(
    list(code),
    in_fun_call = TRUE
  )$codes[[1]]
  expect_equal(opt_code, paste(
    "x <- 1",
    "sum(1)",
    "lapply(1:5, function(w) {",
    "  y <- 1",
    "  z <- x + 1",
    "})",
    sep = "\n"
  ))

  env_orig <- new.env()
  eval(parse(text = code), envir = env_orig)
  env_opt <- new.env()
  eval(parse(text = opt_code), envir = env_opt)

  expect_equal(env_orig, env_opt)
})

test_that("dont propagate right to $ or @", {
  code <- paste(
    "name <- NULL",
    "c(foo$name, foo@name)",
    "foo$name",
    "foo@name",
    sep = "\n"
  )
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]
  expect_equal(opt_code, code)
})

test_that("dont propagate right to $ or @", {
  code <- paste(
    "scale <- NA_real_",
    "switch(mid, 'mean'={",
    "  scale <- sd(x)",
    "}, NULL)",
    sep = "\n"
  )
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]
  expect_equal(opt_code, code)
})

test_that("dont propagate in function call", {
  code <- paste(
    "x <- 3",
    "deparse(substitute(x))",
    sep = "\n"
  )
  opt_code <- opt_constant_propagation(list(code))$codes[[1]]
  expect_equal(opt_code, code)
})
