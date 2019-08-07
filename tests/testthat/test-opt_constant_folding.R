context("opt_constant_folding")

test_that("correctly constant fold", {
  code <- paste(
    "x <- - 2 + (4 - 3)",
    "x <- - 1 + 2 - 3 * 4 / 5 ^ 6",
    "x <- (1000 + 2 - 3 * 4) / 5 ^ (6 - 1)",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 >  3 * 1",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 >= 3",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 <  3",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 <=  3",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 == 3",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 != 3",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 != 3 & TRUE",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 != 3 & FALSE",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 != 3 | TRUE",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 != 3 | FALSE",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 != 3 && TRUE",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 != 3 && FALSE",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 != 3 || TRUE",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 != 3 || FALSE",
    "y <- 14 + 14 + x + 14 + 14",
    "for (i in 1:100) {",
    "  i <- i + 7 * 3",
    "}",
    sep = "\n"
  )
  opt_code <- opt_constant_folding(list(code), fold_floats = TRUE)$codes[[1]]
  expect_equal(opt_code, paste(
    "x <- (-1) ",
    "x <- 0.999232",
    "x <-  0.3168 ",
    "x <- 0.999232",
    "x <- FALSE",
    "x <- FALSE",
    "x <- TRUE",
    "x <- TRUE",
    "x <- FALSE",
    "x <- TRUE",
    "x <- TRUE",
    "x <- FALSE",
    "x <- TRUE",
    "x <- TRUE",
    "x <- TRUE",
    "x <- FALSE",
    "x <- TRUE",
    "x <- TRUE",
    "y <- 28 + x + 14 + 14",
    "for (i in 1:100) {",
    "  i <- i + 21",
    "}",
    sep = "\n"
  ))

  env_orig <- new.env()
  eval(parse(text = code), envir = env_orig)
  env_opt <- new.env()
  eval(parse(text = opt_code), envir = env_opt)

  expect_equal(env_orig, env_opt)
})

test_that("dont fold floats", {
  code <- paste(
    "x <- - 2 + (4 - 3)",
    "x <- - 1 + 2 - 3 * 4 / 5 ^ 6",
    "x <- (1000 + 2 - 3 * 4) / 5 ^ (6 - 1)",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 >  3 * 1",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 >= 3",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 <  3",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 <=  3",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 == 3",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 != 3",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 != 3 & TRUE",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 != 3 & FALSE",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 != 3 | TRUE",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 != 3 | FALSE",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 != 3 && TRUE",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 != 3 && FALSE",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 != 3 || TRUE",
    "x <- 2 - 3 * 4 / 5 ^ 6 - 1 != 3 || FALSE",
    "y <- 14 + 14 + x + 14 + 14",
    "for (i in 1:100) {",
    "  i <- i + 7 * 3",
    "}",
    sep = "\n"
  )
  opt_code <- opt_constant_folding(list(code), fold_floats = FALSE)$codes[[1]]
  expect_equal(opt_code, paste(
    "x <- (-1) ",
    "x <- 1 - 12 / 15625",
    "x <-  990 / 3125 ",
    "x <- 2 - 12 / 15625 - 1",
    "x <- FALSE",
    "x <- FALSE",
    "x <- TRUE",
    "x <- TRUE",
    "x <- FALSE",
    "x <- TRUE",
    "x <- TRUE",
    "x <- FALSE",
    "x <- TRUE",
    "x <- TRUE",
    "x <- TRUE",
    "x <- FALSE",
    "x <- TRUE",
    "x <- TRUE",
    "y <- 28 + x + 14 + 14",
    "for (i in 1:100) {",
    "  i <- i + 21",
    "}",
    sep = "\n"
  ))

  env_orig <- new.env()
  eval(parse(text = code), envir = env_orig)
  env_opt <- new.env()
  eval(parse(text = opt_code), envir = env_opt)

  expect_equal(env_orig, env_opt)
})

test_that("constant fold in while", {
  code <- paste(
    "i <- 0 * 512;",
    "n <- 100",
    "res <- 0",
    "while (i <n) {",
    "  (TRUE || 0 / 2 == 0)",
    "  if (TRUE || 0 / 2 == 0)",
    "    i <- (0 + (1^1)) * (i / 1)",
    "  if (0+i %% 2 == 0)",
    "    res <- res + 1",
    "  i <- i + 1",
    "}",
    sep = "\n"
  )
  opt_code <- opt_constant_folding(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "i <- 0;",
    "n <- 100",
    "res <- 0",
    "while (i <n) {",
    "  TRUE ",
    "  if (TRUE)",
    "    i <-  1 * (i / 1)",
    "  if (0+i %% 2 == 0)",
    "    res <- res + 1",
    "  i <- i + 1",
    "}",
    sep = "\n"
  ))

  env_orig <- new.env()
  eval(parse(text = code), envir = env_orig)
  env_opt <- new.env()
  eval(parse(text = opt_code), envir = env_opt)

  expect_equal(env_orig, env_opt)
})

test_that("constant fold in function", {
  code <- paste(
    "foo <- function() {",
    "  return(2 - 3 * 4 / 5 ^ 6 - 1 != 3 || FALSE)",
    "}",
    "",
    "bar <- function(x) {",
    "  return(2 - 3 * 4 / 5 ^ 6 - 1 != 3 && x)",
    "}",
    "",
    "res <- bar(TRUE)",
    sep = "\n"
  )
  opt_code <- opt_constant_folding(list(code), in_fun_call = TRUE)$codes[[1]]
  expect_equal(opt_code, paste(
    "foo <- function() {",
    "  return(TRUE)",
    "}",
    "",
    "bar <- function(x) {",
    "  return(TRUE && x)",
    "}",
    "",
    "res <- bar(TRUE)",
    sep = "\n"
  ))

  env_orig <- new.env()
  eval(parse(text = code), envir = env_orig)
  env_opt <- new.env()
  eval(parse(text = opt_code), envir = env_opt)

  env_orig <- as.list(env_orig)
  env_opt <- as.list(env_opt)

  expect_equal(names(env_orig), names(env_orig))
  expect_equal(env_orig$res, env_opt$res)
})

test_that("constant fold in function call", {
  code <- paste(
    "sum(1*7, 8/2)",
    sep = "\n"
  )
  opt_code <- opt_constant_folding(list(code), in_fun_call = TRUE)$codes[[1]]
  expect_equal(opt_code, paste(
    "sum(7, 4)",
    sep = "\n"
  ))
})

test_that("constant fold NULL function", {
  code <- paste(
    "foo <- function(n) {}",
    "res <- foo()",
    sep = "\n"
  )
  opt_code <- opt_constant_folding(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "foo <- function(n)  NULL ",
    "res <- foo()",
    sep = "\n"
  ))

  env_orig <- new.env()
  eval(parse(text = code), envir = env_orig)
  env_opt <- new.env()
  eval(parse(text = opt_code), envir = env_opt)

  env_orig <- as.list(env_orig)
  env_opt <- as.list(env_opt)

  expect_equal(names(env_orig), names(env_orig))
  expect_equal(env_orig$res, env_opt$res)
})

test_that("dont constant fold not assigned exprs", {
  code <- paste(
    "-2",
    "2",
    "NULL",
    "\"hola\"",
    sep = "\n"
  )
  opt_code <- opt_constant_folding(list(code))$codes[[1]]
  expect_equal(opt_code, code)
})

test_that("add spaces when folding {const_expr}", {
  code <- paste(
    "if(TRUE){-3}else{NULL}",
    "if(TRUE){3}else{NULL}",
    sep = "\n"
  )
  opt_code <- opt_constant_folding(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "if(TRUE){-3}else NULL ",
    "if(TRUE) 3 else NULL ",
    sep = "\n"
  ))
})

test_that("dont fold integer 'L' symbol", {
  code <- paste(
    "do_range_int <- function(x, halt_if_min = 1L, halt_if_max = -1L) {",
    "  .Call(`_hutilscpp_do_range_int`, x, halt_if_min, halt_if_max)",
    "}",
    sep = "\n"
  )
  opt_code <- opt_constant_folding(list(code))$codes[[1]]
  expect_equal(opt_code, code)
})

test_that("dont fold NA_*_", {
  code <- paste(
    "a <- { NA_character_ }",
    "a <- { NA_complex_ }",
    "a <- { NA_integer_ }",
    "a <- { NA_real_ }",
    "a <- { NA }",
    sep = "\n"
  )
  opt_code <- opt_constant_folding(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "a <-  NA_character_ ",
    "a <-  NA_complex_ ",
    "a <-  NA_integer_ ",
    "a <-  NA_real_ ",
    "a <-  NA ",
    sep = "\n"
  ))

  env_orig <- new.env()
  eval(parse(text = code), envir = env_orig)
  env_opt <- new.env()
  eval(parse(text = opt_code), envir = env_opt)

  expect_equal(env_orig, env_opt)
})

test_that("dont fold (-n)", {
  code <- paste(
    "numb <- 2",
    "(-1) ^ numb",
    sep = "\n"
  )
  opt_code <- opt_constant_folding(list(code))$codes[[1]]
  expect_equal(opt_code, code)
})

test_that("folding nested if else", {
  code <- paste(
    "if (a) {",
    "  if (b) {",
    "    1",
    "  } else {",
    "    2",
    "  }",
    "} else {",
    "  if (c) {",
    "    3",
    "  } else {",
    "    4",
    "  }",
    "}",
    sep = "\n"
  )
  opt_code <- opt_constant_folding(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "if (a) {",
    "  if (b)  1 else  2 ",
    "} else {",
    "  if (c)  3 else  4 ",
    "}",
    sep = "\n"
  ))
})

test_that("fold length 0", {
  code <- paste(
    'if (NULL=="checkbox") NULL',
    sep = "\n"
  )
  opt_code <- opt_constant_folding(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "if (logical(0)) NULL",
    sep = "\n"
  ))
})

test_that("dont fold in function call", {
  code <- paste(
    "deparse(substitute(8 * 8 * 8818))",
    sep = "\n"
  )
  opt_code <- opt_constant_folding(list(code))$codes[[1]]
  expect_equal(opt_code, code)
})

test_that("add spaces if precedence ops removed", {
  code <- paste(
    "if(cond){}else{}",
    "if(cond)(1+3)else(1+3)",
    sep = "\n"
  )
  opt_code <- opt_constant_folding(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "if(cond) NULL else NULL ",
    "if(cond) 4 else 4 ",
    sep = "\n"
  ))
})
