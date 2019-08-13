context("opt_dead_expr")

test_that("dead expr empty code", {
  code <- paste(
    "",
    sep = "\n"
  )
  opt_code <- opt_dead_expr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "",
    sep = "\n"
  ))
})

test_that("dont eliminate DE in parent env", {
  code <- paste(
    "8 + 8 + 1918",
    sep = "\n"
  )
  opt_code <- opt_dead_expr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "8 + 8 + 1918",
    sep = "\n"
  ))
})

test_that("eliminate DE in fun", {
  code <- paste(
    "8 + 8 + 1918",
    "foo <- function() 8818",
    "bar <- function() {",
    "  8818",
    "  8818",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_expr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "8 + 8 + 1918",
    "foo <- function() 8818",
    "bar <- function() {",
    "  8818",
    "}",
    sep = "\n"
  ))
})

test_that("eliminate DE in fun", {
  code <- paste(
    "8 + 8 + 1918",
    "foo <- function() 8818",
    "bar <- function(x) {",
    "  x + 8818",
    "  8818",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_expr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "8 + 8 + 1918",
    "foo <- function() 8818",
    "bar <- function(x) {",
    "  8818",
    "}",
    sep = "\n"
  ))
})

test_that("eliminate DE in fun with ';", {
  code <- paste(
    "bar <- function(x) { 8818; 8818; 8818; 8818 }",
    sep = "\n"
  )
  opt_code <- opt_dead_expr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "bar <- function(x) { ; ; ; 8818 }",
    sep = "\n"
  ))
})

test_that("eliminate DE in loop", {
  code <- paste(
    "bar <- function(x) {",
    "  while (TRUE) {",
    "    x + 8818",
    "  }",
    "  while (TRUE) x + 8818",
    "  for (i in 1:10) {",
    "    x + 8818",
    "  }",
    "  for (i in 1:10) x + 8818",
    "  repeat {",
    "    x + 8818",
    "  }",
    "  repeat x + 8818",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_expr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "bar <- function(x) {",
    "  while (TRUE) {",
    "  }",
    "  while (TRUE) {}",
    "  for (i in 1:10) {",
    "  }",
    "  for (i in 1:10) {}",
    "  repeat {",
    "  }",
    "  repeat {}",
    "}",
    sep = "\n"
  ))
})

test_that("dont eliminate DE in if/else", {
  code <- paste(
    "bar <- function(x) {",
    "  if (x == 0) {",
    "    x + 8818",
    "  } else if (x == 1) {",
    "    x + 8818",
    "  } else {",
    "    x + 8818",
    "  }",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_expr(list(code))$codes[[1]]
  expect_equal(opt_code, code)
})

test_that("eliminate DE in if/else", {
  code <- paste(
    "bar <- function(x) {",
    "  if (x == 0) {",
    "    x + 8818",
    "  } else if (x == 1) {",
    "    x + 8818",
    "  } else {",
    "    x + 8818",
    "  }",
    "  x + 8818",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_expr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "bar <- function(x) {",
    "  if (x == 0) {",
    "  } else if (x == 1) {",
    "  } else {",
    "  }",
    "  x + 8818",
    "}",
    sep = "\n"
  ))
})

test_that("dont eliminate assigns", {
  code <- paste(
    "bar <- function(x) {",
    "  x <- 3",
    "  x <- x + 3",
    "  if (x == 0) {",
    "    x <- 3",
    "    x + 8818",
    "  } else if (x == 1) {",
    "    x <- 3",
    "    x + 8818",
    "  } else {",
    "    x <- 3",
    "    x + 8818",
    "  }",
    "  x <- 3",
    "  x + 8818",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_expr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "bar <- function(x) {",
    "  x <- 3",
    "  x <- x + 3",
    "  if (x == 0) {",
    "    x <- 3",
    "  } else if (x == 1) {",
    "    x <- 3",
    "  } else {",
    "    x <- 3",
    "  }",
    "  x <- 3",
    "  x + 8818",
    "}",
    sep = "\n"
  ))
})

test_that("dont eliminate part of exprs", {
  code <- paste(
    "bar <- function(x) {",
    "  tp <- ip[startsWith(ip, token)]",
    "  completions <- lapply(tp, function(package) NULL)",
    "  x + 8818",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_expr(list(code))$codes[[1]]
  expect_equal(opt_code, code)
})

test_that("dont eliminate empty `if` or loop", {
  code <- paste(
    "foo <- function() {",
    "  if (cond) NULL",
    "  if (cond) NULL else NULL",
    "  while (cond) NULL",
    "  for (i in cond) NULL",
    "  8818",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_expr(list(code))$codes[[1]]
  expect_equal(opt_code, code)
})

test_that("eliminate empty in one side of `ifelse`", {
  code <- paste(
    "foo <- function() {",
    "  if (cond) x else NULL",
    "  if (cond) NULL else x",
    "  8818",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_expr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "foo <- function() {",
    "  if (cond) {} else NULL",
    "  if (cond) NULL else {}",
    "  8818",
    "}",
    sep = "\n"
  ))
})
