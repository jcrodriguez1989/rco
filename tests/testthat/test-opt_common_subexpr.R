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

test_that("dont common subexpr elimination for one op or symbol", {
  code <- paste(
    "a <- 1",
    "b <- 1",
    "c <- (2)",
    "d <- (2)",
    "e <- -3",
    "f <- -3",
    sep = "\n"
  )
  opt_code <- opt_common_subexpr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "a <- 1",
    "b <- 1",
    "c <- (2)",
    "d <- (2)",
    "e <- -3",
    "f <- -3",
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
    "cs_1 <- 2 + 3",
    "c <- 1 * (cs_1)",
    "d <- 4 * (cs_1)",
    "e <- cs_1",
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

test_that("common subexpr elimination with loop 1", {
  code <- paste(
    "a <- 0",
    "b <- 1",
    "c <- 2",
    "while (a < 5) {",
    "  b <- b + (5 + 1)",
    "  c <- c + (5 + 1)",
    "  a <- a + 1",
    "}",
    sep = "\n"
  )
  opt_code <- opt_common_subexpr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "a <- 0",
    "b <- 1",
    "c <- 2",
    "while (a < 5) {",
    "  cs_1 <- (5 + 1)",
    "  b <- b + cs_1",
    "  c <- c + cs_1",
    "  a <- a + 1",
    "}",
    sep = "\n"
  ))
})

test_that("common subexpr elimination with loop 2", {
  code <- paste(
    "a <- 0",
    "b <- 1",
    "c <- 2",
    "while (a < 5) {",
    "  b <- b + (a + 1)",
    "  c <- c + (a + 1)",
    "  a <- a + 1",
    "}",
    sep = "\n"
  )
  opt_code <- opt_common_subexpr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "a <- 0",
    "b <- 1",
    "c <- 2",
    "while (a < 5) {",
    "  cs_1 <- a + 1",
    "  b <- b + (cs_1)",
    "  c <- c + (cs_1)",
    "  a <- cs_1",
    "}",
    sep = "\n"
  ))
})

test_that("common subexpr elimination with in/out loop", {
  code <- paste(
    "a <- 0",
    "b <- 1",
    "c <- 2",
    "d <- a + 1",
    "while (a < 5) {",
    "  b <- b + (a + 1)",
    "  c <- c + (a + 1)",
    "  a <- a + 1",
    "}",
    sep = "\n"
  )
  opt_code <- opt_common_subexpr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "a <- 0",
    "b <- 1",
    "c <- 2",
    "d <- a + 1",
    "while (a < 5) {",
    "  cs_1 <- a + 1",
    "  b <- b + (cs_1)",
    "  c <- c + (cs_1)",
    "  a <- cs_1",
    "}",
    sep = "\n"
  ))
})

test_that("common subexpr elimination same line", {
  code <- paste(
    "a <- 1; b <- a + 2; a <- 3; c <- a + 2",
    "a <- 1; b <- a + 2; c <- a + 2; a <- 3; d <- a + 2",
    "a <- 1; b <- a + 2; c <- a + 2; a <- 3; d <- a + 2; e <- a + 2",
    sep = "\n"
  )
  opt_code <- opt_common_subexpr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "a <- 1; b <- a + 2; a <- 3; c <- a + 2",
    "a <- 1; cs_1 <- a + 2",
    "b <- cs_1; c <- cs_1; a <- 3; d <- a + 2",
    "a <- 1; cs_2 <- a + 2",
    "b <- cs_2; c <- cs_2; a <- 3; cs_3 <- a + 2",
    "d <- cs_3; e <- cs_3",
    sep = "\n"
  ))
})

test_that("CSE with function call", {
  code <- paste(
    "a <- 1 * 2",
    "rm(list = ls())",
    "b <- 1 * 2",
    "c <- 1 * 2",
    "foo <- function(x) {",
    "  a <- 1 * 2",
    "  rm(list = ls())",
    "  b <- 1 * 2",
    "  c <- 1 * 2",
    "}",
    "d <- 1 * 2",
    sep = "\n"
  )
  opt_code <- opt_common_subexpr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "a <- 1 * 2",
    "rm(list = ls())",
    "cs_1 <- 1 * 2",
    "b <- cs_1",
    "c <- cs_1",
    "foo <- function(x) {",
    "  a <- 1 * 2",
    "  rm(list = ls())",
    "  cs_1 <- 1 * 2",
    "  b <- cs_1",
    "  c <- cs_1",
    "}",
    "d <- cs_1",
    sep = "\n"
  ))
})

test_that("CSE function in function", {
  code <- paste(
    "foo <- function() {",
    "  RowNA <- apply(data[, Var], 1, function(x){any(is.na(x))})",
    "}",
    sep = "\n"
  )
  opt_code <- opt_common_subexpr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "foo <- function() {",
    "  RowNA <- apply(data[, Var], 1, function(x){any(is.na(x))})",
    "}",
    sep = "\n"
  ))
})

test_that("CSE in function call", {
  code <- paste(
    "foo(1, x = 1 + 0, y = 1 + 0 + 2)",
    "bar(foo(1, x = 1 + 0, y = 1 + 0 + 2))",
    sep = "\n"
  )
  opt_code <- opt_common_subexpr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "cs_1 <- 1 + 0",
    "foo(1, x = cs_1, y = cs_1 + 2)",
    "cs_2 <- 1 + 0",
    "bar(foo(1, x = cs_2, y = cs_2 + 2))",
    sep = "\n"
  ))
})

test_that("CSE in function call with pkg::", {
  code <- paste(
    "foo::foo(1, x = 1 + 0, y = 1 + 0 + 2)",
    "foo::bar(foo(1, x = 1 + 0, y = 1 + 0 + 2))",
    sep = "\n"
  )
  opt_code <- opt_common_subexpr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "cs_1 <- 1 + 0",
    "foo::foo(1, x = cs_1, y = cs_1 + 2)",
    "cs_2 <- 1 + 0",
    "foo::bar(foo(1, x = cs_2, y = cs_2 + 2))",
    sep = "\n"
  ))
})

test_that("CSE in assigned fun", {
  code <- paste(
    "a <- b <- c(0 - 1 * 2, 0 + 1 * 2)",
    sep = "\n"
  )
  opt_code <- opt_common_subexpr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "cs_1 <- 1 * 2",
    "a <- b <- c(0 - cs_1, 0 + cs_1)",
    sep = "\n"
  ))
})
