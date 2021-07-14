context("opt_memory_alloc")

test_that("Code snippet with semicolon and nothing to optimize", {
  code <- "var1 <- 3; var2 <- var1 * 5"
  opt_code <- rco::opt_memory_alloc(list(code))$codes[[1]]
  expect_equal(opt_code, "var1 <- 3; var2 <- var1 * 5")
})

test_that("Empty code snippet", {
  code <- paste(
    "",
    sep = "\n"
  )
  opt_code <- rco::opt_memory_alloc(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "",
    sep = "\n"
  ))
})

test_that("Code snippet with assignments within loops and presence of functions in the loop conditionals", {
  code <- paste(
    "v <- NULL",
    "for(i in 1:10) {",
    "  v[i] <- i*i",
    "}",
    "fun <- function(n) {",
    "  num <- NULL",
    "  for(i in c(2010,2011)) {",
    "    num[i] <- i",
    "  }",
    "  no_fun_num <- NULL",
    "  for(i in 2010:2011) {",
    "    no_fun_num[i] <- i",
    "  }",    
    "  sum(num)",
    "  sum(no_fun_sum)",
    "}",
    sep = "\n"
  )
  opt_code <- rco::opt_memory_alloc(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "v <- vector(length = 10)",
    "for(i in 1:10) {",
    "  v[i] <- i*i",
    "}",
    "fun <- function(n) {",
    "  num <- NULL",
    "  for(i in c(2010,2011)) {",
    "    num[i] <- i",
    "  }",
    "no_fun_num <- vector(length = 2011)",
    "  for(i in 2010:2011) {",
    "    no_fun_num[i] <- i",
    "  }",
    "  sum(num)",
    "  sum(no_fun_sum)",
    "}",
    sep = "\n"
  ))
})

test_that("Code snippet showcasing different types of assignments to vectors", {
  code <- paste(
    "v = numeric()",
    "for(i in 1:5) {",
    "  v[i] <- i^2",
    "}",
    "logical() -> w",
    "for(j in 1:10) {",
    "  w[j] <- FALSE",
    "}",
    "c() -> x",
    "for(k in 1:6) {",
    "  x[k] <- k*2",
    "}",
    "y = c()",
    "for(l in seq_len(5)) {",
    "  y[l] <- l*2",
    "}",
    sep = "\n"
  )
  opt_code <- rco::opt_memory_alloc(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "v <- vector(length = 5)",
    "for(i in 1:5) {",
    "  v[i] <- i^2",
    "}",
    "w <- vector(length = 10)",
    "for(j in 1:10) {",
    "  w[j] <- FALSE",
    "}",
    "x <- vector(length = 6)",
    "for(k in 1:6) {",
    "  x[k] <- k*2",
    "}",
    "y = c()",
    "for(l in seq_len(5)) {",
    "  y[l] <- l*2",
    "}",
    sep = "\n"
  ))
})

test_that("Code snippet ", {
  code <- paste(
    "x <- NULL",
    "v <- NULL",
    "z <- NULL",
    "for(i in 1:5) {",
    "  v[i] = i*i",
    "  i -> x[i]",
    "  v[i+5] <- 2*i",
    "  z[i] <- v[i]",
    "}",
    sep = "\n"
  )
  opt_code <- rco::opt_memory_alloc(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "x <- vector(length = 5)",
    "v <- NULL",
    "z <- vector(length = 5)",
    "for(i in 1:5) {",
    "  v[i] = i*i",
    "  i -> x[i]",
    "  v[i+5] <- 2*i",
    "  z[i] <- v[i]",
    "}",
    sep = "\n"
  ))
})

test_that("memory allocation empty code", {
  code <- paste(
    "break_it <- function() {",
    "  i <<- 1",
    "}",
    "x <- c()",
    "for(i in 1:10) {",
    "  break_it()",
    "  x[i] <- i^2",
    "  print(i)",
    "}",
    sep = "\n"
  )
  opt_code <- rco::opt_memory_alloc(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "break_it <- function() {",
    "  i <<- 1",
    "}",
    "x <- c()",
    "for(i in 1:10) {",
    "  break_it()",
    "  x[i] <- i^2",
    "  print(i)",
    "}",
    sep = "\n"
  ))
})

test_that("memory allocation empty code", {
  code <- paste(
    "v <- NULL",
    "if(5 == 5) { #Comment1",
    "  if(3 == 3) {",
    "    equal <- NULL",
    "    for(i in 1:10) {",
    "      #Comment2",
    "      equal[i] <- i",
    "    }",
    "  }",
    "}",
    "really <- function() {",
    "  v <- c()",
    "  for(i in 1:10) {",
    "    v[i] <- i",
    "  }",
    "}",
    "#Comment3",
    "really()",
    "for(i in 1:10) {",
    "  v <- NULL",
    "  #Comment4",
    "  for(j in 1:5) {",
    "    v[j] <- i",
    "  }",
    "}",
    sep = "\n"
  )
  opt_code <- rco::opt_memory_alloc(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "v <- vector(length = 10)",
    "if(5 == 5) { #Comment1",
    "  if(3 == 3) {",
    "equal <- vector(length = 10)",
    "    for(i in 1:10) {",
    "      #Comment2",
    "      equal[i] <- i",
    "    }",
    "  }",
    "}",
    "really <- function() {",
    "v <- vector(length = 10)",
    "  for(i in 1:10) {",
    "    v[i] <- i",
    "  }",
    "}",
    "#Comment3",
    "really()",
    "for(i in 1:10) {",
    "v <- vector(length = 10)",
    "  #Comment4",
    "  for(j in 1:5) {",
    "    v[j] <- i",
    "  }",
    "}",
    sep = "\n"
  ))
})












