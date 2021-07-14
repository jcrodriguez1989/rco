context("opt_column_extractor")

test_that("Replace function call with system call for all general cases", {
  code <-  paste("mtcars[ , 11]",
                 "#This is a comment!!",
                 "mtcars [[11]]",
                 "mtcars$carb",
                 "yo <- 1",
                 "yo",
                 "mtcars[[c(11)]]",
                 ".subset2(mtcars, 11)",
                 sep = "\n")
  opt_code <- opt_column_extractor(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    ".subset2 (mtcars, 11)",
    "#This is a comment!!",
    ".subset2 (mtcars, 11)",
    ".subset2 (mtcars, which (colnames (mtcars) == \"carb\"))",
    "yo <- 1",
    "yo",
    ".subset2 (mtcars, c(11))",
    ".subset2(mtcars, 11)",
    sep = "\n"
  ))
})

test_that("Testing on user-created datasets", {
  code <- paste("points <- data.frame(x = rnorm(100), y = rnorm(100))",
                "points[ ,2]",
                "rnorm(100)",
                "#This is a comment!!",
                "points [[2]]",
                "points$y",
                "yo <- 1",
                "yo",
                "points[[c(2)]]",
                ".subset2(points, 2)",
                sep = "\n")
  opt_code <- opt_column_extractor(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "points <- data.frame(x = rnorm(100), y = rnorm(100))",
    ".subset2 (points, 2)",
    "rnorm(100)",
    "#This is a comment!!",
    ".subset2 (points, 2)",
    ".subset2 (points, which (colnames (points) == \"y\"))",
    "yo <- 1",
    "yo",
    ".subset2 (points, c(2))",
    ".subset2(points, 2)",
    sep = "\n"
  ))
})

test_that("A code snippet consisting of only a function", {
  code <- paste(
    "foo <- function(x, n){",
    "print(x)",
    "rnorm(100)",
    "x[ ,n]",
    "x$n",
    "y <- list(A = 1, C = 3)",
    "y[2]",
    "}",
    sep = "\n"
  )
  opt_code <- opt_column_extractor(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "foo <- function(x, n){",
    "print(x)",
    "rnorm(100)",
    ".subset2 (x, n)",
    ".subset2 (x, which (colnames (x) == \"n\"))",
    "y <- list(A = 1, C = 3)",
    ".subset2 (y, 2)",
    "}",
    sep = "\n"
  ))
})

test_that("Code snippet containing normal and function code", {
  code <- paste(
    "custom <- matrix(1:9, nrow = 3, ncol = 3)",
    "#I am Mr.Comment, y'all!!",
    "mtcars[ ,5]",
    "foo <- function(x, n){",
    "x[ ,n]",
    "}",
    "custom[ ,2]",
    "milky_moo <- function(obj){",
    "print(\"obj\")",
    "}",
    "random_variable",
    sep = "\n"
  )
  opt_code <- opt_column_extractor(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "custom <- matrix(1:9, nrow = 3, ncol = 3)",
    "#I am Mr.Comment, y'all!!",
    ".subset2 (mtcars, 5)",
    "foo <- function(x, n){",
    ".subset2 (x, n)",
    "}",
    ".subset2 (custom, 2)",
    "milky_moo <- function(obj){",
    "print(\"obj\")",
    "}",
    "random_variable",
    sep = "\n"
  ))
})

