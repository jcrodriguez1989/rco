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
    ".subset2(mtcars, 11)",
    "#This is a comment!!",
    ".subset2(mtcars, 11)",
    ".subset2(mtcars, 11)",
    "yo <- 1",
    "yo",
    ".subset2(mtcars, c(11))",
    ".subset2(mtcars, 11)",
    sep = "\n"
  ))
})