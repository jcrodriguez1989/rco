context("opt_value_extractor")

test_that("Replace function call with system call for all general cases", {
  code <-  paste("mtcars[32 , 11]",
                 "#This is a comment!!",
                 "mtcars [[11]][32]",
                 "mtcars$carb[32]",
                 "yo <- 1",
                 "yo",
                 "mtcars[[c(11, 32)]]",
                 ".subset2(mtcars, 11)[32]",
                 sep = "\n")
  opt_code <- opt_value_extractor(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    ".subset2(mtcars, 11)[32]",
    "#This is a comment!!",
    ".subset2(mtcars, 11)[32]",
    ".subset2(mtcars, 11)[32]",
    "yo <- 1",
    "yo",
    ".subset2(mtcars, 11)[32]",
    ".subset2(mtcars, 11)[32]",
    sep = "\n"
  ))
})