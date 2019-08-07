context("parse")

test_that("correctly parse and deparse text", {
  code <- paste(
    "i<-0",
    "for (j in 1:3) {",
    "  i <- i^i",
    "}",
    "i",
    sep = "\n"
  )
  parsed_data <- parse_text(code)
  deparsed_data <- deparse_data(parsed_data)
  expect_equal(code, deparsed_data)
})

test_that("keep new lines", {
  code <- paste(
    "",
    "",
    "i <- 0",
    "",
    "j <- 0",
    sep = "\n"
  )
  parsed_data <- parse_text(code)
  deparsed_data <- deparse_data(parsed_data)
  expect_equal(code, deparsed_data)
})
