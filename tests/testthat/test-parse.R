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
  parsed_data <- parse_flat_data(code)
  expect_equal(dim(parsed_data), c(34, 13))
  deparsed_data <- deparse_flat_data(parsed_data)
  expect_equal(code, deparsed_data)
})
