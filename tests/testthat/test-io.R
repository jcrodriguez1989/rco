context("io")

test_that("correctly writes and reads file", {
  tmp_file1 <- paste0(tempdir(), "/tmp_file1.R")
  code <- "hello_world <- 'hello_world'\n"
  write_code_file(code, tmp_file1)
  read_code <- read_code_file(tmp_file1)
  expect_equal(
    code,
    paste(read_code, collapse = "\n")
  )
})
