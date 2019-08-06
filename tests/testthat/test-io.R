context("io")

test_that("correctly writes and reads file", {
  tmp_file <- paste0(tempdir(), "/tmp_file.R")
  code <- "hello_world <- 'hello_world'\n"
  write_code_file(code, tmp_file)
  read_code <- read_code_file(tmp_file)
  expect_equal(code, read_code)
  file.remove(tmp_file)
})
