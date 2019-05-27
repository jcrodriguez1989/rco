context("optimize_files")

# optimizers for testing
testing_optimizer <- function(codes, edit) {
  optim_codes <- codes
  if (edit) {
    optim_codes[[1]] <- c("hw <- 'hw'", "", optim_codes[[1]])
  }
  return(list(codes = optim_codes))
}

testing_optimizer_no_edit <- function(codes) {
  testing_optimizer(codes, edit = FALSE)
}

testing_optimizer_edit <- function(codes) {
  testing_optimizer(codes, edit = TRUE)
}

test_that("optimizer dont change files w/overwrite", {
  tmp_file1 <- paste0(tempdir(), "/tmp_file1.R")
  tmp_file1_opt <- paste0(tempdir(), "/optimized_tmp_file1.R")
  tmp_file1_copy <- paste0(tempdir(), "/tmp_file1_copy.R")
  code <- "hello_world <- 'hello_world'\n"
  write_code_file(code, tmp_file1)
  expect_true(file.copy(tmp_file1, tmp_file1_copy))
  optimize_files(tmp_file1, list(testing_optimizer_no_edit), overwrite = TRUE)

  # tests that the file optimized file didnt change (as it could not be
  # optimized)
  expect_equal(read_code_file(tmp_file1),
               read_code_file(tmp_file1_copy))

  # test that no "optimized_" file was created
  expect_true(!file.exists(tmp_file1_opt))

  file.remove(tmp_file1, tmp_file1_copy)
})

test_that("optimizer dont change files wo/overwrite", {
  tmp_file1 <- paste0(tempdir(), "/tmp_file1.R")
  tmp_file1_opt <- paste0(tempdir(), "/optimized_tmp_file1.R")
  tmp_file1_copy <- paste0(tempdir(), "/tmp_file1_copy.R")
  code <- "hello_world <- 'hello_world'\n"
  write_code_file(code, tmp_file1)
  expect_true(file.copy(tmp_file1, tmp_file1_copy))
  optimize_files(tmp_file1, list(testing_optimizer_no_edit), overwrite = FALSE)

  # tests that the file optimized file didnt change
  expect_equal(read_code_file(tmp_file1),
               read_code_file(tmp_file1_copy))

  # test that no "optimized_" file was created (as it could not be optimized)
  expect_true(!file.exists(tmp_file1_opt))

  file.remove(tmp_file1, tmp_file1_copy)
})

test_that("optimizer change files w/overwrite", {
  tmp_file1 <- paste0(tempdir(), "/tmp_file1.R")
  tmp_file1_opt <- paste0(tempdir(), "/optimized_tmp_file1.R")
  tmp_file1_copy <- paste0(tempdir(), "/tmp_file1_copy.R")
  tmp_file1_manual_opt <- paste0(tempdir(), "/tmp_file1_manual_opt.R")
  code <- "hello_world <- 'hello_world'\n"
  write_code_file(code, tmp_file1)
  write_code_file(c("hw <- 'hw'", "", code), tmp_file1_manual_opt)
  expect_true(file.copy(tmp_file1, tmp_file1_copy))
  optimize_files(tmp_file1, list(testing_optimizer_edit), overwrite = TRUE)

  # tests that the file was optimized
  expect_equal(read_code_file(tmp_file1),
               read_code_file(tmp_file1_manual_opt))
  # and overwritten
  expect_true(!isTRUE(all.equal(
    read_code_file(tmp_file1),
    read_code_file(tmp_file1_copy)
  )))

  # test that no "optimized_" file was created (as it had to be overwritten)
  expect_true(!file.exists(tmp_file1_opt))

  file.remove(tmp_file1, tmp_file1_copy, tmp_file1_manual_opt)
})

test_that("optimizer change files wo/overwrite", {
  tmp_file1 <- paste0(tempdir(), "/tmp_file1.R")
  tmp_file1_opt <- paste0(tempdir(), "/optimized_tmp_file1.R")
  tmp_file1_copy <- paste0(tempdir(), "/tmp_file1_copy.R")
  tmp_file1_manual_opt <- paste0(tempdir(), "/tmp_file1_manual_opt.R")
  code <- "hello_world <- 'hello_world'\n"
  write_code_file(code, tmp_file1)
  write_code_file(c("hw <- 'hw'", "", code), tmp_file1_manual_opt)
  expect_true(file.copy(tmp_file1, tmp_file1_copy))
  optimize_files(tmp_file1, list(testing_optimizer_edit), overwrite = FALSE)

  # tests that the file was optimized
  expect_equal(read_code_file(tmp_file1_opt),
               read_code_file(tmp_file1_manual_opt))
  # and not overwritten
  expect_true(isTRUE(all.equal(
    read_code_file(tmp_file1),
    read_code_file(tmp_file1_copy)
  )))
  expect_true(!isTRUE(all.equal(
    read_code_file(tmp_file1),
    read_code_file(tmp_file1_opt)
  )))

  file.remove(tmp_file1, tmp_file1_opt, tmp_file1_copy, tmp_file1_manual_opt)
})
