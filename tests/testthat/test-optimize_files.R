context("optimize_files")

# optimizers for testing
testing_optimizer <- function(codes, edit) {
  optim_codes <- codes
  if (edit) {
    optim_codes[[1]] <- c("IACC <- 'IACC'", "", optim_codes[[1]])
  }
  list(codes = optim_codes)
}

testing_optimizer_no_edit <- function(codes) {
  testing_optimizer(codes, edit = FALSE)
}

testing_optimizer_edit <- function(codes) {
  testing_optimizer(codes, edit = TRUE)
}

test_that("optimizer dont change files w/overwrite", {
  file <- paste0(tempdir(), "/file.R")
  file_opt <- paste0(tempdir(), "/optimized_file.R")
  file_copy <- paste0(tempdir(), "/file_copy.R")
  code <- "InstitutoACC <- 'InstitutoACC'\n"

  write_code_file(code, file)
  expect_true(file.copy(file, file_copy))
  optimize_files(file, list(testing_optimizer_no_edit), overwrite = TRUE)

  # tests that the file optimized file didnt change (as it could not be
  # optimized)
  expect_equal(read_code_file(file), read_code_file(file_copy))

  # test that no "optimized_" file was created
  expect_true(!file.exists(file_opt))

  file.remove(file, file_copy)
})

test_that("optimizer dont change files wo/overwrite", {
  file <- paste0(tempdir(), "/file.R")
  file_opt <- paste0(tempdir(), "/optimized_file.R")
  file_copy <- paste0(tempdir(), "/file_copy.R")
  code <- "InstitutoACC <- 'InstitutoACC'\n"

  write_code_file(code, file)
  expect_true(file.copy(file, file_copy))
  optimize_files(file, list(testing_optimizer_no_edit), overwrite = FALSE)

  # tests that the file optimized file didnt change
  expect_equal(read_code_file(file), read_code_file(file_copy))

  # test that no "optimized_" file was created (as it could not be optimized)
  expect_true(!file.exists(file_opt))

  file.remove(file, file_copy)
})

test_that("optimizer change files w/overwrite", {
  file <- paste0(tempdir(), "/file.R")
  file_opt <- paste0(tempdir(), "/optimized_file.R")
  file_copy <- paste0(tempdir(), "/file_copy.R")
  file_manual_opt <- paste0(tempdir(), "/file_manual_opt.R")
  code <- "InstitutoACC <- 'InstitutoACC'\n"

  write_code_file(code, file)
  write_code_file(c("IACC <- 'IACC'", "", code), file_manual_opt)
  expect_true(file.copy(file, file_copy))

  optimize_files(file, list(testing_optimizer_edit),
    overwrite = TRUE,
    iterations = 1
  )

  # tests that the file was optimized
  expect_equal(read_code_file(file), read_code_file(file_manual_opt))

  # and overwritten
  expect_true(!isTRUE(all.equal(
    read_code_file(file),
    read_code_file(file_copy)
  )))

  # test that no "optimized_" file was created (as it had to be overwritten)
  expect_true(!file.exists(file_opt))

  file.remove(file, file_copy, file_manual_opt)
})

test_that("optimizer change files wo/overwrite", {
  file <- paste0(tempdir(), "/file.R")
  file_opt <- paste0(tempdir(), "/optimized_file.R")
  file_copy <- paste0(tempdir(), "/file_copy.R")
  file_manual_opt <- paste0(tempdir(), "/file_manual_opt.R")
  code <- "InstitutoACC <- 'InstitutoACC'\n"

  write_code_file(code, file)
  write_code_file(c("IACC <- 'IACC'", "", code), file_manual_opt)
  expect_true(file.copy(file, file_copy))
  optimize_files(file, list(testing_optimizer_edit),
    overwrite = FALSE,
    iterations = 1
  )

  # tests that the file was optimized
  expect_equal(read_code_file(file_opt), read_code_file(file_manual_opt))

  # and not overwritten
  expect_true(isTRUE(all.equal(
    read_code_file(file),
    read_code_file(file_copy)
  )))
  expect_true(isTRUE(all.equal(
    read_code_file(file_opt),
    read_code_file(file_manual_opt)
  )))

  file.remove(file, file_opt, file_copy, file_manual_opt)
})
