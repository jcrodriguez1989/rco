context("optimize_folder")

test_that("optimize non recursively in a folder", {
  tmp_dir <- tempdir()
  base_dir <- paste0(tmp_dir, "/folder1/")
  sub_dir <- paste0(base_dir, "subdir/")
  unlink(base_dir, force = TRUE, recursive = TRUE)
  dir.create(base_dir, showWarnings = FALSE)
  dir.create(sub_dir, showWarnings = FALSE)
  file_opt <- paste0(base_dir, "file_opt.R")
  file_unopt <- paste0(base_dir, "file_unopt.R")
  subd_file_opt <- paste0(sub_dir, "file_opt.R")
  subd_file_unopt <- paste0(sub_dir, "file_unopt.R")
  opt_code <- "var1 <- 3; var2 <- 15"
  unopt_code <- "var1 <- 3; var2 <- var1 * 5"

  # create one optimizable and non optimizable file at base_dir.
  # And the same at sub_dir
  write_code_file(opt_code, file_opt)
  expect_true(file.copy(file_opt, subd_file_opt))
  write_code_file(unopt_code, file_unopt)
  expect_true(file.copy(file_unopt, subd_file_unopt))

  optimize_folder(base_dir, recursive = FALSE)

  # expect that there is only one new file, and it is only in base_dir
  expect_true(length(dir(base_dir)) == 4)
  expect_true(length(dir(sub_dir)) == 2)

  # expect that the unopt file was optimized
  expect_equal(
    read_code_file(paste0(base_dir, "optimized_", basename(file_unopt))),
    read_code_file(file_opt)
  )
})

test_that("optimize recursively in a folder", {
  tmp_dir <- tempdir()
  base_dir <- paste0(tmp_dir, "/folder2/")
  sub_dir <- paste0(base_dir, "subdir/")
  unlink(base_dir, force = TRUE, recursive = TRUE)
  dir.create(base_dir, showWarnings = FALSE)
  dir.create(sub_dir, showWarnings = FALSE)
  file_opt <- paste0(base_dir, "file_opt.R")
  file_unopt <- paste0(base_dir, "file_unopt.R")
  subd_file_opt <- paste0(sub_dir, "file_opt.R")
  subd_file_unopt <- paste0(sub_dir, "file_unopt.R")
  opt_code <- "var1 <- 3; var2 <- 15"
  unopt_code <- "var1 <- 3; var2 <- var1 * 5"

  # create one optimizable and non optimizable file at base_dir.
  # And the same at sub_dir
  write_code_file(opt_code, file_opt)
  expect_true(file.copy(file_opt, subd_file_opt))
  write_code_file(unopt_code, file_unopt)
  expect_true(file.copy(file_unopt, subd_file_unopt))

  optimize_folder(base_dir, recursive = TRUE)

  # expect that there is one new file in base_dir and in sub_dir
  expect_true(length(dir(base_dir)) == 4)
  expect_true(length(dir(sub_dir)) == 3)

  # expect that the unopt files were optimized
  expect_equal(
    read_code_file(paste0(base_dir, "optimized_", basename(file_unopt))),
    read_code_file(file_opt)
  )
  expect_equal(
    read_code_file(paste0(sub_dir, "optimized_", basename(file_unopt))),
    read_code_file(file_opt)
  )
})

test_that("error on non-exiting folder", {
  expect_error(optimize_folder("non_existing_folder"))
})

test_that("optimize empty folder", {
  tmp_dir <- tempdir()
  base_dir <- paste0(tmp_dir, "/folder3/")
  unlink(base_dir, force = TRUE, recursive = TRUE)
  dir.create(base_dir, showWarnings = FALSE)

  expect_null(optimize_folder(base_dir))
})
