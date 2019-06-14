context("optimize_text")

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

test_that("optimizing text", {
  code <- "hello_world <- 'hello_world'\n"

  opt_code_no_change <-
    optimize_text(code, optimizers = list(testing_optimizer_no_edit))
  expect_equal(code, opt_code_no_change)

  opt_code_change <-
    optimize_text(code,
      optimizers = list(testing_optimizer_edit),
      iterations = 1
    )
  expect_true(!isTRUE(all.equal(code, opt_code_change)))
})
