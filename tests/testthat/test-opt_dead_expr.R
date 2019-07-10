context("opt_dead_expr")

test_that("dead expr empty code", {
  code <- paste(
    "",
    sep = "\n"
  )
  opt_code <- opt_dead_expr(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "",
    sep = "\n"
  ))
})
