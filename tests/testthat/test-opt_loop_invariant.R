context("opt_loop_invariant")

test_that("loop invariant empty code", {
  code <- paste(
    "",
    sep = "\n"
  )
  opt_code <- opt_loop_invariant(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "",
    sep = "\n"
  ))
})

test_that("simple loop invariant", {
  code <- paste(
    "while (TRUE) {",
    "  x <- 3",
    "}",
    sep = "\n"
  )
  opt_code <- opt_loop_invariant(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "x <- 3",
    "while (TRUE) {",
    "}",
    sep = "\n"
  ))
})

test_that("double loop invariant", {
  code <- paste(
    "while (TRUE) {",
    "  for(i in 1:10) {",
    "    x <- 3",
    "  }",
    "}",
    sep = "\n"
  )
  opt_code <- opt_loop_invariant(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "x <- 3",
    "while (TRUE) {",
    "  for(i in 1:10) {",
    "  }",
    "}",
    sep = "\n"
  ))
})

test_that("double loop invariant", {
  code <- paste(
    "while (TRUE) {",
    "  for(i in 1:10) {",
    "    x <- 3",
    "  }",
    "}",
    sep = "\n"
  )
  opt_code <- opt_loop_invariant(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "x <- 3",
    "while (TRUE) {",
    "  for(i in 1:10) {",
    "  }",
    "}",
    sep = "\n"
  ))
})

test_that("simple loop invariant", {
  code <- paste(
    "for(j in 1:20) {",
    "  for(i in 1:10) {",
    "    x <- j + 1",
    "  }",
    "}",
    sep = "\n"
  )
  opt_code <- opt_loop_invariant(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "for(j in 1:20) {",
    "  x <- j + 1",
    "  for(i in 1:10) {",
    "  }",
    "}",
    sep = "\n"
  ))
})

test_that("dont propagate", {
  code <- paste(
    "for(i in 1:20) {",
    "  x <- i + 1",
    "}",
    "",
    "i <- 0",
    "while (i < 20) {",
    "  x <- x + i",
    "  i <- i + 1",
    "}",
    "",
    "i <- 0",
    "repeat{",
    "  x <- x + i",
    "  i <- i + 1",
    "  if (i >= 20) break",
    "}",
    sep = "\n"
  )
  opt_code <- opt_loop_invariant(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "for(i in 1:20) {",
    "  x <- i + 1",
    "}",
    "",
    "i <- 0",
    "while (i < 20) {",
    "  x <- x + i",
    "  i <- i + 1",
    "}",
    "",
    "i <- 0",
    "repeat{",
    "  x <- x + i",
    "  i <- i + 1",
    "  if (i >= 20) break",
    "}",
    sep = "\n"
  ))
})
