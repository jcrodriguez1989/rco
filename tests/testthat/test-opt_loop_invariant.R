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
    "if (TRUE) {",
    "  x <- 3",
    "} ",
    "while (TRUE) {",
    "}",
    sep = "\n"
  ))
})

test_that("simple loop invariant two exprs", {
  code <- paste(
    "while (TRUE) {",
    "  x <- 3",
    "  y <- 4",
    "}",
    sep = "\n"
  )
  opt_code <- opt_loop_invariant(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "if (TRUE) {",
    "  x <- 3",
    "  y <- 4",
    "} ",
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
    "while (TRUE) {",
    "  if (length(1:10) > 0) {",
    "    x <- 3",
    "  } ",
    "  for(i in 1:10) {",
    "  }",
    "}",
    sep = "\n"
  ))
})

test_that("double loop invariant dont skip one", {
  code <- paste(
    "while (TRUE) {",
    "  while (FALSE) {",
    "    x <- 3",
    "  }",
    "}",
    sep = "\n"
  )
  opt_code <- opt_loop_invariant(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "while (TRUE) {",
    "  if (FALSE) {",
    "    x <- 3",
    "  } ",
    "  while (FALSE) {",
    "  }",
    "}",
    sep = "\n"
  ))
})

test_that("double loop invariant in outer", {
  code <- paste(
    "for (i in 1:100) {",
    "  c <- 1",
    "  for (j in 1:100) {",
    "    c <- c + 1",
    "  }",
    "}",
    sep = "\n"
  )
  opt_code <- opt_loop_invariant(list(code))$codes[[1]]
  expect_equal(opt_code, code)
})

test_that("double loop invariant in outer 2", {
  code <- paste(
    "for (i in 1:100) {",
    "  c <- 1",
    "  for (j in 1:100) {",
    "    c <- c + 1",
    "    d <- c",
    "  }",
    "}",
    sep = "\n"
  )
  opt_code <- opt_loop_invariant(list(code))$codes[[1]]
  expect_equal(opt_code, code)
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
    "  if (length(1:10) > 0) {",
    "    x <- j + 1",
    "  } ",
    "  for(i in 1:10) {",
    "  }",
    "}",
    sep = "\n"
  ))

  env_orig <- new.env()
  eval(parse(text = code), envir = env_orig)
  env_opt <- new.env()
  eval(parse(text = opt_code), envir = env_opt)

  expect_equal(env_orig, env_opt)
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
  expect_equal(opt_code, code)
})

test_that("abort loop invariant if next/break/return", {
  code <- paste(
    "while (i < 10) {",
    "  break",
    "  x <- 8818",
    "}",
    "while (i < 10) {",
    "  next",
    "  x <- 8818",
    "}",
    "for (i in 1:10) {",
    "  break",
    "  x <- 8818",
    "}",
    "for (i in 1:10) {",
    "  next",
    "  x <- 8818",
    "}",
    sep = "\n"
  )
  opt_code <- opt_loop_invariant(list(code))$codes[[1]]
  expect_equal(opt_code, code)
})

test_that("no error on empty loops", {
  code <- paste(
    "while (i < 8818) {}",
    "while (i < 8818) NULL",
    "for (i in 1:8818) {}",
    "for (i in 1:8818) NULL",
    sep = "\n"
  )
  opt_code <- opt_loop_invariant(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "while (i < 8818) {}",
    "if (i < 8818) {",
    "NULL",
    "}",
    "for (i in 1:8818) {}",
    "if (length(1:8818) > 0) {",
    "NULL}",
    "",
    sep = "\n"
  ))
})
