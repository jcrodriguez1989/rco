context("opt_dead_code")

test_that("dead code eliminate for command interruption", {
  code <- paste(
    "x <- 0",
    "while (x < 100) {",
    "  if (x < 5) {",
    "    x <- x + 1",
    "    next",
    "    some_dead_code()",
    "    some_dead_code <- x",
    "  } else {",
    "    x <- x + 2",
    "    break",
    "    some_more_dead_code()",
    "    some_more_dead_code <- x",
    "  }",
    "}",
    "",
    "foo <- function() {",
    "  return(8*8*1918)",
    "  some_extra_dead_code()",
    "  some_extra_dead_code <- 8818",
    "}",
    sep = "\n")
  opt_code <- opt_dead_code(list(code))$codes[[1]]; cat(opt_code)
  expect_equal(opt_code, paste(
    "x <- 0",
    "while (x < 100) {",
    "  if (x < 5) {",
    "    x <- x + 1",
    "    next",
    "  } else {",
    "    x <- x + 2",
    "    break",
    "  }",
    "}",
    "",
    "foo <- function() {",
    "  return(8*8*1918)",
    "}",
    sep = "\n"
  ))
})
