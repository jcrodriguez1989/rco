context("opt_dead_code")

test_that("dead code eliminate for command interruption", {
  code <- paste(
    "x <- 0",
    "while (x < 100) {",
    "  if (x < 5) {",
    "    x <- x + 1",
    "    next",
    "    dead_code()",
    "    dead_code()",
    "  } else {",
    "    x <- x + 2",
    "    break",
    "    dead_code()",
    "    dead_code()",
    "  }",
    "}",
    "",
    "foo <- function() {",
    "  return(8*8*1918)",
    "  dead_code()",
    "  dead_code()",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_code(list(code))$codes[[1]]
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

  env_orig <- new.env()
  eval(parse(text = code), envir = env_orig)
  env_opt <- new.env()
  eval(parse(text = opt_code), envir = env_opt)

  expect_equal(env_orig, env_orig)
})

test_that("dead code eliminate after `next`", {
  code <- paste(
    "while (TRUE) {",
    "  x <- 1",
    "  if (x == 1) {",
    "    next",
    "    dead_code()",
    "    dead_code()",
    "  } else {",
    "    next",
    "    dead_code()",
    "    dead_code()",
    "  }",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_code(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "while (TRUE) {",
    "  x <- 1",
    "  if (x == 1) {",
    "    next",
    "  } else {",
    "    next",
    "  }",
    "}",
    sep = "\n"
  ))
})

test_that("dead code eliminate after `break`", {
  code <- paste(
    "while (TRUE) {",
    "  x <- 1",
    "  if (x == 1) {",
    "    break",
    "    dead_code()",
    "    dead_code()",
    "  } else {",
    "    break",
    "    dead_code()",
    "    dead_code()",
    "  }",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_code(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "while (TRUE) {",
    "  x <- 1",
    "  if (x == 1) {",
    "    break",
    "  } else {",
    "    break",
    "  }",
    "}",
    sep = "\n"
  ))

  env_orig <- new.env()
  eval(parse(text = code), envir = env_orig)
  env_opt <- new.env()
  eval(parse(text = opt_code), envir = env_opt)

  expect_equal(env_orig, env_orig)
})

test_that("dead code eliminate after `return`", {
  code <- paste(
    "function(x) {",
    "  if (x == 1) {",
    "    return(x + 1)",
    "    dead_code()",
    "    dead_code()",
    "  } else {",
    "    return(x + 2)",
    "    dead_code()",
    "    dead_code()",
    "  }",
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_code(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "function(x) {",
    "  if (x == 1) {",
    "    return(x + 1)",
    "  } else {",
    "    return(x + 2)",
    "  }",
    "}",
    sep = "\n"
  ))
})

test_that("eliminate FALSE while", {
  code <- paste(
    "x <- 3",
    "while (TRUE) {",
    "  not_dead_code()",
    "  not_dead_code()",
    "}",
    "",
    "while (!TRUE) {",
    "  dead_code()",
    "  dead_code()",
    "}",
    "",
    "while (FALSE) {",
    "  dead_code()",
    "  dead_code()",
    "}",
    "",
    sep = "\n"
  )
  opt_code <- opt_dead_code(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "x <- 3",
    "while (TRUE) {",
    "  not_dead_code()",
    "  not_dead_code()",
    "}",
    "",
    "while (!TRUE) {",
    "  dead_code()",
    "  dead_code()",
    "}",
    "",
    "",
    sep = "\n"
  ))
})

test_that("eliminate FALSE if", {
  code <- paste(
    "x <- 4",
    "if (FALSE) {",
    "  dead_code()",
    "  dead_code()",
    "}",
    "x <- 5",
    "if (FALSE) {",
    "  dead_code()",
    "  dead_code()",
    "} else {",
    "  not_dead_code()",
    "  not_dead_code()",
    "}",
    "x <- 6",
    sep = "\n"
  )
  opt_code <- opt_dead_code(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "x <- 4",
    "x <- 5",
    "not_dead_code()",
    "not_dead_code()",
    "x <- 6",
    sep = "\n"
  ))
})

test_that("replace TRUE if", {
  code <- paste(
    "x <- 4",
    "if (TRUE) {",
    "  not_dead_code()",
    "  not_dead_code()",
    "}",
    "x <- 5",
    "if (TRUE) {",
    "  not_dead_code()",
    "  not_dead_code()",
    "} else {",
    "  dead_code()",
    "  dead_code()",
    "}",
    "x <- 6",
    sep = "\n"
  )
  opt_code <- opt_dead_code(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "x <- 4",
    "not_dead_code()",
    "not_dead_code()",
    "x <- 5",
    "not_dead_code()",
    "not_dead_code()",
    "x <- 6",
    sep = "\n"
  ))
})

test_that("dont eliminate in `if return() else ...`", {
  code <- paste(
    "if (rhs) rhs else return(0L)",
    "if (rhs) return(1L) else rhs",
    "if (rhs) return(0L) else rhs",
    "if (rhs) rhs else return(1L)",
    sep = "\n"
  )
  opt_code <- opt_dead_code(list(code))$codes[[1]]
  expect_equal(opt_code, code)
})

test_that("dead code careful with if () next", {
  code <- paste(
    "if(colnames[i] == 'lambda') next()",
    "if(colnames[i] == 'lambda') next()",
    sep = "\n"
  )
  opt_code <- opt_dead_code(list(code))$codes[[1]]
  expect_equal(opt_code, paste(
    "if(colnames[i] == 'lambda') next",
    "if(colnames[i] == 'lambda') next",
    sep = "\n"
  ))
})

test_that("dont dead code in function call", {
  code <- paste(
    "foo <- function(method) {",
    "  switch(method,",
    '         return("precmat.RW1(n)"),',
    '         return("precmat.RW2(n)"),',
    '         return("precmat.RWn(n, order)"),',
    '         return("precmat.season(n,season)"),',
    '         return("precmat.IGMRFreglat(n,m,...)"),',
    '         return("precmat.IGMRFirreglat(A,...)"),',
    '         return("precmat.GMRFreglat(n,m,...)"))',
    "}",
    sep = "\n"
  )
  opt_code <- opt_dead_code(list(code))$codes[[1]]
  expect_equal(opt_code, code)
})
