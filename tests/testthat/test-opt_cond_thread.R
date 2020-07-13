context("opt_cond_thread")

test_that("General Case without comments", {
  code <- paste(
    "num <- sample(1:100, 1)",
    "even_sum <- 0",
    "odd_sum_a <- 0",
    "odd_sum_b <- 0",
    "if(num %% 2 == 1) {",
    "  odd_sum_a <- odd_sum_a + num",
    "}",
    "if(num %% 2 == 1) {",
    "  odd_sum_b <- odd_sum_b + num",
    "}",
    "if(!(num %% 2 == 1)) {",
    "  even_sum <- even_sum + num",
    "}",
    sep = "\n"
  )
  opt_code <- opt_cond_thread(list(code))$codes[[1]]
  expect_equal(opt_code, paste("num <- sample(1:100, 1)\neven_sum <- 0\nodd_sum_a <- 0\nodd_sum_b <- 0\nif(num %% 2 == 1) {\nodd_sum_a <- odd_sum_a + num\n odd_sum_b <- odd_sum_b + num\n}else {\n even_sum <- even_sum + num\n}"))
})

test_that("General Case without comments(Alternative)", {
  code <- paste(
    "num <- sample(1:100, 1)",
    "even_sum_a <- 0",
    "odd_sum <- 0",
    "even_sum_b <- 0",
    "if(!(num %% 2 == 1)) {",
    "  even_sum_a <- even_sum_a + num",
    "}",
    "if(!(num %% 2 == 1)) {",
    "  even_sum_b <- even_sum_b + num",
    "}",
    "if(num %% 2 == 1) {",
    "  odd_sum <- odd_sum + num",
    "}",
    sep = "\n"
  )
  opt_code <- opt_cond_thread(list(code))$codes[[1]]
  expect_equal(opt_code, paste("num <- sample(1:100, 1)\neven_sum_a <- 0\nodd_sum <- 0\neven_sum_b <- 0\nif(!(num %% 2 == 1)) {\neven_sum_a <- even_sum_a + num\n even_sum_b <- even_sum_b + num\n}else {\n odd_sum <- odd_sum + num\n}"))
})

test_that("General Case with comments", {
  code <- paste(
    "num <- sample(1:100, 1)",
    "even_sum_a <- 0",
    "even_sum_b <- 0",
    "#Hello, I am a comment.",
    "odd_sum_a <- 0",
    "odd_sum_b <- 0",
    "if(num %% 2 == 1) { # I'm another comment, and I won't go easily",
    "  odd_sum_a <- odd_sum_a + num",
    "}",
    "#This is another comment",
    "if(num %% 2 == 1) {",
    "  odd_sum_b <- odd_sum_b + num",
    "}",
    "if(!(num %% 2 == 1)) {",
    "  even_sum_a <- even_sum_a + num",
    "}",
    "if(!(num %% 2 == 1)) {",
    "  even_sum_b <- even_sum_b + num",
    "}",
    sep = "\n"
  )
  opt_code <- opt_cond_thread(list(code))$codes[[1]]
  expect_equal(opt_code, paste("num <- sample(1:100, 1)\neven_sum_a <- 0\neven_sum_b <- 0\n#Hello, I am a comment.\nodd_sum_a <- 0\nodd_sum_b <- 0\nif(num %% 2 == 1) {\n # I'm another comment, and I won't go easily\nodd_sum_a <- odd_sum_a + num\n\n odd_sum_b <- odd_sum_b + num\n}else {\n even_sum_a <- even_sum_a + num\n even_sum_b <- even_sum_b + num\n}\n#This is another comment\n"))
})

test_that("Code snippet without any IF-ELSE blocks", {
  code <- paste(
    "for(i in 1:100) {",
    "  print(i)",
    "}",
    "rnorm(1)",
    sep = "\n"
  )
  opt_code <- opt_cond_thread(list(code))$codes[[1]]
  expect_equal(opt_code, paste("for(i in 1:100) {\n  print(i)\n}\nrnorm(1)"))
})

test_that("Code Blocks containing IF-ELSE blocks within loops", {
  code <- paste(
    "num_vec <- sample(1:5, 10, replace = T)",
    "ones <- 0",
    "others <- 0",
    "for(i in num_vec) {",
    "if(i == 1) {",
    "  ones <- ones + 1",
    "}",
    "if(i != 1) {",
    "  others <- others + 1",
    "}}",
    sep = "\n"
  )
  opt_code <- opt_cond_thread(list(code))$codes[[1]]
  expect_equal(opt_code, paste("num_vec <- sample(1:5, 10, replace = T)\nones <- 0\nothers <- 0\nfor(i in num_vec) {\nif(i == 1) {\n  ones <- ones + 1\n}else {\n   others <- others + 1\n}}"))
})

test_that("Code block containing IF-ELSE blocks inside a function", {
  code <- paste(
    "is_even <- function(a) {",
    "if(a %% 2 == 1) {",
    "  return (FALSE)",
    "}",
    "if(!(a %% 2 == 1)) {",
    "  return (TRUE)",
    "}",
    "if(!(a %% 2 == 1)) {",
    "  print(\"Yaay\")",
    "}",
    "}",
    "is_even(150)",
    sep = "\n"
  )
  opt_code <- opt_cond_thread(list(code))$codes[[1]]
  expect_equal(opt_code, paste("is_even <- function(a) {\nif(a %% 2 == 1) {\n  return (FALSE)\n}else {\n   return (TRUE)\n   print(\"Yaay\")\n}}\nis_even(150)"))
})

test_that("Code Block containing a nested IF-ELSE block", {
  code <- paste(
    "num <- sample(1:100, 1)",
    "even_sum <- 0",
    "odd_sum <- 0",
    "count <- 0",
    "if(even_sum + odd_sum < 10) {",
    "  if(num %% 2 == 1) {",
    "    odd_sum <- odd_sum + num",
    "  }",
    "  if(!(num %% 2 == 1)) {",
    "    even_sum <- even_sum + num",
    "  }",
    "  count <- count + 1",
    "}",
    sep = "\n"
  )
  opt_code <- opt_cond_thread(list(code))$codes[[1]]
  expect_equal(opt_code, paste("num <- sample(1:100, 1)\neven_sum <- 0\nodd_sum <- 0\ncount <- 0\nif(even_sum + odd_sum < 10) {\nif(num %% 2 == 1) {\n  odd_sum <- odd_sum + num\n}else {\n   even_sum <- even_sum + num\n}count <- count + 1\n}"))
})

test_that("Conversion to else due to Greater than equal to logic", {
  code <- paste(
    "num <- sample(1:100, 1)",
    "big <- 0",
    "small <- 0",
    "if(num >= 50) {",
    "  big <- big + 1",
    "}",
    "if(num < 50) {",
    "  small <- small + 1",
    "}",
    "print(big)",
    "print(small)",
    sep = "\n"
  )
  opt_code <- opt_cond_thread(list(code))$codes[[1]]
  expect_equal(opt_code, paste("num <- sample(1:100, 1)\nbig <- 0\nsmall <- 0\nif(num >= 50) {\n  big <- big + 1\n}else {\n   small <- small + 1\n}\nprint(big)\nprint(small)"))
})

test_that("Conversion to else due to Greater than equal to logic(alternative)", {
  code <- paste(
    "num <- sample(1:100, 1)",
    "big <- 0",
    "small <- 0",
    "if(num < 50) {",
    "  small <- small + 1",
    "}",
    "if(num >= 50) {",
    "  big <- big + 1",
    "}",
    "print(big)",
    "print(small)",
    sep = "\n"
  )
  opt_code <- opt_cond_thread(list(code))$codes[[1]]
  expect_equal(opt_code, paste("num <- sample(1:100, 1)\nbig <- 0\nsmall <- 0\nif(num < 50) {\n  small <- small + 1\n}else {\n   big <- big + 1\n}\nprint(big)\nprint(small)"))
})

test_that("Conversion to else due to Lesser than equal to logic", {
  code <- paste(
    "num <- sample(1:100, 1)",
    "big <- 0",
    "small <- 0",
    "if(num <= 50) {",
    "  small <- small + 1",
    "}",
    "if(num > 50) {",
    "  big <- big + 1",
    "}",
    "print(big)",
    "print(small)",
    sep = "\n"
  )
  opt_code <- opt_cond_thread(list(code))$codes[[1]]
  expect_equal(opt_code, paste("num <- sample(1:100, 1)\nbig <- 0\nsmall <- 0\nif(num <= 50) {\n  small <- small + 1\n}else {\n   big <- big + 1\n}\nprint(big)\nprint(small)"))
})

test_that("Conversion to else due to Lesser than equal to logic (Alternative)", {
  code <- paste(
    "num <- sample(1:100, 1)",
    "big <- 0",
    "small <- 0",
    "if(num > 50) {",
    "  big <- big + 1",
    "}",
    "if(num <= 50) {",
    "  small <- small + 1",
    "}",
    "print(big)",
    "print(small)",
    sep = "\n"
  )
  opt_code <- opt_cond_thread(list(code))$codes[[1]]
  expect_equal(opt_code, paste("num <- sample(1:100, 1)\nbig <- 0\nsmall <- 0\nif(num > 50) {\n  big <- big + 1\n}else {\n   small <- small + 1\n}\nprint(big)\nprint(small)"))
})

test_that("Conversion to else due to inequality", {
  code <- paste(
    "num <- sample(1:10, 1)",
    "lottery <- FALSE",
    "if(num == 5) {",
    "  lottery <- TRUE",
    "}" ,
    "if(num != 5) {",
    "  lottery <- FALSE",
    "}",
    "print(lottery)",
    sep = "\n"
  )
  opt_code <- opt_cond_thread(list(code))$codes[[1]]
  expect_equal(opt_code, paste("num <- sample(1:10, 1)\nlottery <- FALSE\nif(num == 5) {\nlottery <- TRUE\n}else {\n lottery <- FALSE\n}\nprint(lottery)"))
})

test_that("Case when the IF condtions have function calls", {
  code <- paste(
    "a <- 0",
    "b <- 0",
    "if(rnorm(1) > 0) {",
    "  a <- a + 1",
    "}",
    "if(rnorm(1) > 0) {",
    "  print(\"Yayy\")",
    "}",
    "if(!(rnorm(1) > 0)) {",
    "  b <- b + 1",
    "}",
    "print(a)",
    "print(b)",
    sep = "\n"
  )
  opt_code <- opt_cond_thread(list(code))$codes[[1]]
  expect_equal(opt_code, paste("a <- 0\nb <- 0\nif(rnorm(1) > 0) {\n  a <- a + 1\n}\nif(rnorm(1) > 0) {\n  print(\"Yayy\")\n}\nif(!(rnorm(1) > 0)) {\n  b <- b + 1\n}\nprint(a)\nprint(b)"))
})

