context("opt_cond_thread")

test_that("General Case without comments", {
  code <- paste(
    "num <- sample(1:100, 1)",
    "even_sum <- 0",
    "odd_sum_a <- 0",
    "odd_sum_b <- 0",
    "if(num %% 2) {",
    "odd_sum_a <- odd_sum_a + num",
    "}",
    "if(num %% 2) {",
    "odd_sum_b <- odd_sum_b + num",
    "}",
    "if(!(num %% 2)) {",
    "even_sum <- even_sum + num",
    "}",
    sep = "\n"
  )
  opt_code <- opt_cond_thread(list(code))$codes[[1]]
  expect_equal(opt_code, paste("num <- sample(1:100, 1)\neven_sum <- 0\nodd_sum_a <- 0\nodd_sum_b <- 0\nif(num %% 2) {\nodd_sum_a <- odd_sum_a + num\n odd_sum_b <- odd_sum_b + num\n}else {\n even_sum <- even_sum + num\n}"))
})

test_that("General Case with comments", {
  code <- paste(
    "num <- sample(1:100, 1)",
    "even_sum_a <- 0",
    "even_sum_b <- 0",
    "#Hello, I am a comment.",
    "odd_sum_a <- 0",
    "odd_sum_b <- 0",
    "if(num %% 2) { # I'm another comment, and I won't go easily",
    "odd_sum_a <- odd_sum_a + num",
    "}",
    "#This is another comment",
    "if(num %% 2) {",
    "odd_sum_b <- odd_sum_b + num",
    "}",
    "if(!(num %% 2)) {",
    "even_sum_a <- even_sum_a + num",
    "}",
    "if(!(num %% 2)) {",
    "even_sum_b <- even_sum_b + num",
    "}",
    sep = "\n"
  )
  opt_code <- opt_cond_thread(list(code))$codes[[1]]
  expect_equal(opt_code, paste("num <- sample(1:100, 1)\neven_sum_a <- 0\neven_sum_b <- 0\n#Hello, I am a comment.\nodd_sum_a <- 0\nodd_sum_b <- 0\nif(num %% 2) {\n # I'm another comment, and I won't go easily\nodd_sum_a <- odd_sum_a + num\n\n odd_sum_b <- odd_sum_b + num\n}else {\n even_sum_a <- even_sum_a + num\n even_sum_b <- even_sum_b + num\n}\n#This is another comment\n"))
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
    "if(a %% 2) {",
    "  return (FALSE)",
    "}",
    "if(!(a %% 2)) {",
    "  return (TRUE)",
    "}",
    "if(!(a %% 2)) {",
    "  print(\"Yaay\")",
    "}",
    "}",
    "is_even(150)",
    sep = "\n"
  )
  opt_code <- opt_cond_thread(list(code))$codes[[1]]
  expect_equal(opt_code, paste("is_even <- function(a) {\nif(a %% 2) {\n  return (FALSE)\n}else {\n   return (TRUE)\n   print(\"Yaay\")\n}}\nis_even(150)"))
})

test_that("Code Block containing a nested IF-ELSE block", {
  code <- paste(
    "num <- sample(1:100, 1)",
    "even_sum <- 0",
    "odd_sum <- 0",
    "count <- 0",
    "if(even_sum + odd_sum < 10) {",
    "if(num %% 2) {",
    "  odd_sum <- odd_sum + num",
    "}",
    "if(!(num %% 2)) {",
    "  even_sum <- even_sum + num",
    "}",
    "count <- count + 1",
    "}",
    sep = "\n"
  )
  opt_code <- opt_cond_thread(list(code))$codes[[1]]
  expect_equal(opt_code, paste("num <- sample(1:100, 1)\neven_sum <- 0\nodd_sum <- 0\ncount <- 0\nif(even_sum + odd_sum < 10) {\nif(num %% 2) {\n  odd_sum <- odd_sum + num\n}else {\n   even_sum <- even_sum + num\n}count <- count + 1\n}"))
})