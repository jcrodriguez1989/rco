context("for_to_lapply")
# get resulting environment after exec_text code execution starting with baseenv
return_env_after_exec <- function(exec_text) {
  new_env <- new.env(parent = baseenv())
  eval(parse(text = exec_text), envir = new_env)
  return(new_env)
}

test_that("'for' with no var used", {
  pre_vars <- "
  "
  exec_code <- "
    for (i in 1:10) {
      print(i^2);
    }
  "
  for_exec_code <- paste0(pre_vars, "\n", exec_code)
  lapply_exec_code <- paste0(pre_vars, "\n", rco::for_to_lapply(exec_code))
  expect_identical(
    as.list(return_env_after_exec(for_exec_code), all.names = TRUE),
    as.list(return_env_after_exec(lapply_exec_code), all.names = TRUE)
  )
})

test_that("'for' with two '<-' to outside vars used", {
  pre_vars <- "
    squared <- c();
    cubed <- c();
  "
  exec_code <- "
    for (i in 1:10) {
      squared[[i]] <- i^2;
      cubed[[i]] <- i^3;
    }
  "
  for_exec_code <- paste0(pre_vars, "\n", exec_code)
  lapply_exec_code <- paste0(pre_vars, "\n", rco::for_to_lapply(exec_code))
  expect_identical(
    as.list(return_env_after_exec(for_exec_code), all.names = TRUE),
    as.list(return_env_after_exec(lapply_exec_code), all.names = TRUE)
  )
})

test_that("'for' that updates and uses a variable", {
  pre_vars <- "
    fib <- 0:1;
    n <- 20;
  "
  exec_code <- "
    # returns a vector with fib0 to fibn
    for (i in 3:(n+1)) {
      fib[[i]] <- fib[[i-1]] + fib[[i-2]];
    }
  "
  for_exec_code <- paste0(pre_vars, "\n", exec_code)
  lapply_exec_code <- paste0(pre_vars, "\n", rco::for_to_lapply(exec_code))
  expect_identical(
    as.list(return_env_after_exec(for_exec_code), all.names = TRUE),
    as.list(return_env_after_exec(lapply_exec_code), all.names = TRUE)
  )
})
