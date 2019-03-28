R Code Optimizer
================

## Project

This project has not started yet. This project was proposed for the
R-GSoC 2019.

## Example function

As an idea of the project, an example function has been implemented.

`for_to_lapply` takes as input a character vector containing the code of
an R working `for` loop and rewrites it into a `lapply` function.

### Notes

  - It is not true that a `lapply` loop consumes less execution time
    than a `for` loop.
  - Comments from the `for_text` input code, are being deleted from the
    resulting code.
  - This implementation creates a lapply function that uses ‘assign’ to
    copy values of the parent environments, and thus, would not work for
    example in code parallelization.
  - Might fail if the generated code is used on other than `.GlobalEnv`.
  - Might fail if the `for` loop uses `<<-`, or assigns to specified
    environments.

### Description

According to the R Help pages:

``` r
for(var in seq) expr
* var  A syntactical name for a variable.
* seq  An expression evaluating to a vector (including a list and an
       expression) or to a pairlist or NULL. A factor value will be coerced
       to a character vector.
* expr An expression in a formal sense. This is either a simple expression
       or a so called compound expression, usually of the form { expr1 ;
       expr2 }.
```

``` r
lapply(X, FUN, ...)
* X    a vector (atomic or list) or an expression object. Other objects
       (including classed objects) will be coerced by base::as.list.
* FUN  the function to be applied to each element of X: see ‘Details’. In the
       case of functions like +, %*%, the function name must be backquoted or
       quoted.
* ...  optional arguments to FUN.
```

So simply `for(var in seq) expr` \<=\> `lapply(seq, function(var) { expr
})`

However, the main difference between both is that lapply is a function,
meanwhile for is a Control Flow command, and thus, they differ in which
environment they work on.

### Usage

If the following code does not give an error:

``` r
squared <- numeric(10)
for (i in seq_along(squared)) {
  squared[[i]] <- i^2
}
ls()
```

    ## [1] "i"       "squared"

``` r
i; squared
```

    ## [1] 10

    ##  [1]   1   4   9  16  25  36  49  64  81 100

Then, we can convert the `for` code to a `lapply` code:

``` r
lapply_code <- for_to_lapply("
for (i in seq_along(squared)) {
  squared[[i]] <- i^2
}
")
```

    ## invisible(lapply(seq_along(squared), function(i) {
    ##   squared[[i]] <- i^2
    ## 
    ##   # the following lines are to copy variable modifications to the
    ##   # environment outside the lapply
    ##   act_env_lst <- as.list(environment(), all.names = TRUE)
    ##   to_env <- parent.env(environment())
    ##   invisible(lapply(names(act_env_lst), function(var) {
    ##     assign(var, act_env_lst[[var]], envir = to_env)
    ##   }))
    ## }))

And then copy and paste (and use) this code:

``` r
rm(i); rm(squared); rm(lapply_code)
squared <- numeric(10)
invisible(lapply(seq_along(squared), function(i) {
  squared[[i]] <- i^2

  # the following lines are to copy variable modifications to the
  # environment outside the lapply
  act_env_lst <- as.list(environment(), all.names = TRUE)
  to_env <- parent.env(environment())
  invisible(lapply(names(act_env_lst), function(var) {
    assign(var, act_env_lst[[var]], envir = to_env)
  }))
}))
ls()
```

    ## [1] "i"       "squared"

``` r
i; squared
```

    ## [1] 10

    ##  [1]   1   4   9  16  25  36  49  64  81 100

Or (just to show) to eval this text code:

``` r
rm(i); rm(squared);
squared <- numeric(10)
lapply_code <- for_to_lapply("
for (i in seq_along(squared)) {
  squared[[i]] <- i^2
}
")
```

    ## invisible(lapply(seq_along(squared), function(i) {
    ##   squared[[i]] <- i^2
    ## 
    ##   # the following lines are to copy variable modifications to the
    ##   # environment outside the lapply
    ##   act_env_lst <- as.list(environment(), all.names = TRUE)
    ##   to_env <- parent.env(environment())
    ##   invisible(lapply(names(act_env_lst), function(var) {
    ##     assign(var, act_env_lst[[var]], envir = to_env)
    ##   }))
    ## }))

``` r
eval(parse(text=lapply_code))
ls()
```

    ## [1] "i"           "lapply_code" "squared"

``` r
i; squared
```

    ## [1] 10

    ##  [1]   1   4   9  16  25  36  49  64  81 100
