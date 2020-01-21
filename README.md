
# rco - The R Code Optimizer

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build
status](https://travis-ci.org/jcrodriguez1989/rco.svg?branch=master)](https://travis-ci.org/jcrodriguez1989/rco)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/jcrodriguez1989/rco?branch=master&svg=true)](https://ci.appveyor.com/project/jcrodriguez1989/rco)
[![Coverage
status](https://codecov.io/gh/jcrodriguez1989/rco/branch/master/graph/badge.svg)](https://codecov.io/github/jcrodriguez1989/rco?branch=master)

Make your R code run faster\! `rco` analyzes your code and applies
different optimization strategies that return an R code that runs
faster.

The `rco` project, from its start to version 1.0.0, was made possible by
a [Google Summer of Code 2019
project](https://summerofcode.withgoogle.com/archive/2019/projects/6300906386096128/).

Thanks to the kind mentorship of [Dr. Yihui Xie](https://yihui.name/en/)
and [Dr. Nicolás Wolovick](https://cs.famaf.unc.edu.ar/~nicolasw/).

## Installation

`rco` is currently only available as a GitHub package.

To install it run the following from an R console:

``` r
if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github("jcrodriguez1989/rco", dependencies = TRUE)
```

## Usage

`rco` can be used in three ways:

  - Using the RStudio Addins
    
    1.  `Optimize active file`: Optimizes the file currently open in
        RStudio. It will apply the optimizers present in
        `all_optimizers`.
    
    2.  `Optimize selection`: Optimizes the code currently highlited in
        the RStudio Source Pane. It will apply the optimizers present in
        `all_optimizers`.

  - Using the `shiny` GUIs
    
    1.  `rco_gui("code_optimizer")` opens a `shiny` interface in a
        browser. This GUI allows to easily optimize chunks of code.
    
    2.  `rco_gui("pkg_optimizer")` opens a `shiny` interface in a
        browser. This GUI allows to easily optimize R packages that are
        hosted at CRAN or GitHub.

  - Using the R functions
    
    1.  Optimize some `.R` code files
    
    <!-- end list -->
    
    ``` r
    optimize_files(c("file_to_optimize_1.R", "file_to_optimize_2.R"))
    ```
    
    2.  Optimize some code in a character vector
    
    <!-- end list -->
    
    ``` r
    code <- paste(
      "code_to_optimize <- 8 ^ 8 * 1918",
      "cto <- code_to_optimize * 2",
      sep = "\n"
    )
    optimize_text(code)
    ```
    
    3.  Optimize all `.R` code files into a folder
    
    <!-- end list -->
    
    ``` r
    optimize_folder("~/myfolder_to_optimize", recursive = FALSE)
    ```

## Example

Suppose we have the following code:

``` r
code <- paste(
  "# I want to know my age in seconds!",
  "years_old <- 29",
  "days_old <- 365 * years_old # leap years don't exist",
  "hours_old <- 24 * days_old",
  "seconds_old <- 60 * 60 * hours_old",
  "",
  "if (seconds_old > 10e6) {",
  '  print("Whoa! More than a million seconds old, what a wise man!")',
  "} else {",
  '  print("Meh!")',
  "}",
  sep = "\n"
)
```

We can automatically optimize it by doing:

``` r
opt_code <- optimize_text(code, iterations = 1)
```

    ## Optimization number 1

    ## # I want to know my age in seconds!
    ## years_old <- 29
    ## days_old <- 365 * 29 # leap years don't exist
    ## hours_old <- 24 * days_old
    ## seconds_old <- 3600 * hours_old
    ## 
    ## if (seconds_old > 10e6) {
    ##   print("Whoa! More than a million seconds old, what a wise man!")
    ## } else {
    ##   print("Meh!")
    ## }

After one optimization pass we can see that it has only propagated the
`years_old` variable. Another pass:

``` r
opt_code <- optimize_text(opt_code, iterations = 1)
```

    ## Optimization number 1

    ## # I want to know my age in seconds!
    ## years_old <- 29
    ## days_old <- 10585 # leap years don't exist
    ## hours_old <- 24 * 10585
    ## seconds_old <- 3600 * hours_old
    ## 
    ## if (seconds_old > 10e6) {
    ##   print("Whoa! More than a million seconds old, what a wise man!")
    ## } else {
    ##   print("Meh!")
    ## }

Now, it has folded the `days_old` variable, and then propagated it.
Another pass:

``` r
opt_code <- optimize_text(opt_code, iterations = 1)
```

    ## Optimization number 1

    ## # I want to know my age in seconds!
    ## years_old <- 29
    ## days_old <- 10585 # leap years don't exist
    ## hours_old <- 254040
    ## seconds_old <- 3600 * 254040
    ## 
    ## if (seconds_old > 10e6) {
    ##   print("Whoa! More than a million seconds old, what a wise man!")
    ## } else {
    ##   print("Meh!")
    ## }

It has folded the `hours_old` variable, and then propagated it. Another
pass:

``` r
opt_code <- optimize_text(opt_code, iterations = 1)
```

    ## Optimization number 1

    ## # I want to know my age in seconds!
    ## years_old <- 29
    ## days_old <- 10585 # leap years don't exist
    ## hours_old <- 254040
    ## seconds_old <- 914544000
    ## 
    ## if (914544000 > 10e6) {
    ##   print("Whoa! More than a million seconds old, what a wise man!")
    ## } else {
    ##   print("Meh!")
    ## }

It has folded the `seconds_old` variable, and then propagated it into
the `if` condition. Another pass:

``` r
opt_code <- optimize_text(opt_code, iterations = 1)
```

    ## Optimization number 1

    ## # I want to know my age in seconds!
    ## years_old <- 29
    ## days_old <- 10585 # leap years don't exist
    ## hours_old <- 254040
    ## seconds_old <- 914544000
    ## 
    ## print("Whoa! More than a million seconds old, what a wise man!")

Now, it has folded the `if` condition, and as it was `TRUE` it just kept
its body, as testing the condition or the `else` clause were dead code.
So, `optimize_text` function has automatically detected constant
variables, constant foldable operations, and dead code. And returned an
optimized R code.

## Guidelines for contributing

`rco` is an open source package, and the contributions to the
development of the library are more than welcome. Please see our
[CONTRIBUTING.md](https://github.com/jcrodriguez1989/rco/blob/master/.github/CONTRIBUTING.md)
file and [“Contributing an
Optimizer”](https://jcrodriguez1989.github.io/rco/articles/contributing-an-optimizer.html)
article for detailed guidelines of how to contribute.

## Code of Conduct

Please note that the ‘rco’ project is released with a [Contributor Code
of
Conduct](https://github.com/jcrodriguez1989/rco/blob/master/CODE_OF_CONDUCT.md).

By contributing to this project, you agree to abide by its terms.
