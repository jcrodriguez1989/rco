
# rco - The R Code Optimizer

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build
status](https://travis-ci.org/jcrodriguez1989/rco.svg?branch=master)](https://travis-ci.org/jcrodriguez1989/rco)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/jcrodriguez1989/rco?branch=master&svg=true)](https://ci.appveyor.com/project/jcrodriguez1989/rco)
[![Coverage
status](https://codecov.io/gh/jcrodriguez1989/rco/branch/master/graph/badge.svg)](https://codecov.io/github/jcrodriguez1989/rco?branch=master)

This project is being supported by [Google Summer of
Code 2019](https://summerofcode.withgoogle.com/projects/#5337917017292800).

Thanks to the kind mentorship of Drs. [Yihui
Xie](https://yihui.name/en/) and [Nicolás
Wolovick](https://cs.famaf.unc.edu.ar/~nicolasw/).

## Installation

`rco` is currently only available as a GitHub package.

To install it run the following from an R console:

``` r
if (!require("remotes"))
  install.packages("remotes")
remotes::install_github("jcrodriguez1989/rco")
```

## Usage

Optimize some `.R` code files:

``` r
optimize_files(c("file_to_optimize1.R", "file_to_optimize2.R"))
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
  sep = "\n")
```

We can automatically optimize it by doing:

``` r
opt_code <- optimize_text(code)
```

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
opt_code <- optimize_text(opt_code)
```

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
opt_code <- optimize_text(opt_code)
```

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
opt_code <- optimize_text(opt_code)
```

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
opt_code <- optimize_text(opt_code)
```

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
