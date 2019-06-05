
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
