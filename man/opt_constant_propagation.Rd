% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opt-constant-propagation.R
\name{opt_constant_propagation}
\alias{opt_constant_propagation}
\title{Optimizer: Constant Propagation.}
\usage{
opt_constant_propagation(texts, in_fun_call = FALSE)
}
\arguments{
\item{texts}{A list of character vectors with the code to optimize.}

\item{in_fun_call}{A logical indicating whether it should propagate in
function calls. Note: this could change the semantics of the program.}
}
\description{
Performs one constant propagation pass.
Carefully examine the results after running this function!
}
\examples{
code <- paste(
  "i <- 170",
  "x <- -170",
  "y <- x + 124",
  "z <- i - 124",
  sep = "\n"
)
cat(opt_constant_propagation(list(code))$codes[[1]])

hemisphere_vol <- paste(
  "pi <- 3.141593 ",
  "radius <- 25 ",
  "hemis_vol <- 2/3 * pi * radius ^ 3 ",
  sep = "\n"
)
cat(opt_constant_propagation(list(hemisphere_vol))$codes[[1]])
}
