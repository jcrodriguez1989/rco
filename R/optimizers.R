#' All optimizers list
#'
#' List of all the optimizer functions:
#' \itemize{
#'   \item Constant Folding \code{\link{opt_constant_folding}}
#'   \item Constant Propagation \code{\link{opt_constant_propagation}}
#'   \item Dead Code Elimination \code{\link{opt_dead_code}}
#' }
#' Carefully examine the results after running this function!
#'
#' @export
#'
all_optimizers <- list(
  opt_constant_folding,
  opt_constant_propagation,
  opt_dead_code
)
