#' All optimizers list
#'
#' List of all the optimizer functions:
#' \itemize{
#'   \item Constant Folding \code{\link{opt_constant_folding}}
#'   \item Constant Propagation \code{\link{opt_constant_propagation}}
#'   \item Dead Code Elimination \code{\link{opt_dead_code}}
#' }
#'
#' @export
#'
all_optimizers <- list(
  "Constant Folding" = opt_constant_folding,
  "Constant Propagation" = opt_constant_propagation,
  "Dead Code Elimination" = opt_dead_code
)
