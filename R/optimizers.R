#' All optimizers list.
#'
#' List of all the optimizer functions:
#' \itemize{
#'   \item Constant Folding \code{\link{opt_constant_folding}}
#'   \item Constant Propagation \code{\link{opt_constant_propagation}}
#'   \item Dead Code Elimination \code{\link{opt_dead_code}}
#'   \item Dead Store Elimination \code{\link{opt_dead_store}}
#'   \item Dead Expression Elimination \code{\link{opt_dead_expr}}
#'   \item Common Subexpression Elimination \code{\link{opt_common_subexpr}}
#'   \item Loop-invariant Code Motion \code{\link{opt_loop_invariant}}
#' }
#'
#' @export
#'
all_optimizers <- list(
  "Constant Folding" = opt_constant_folding,
  "Constant Propagation" = opt_constant_propagation,
  "Dead Code Elimination" = opt_dead_code,
  "Dead Store Elimination" = opt_dead_store,
  "Dead Expression Elimination" = opt_dead_expr,
  "Common Subexpression Elimination" = opt_common_subexpr,
  "Loop-invariant Code Motion" = opt_loop_invariant
)
