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

#' Max optimizers list.
#'
#' List of all the optimizer functions, with maximum optimization techniques
#' enabled.
#' Note that using this optimizers could change the semantics of the program!
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
max_optimizers <- list(
  "Constant Folding" = function(texts) {
    opt_constant_folding(texts, fold_floats = TRUE, in_fun_call = TRUE)
  },
  "Constant Propagation" = function(texts) {
    opt_constant_propagation(texts, in_fun_call = TRUE)
  },
  "Dead Code Elimination" = opt_dead_code,
  "Dead Store Elimination" = opt_dead_store,
  "Dead Expression Elimination" = opt_dead_expr,
  "Common Subexpression Elimination" = function(texts) {
    opt_common_subexpr(texts, in_fun_call = TRUE)
  },
  "Loop-invariant Code Motion" = opt_loop_invariant
)
