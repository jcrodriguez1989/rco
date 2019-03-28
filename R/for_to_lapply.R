# According to the R Help pages:
#
# for(var in seq) expr
# * var  A syntactical name for a variable.
# * seq  An expression evaluating to a vector (including a list and an
#        expression) or to a pairlist or NULL. A factor value will be coerced
#        to a character vector.
# * expr An expression in a formal sense. This is either a simple expression
#        or a so called compound expression, usually of the form { expr1 ;
#        expr2 }.
#
# lapply(X, FUN, ...)
# * X    a vector (atomic or list) or an expression object. Other objects
#        (including classed objects) will be coerced by base::as.list.
# * FUN  the function to be applied to each element of X: see ‘Details’. In the
#        case of functions like +, %*%, the function name must be backquoted or
#        quoted.
# * ...  optional arguments to FUN.

# So simply for(var in seq) expr <=> lapply(seq, function(var) { expr })
# However, the main difference between both is that lapply is a function,
# meanwhile for is a Control Flow command, and thus, they differ in which
# environment they work on.

# Notes:
# * comments from the for_text input code, are being deleted by parse_expr.
# * this implementation creates a lapply function that uses 'assign' to copy
#   values of the parent environments, and thus, would not work for example in
#   code parallelization.

#' Convert 'for' loop code to 'lapply' code
#'
#' @param for_text a character vector with the runnable 'for' loop code.
#'
#' @examples
#' for_to_lapply("
#'   for (i in 1:10) {
#'     # just prints the square of numbers from 1 to 10
#'     print(i^2)
#'   }
#' ")
#'
#' @importFrom magrittr %>%
#' @importFrom methods is
#' @importFrom rlang parse_expr expr_deparse
#' @importFrom styler style_text
#' @export
for_to_lapply <- function(for_text) {
  parsed_for <- rlang::parse_expr(for_text)
  if (!is(parsed_for, "for")) {
    stop("for_text variable must be a working 'for' loop as string.",
      call. = FALSE
    )
  }

  res <- paste0(
    get_header(parsed_for[[2]], parsed_for[[3]]),
    get_body(parsed_for[[4]]),
    get_footer()
  ) %>%
    styler::style_text()
  print(res)
  paste(as.character(res), collapse = "\n")
}

get_header <- function(var, seq) {
  paste0(
    "invisible(lapply(",
    rlang::expr_deparse(seq),
    ", function(",
    rlang::expr_deparse(var),
    ") {"
  )
}

get_body <- function(expr) {
  if (is(expr, "{")) {
    expr <- expr[-1]
  }
  paste0(
    paste(lapply(expr, rlang::expr_deparse), collapse = "; "), "; \n\n",
    "# the following lines are to copy variable modifications to the \n",
    "# environment outside the lapply\n",
    "act_env_lst <- as.list(environment(), all.names = TRUE);",
    "to_env <- parent.env(environment());",
    "invisible(lapply(names(act_env_lst), function(var) {",
    "  assign(var, act_env_lst[[var]], envir = to_env)",
    "}))"
  )
}

get_footer <- function() {
  "}))"
}
