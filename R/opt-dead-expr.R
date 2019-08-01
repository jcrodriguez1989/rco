#' Optimizer: Dead Expression Elimination
#'
#' Performs one dead expression elimination pass.
#' Carefully examine the results after running this function!
#'
#' @param texts A list of character vectors with the code to optimize.
#'
#' @examples
#' code <- paste(
#'   "foo <- function(x) {",
#'   "  x ^ 3",
#'   "  return(x ^ 3)",
#'   "}",
#'   sep = "\n"
#' )
#' cat(opt_dead_expr(list(code))$codes[[1]])
#' @export
#'
opt_dead_expr <- function(texts) {
  # todo: check if dead expressions have never assigned vars.
  # foo() { x; return(8818) } is not equivalent to foo() { return(8818) }
  # as the first one might have an error.
  res <- list()
  res$codes <- lapply(texts, de_one_file)
  res
}

# Executes dead expression elimination on one file of code
#
# @param text A character vector with code to optimize.
#
de_one_file <- function(text) {
  fpd <- parse_text(text)
  res_fpd <- fpd[fpd$parent < 0, ] # keep lines with just comments
  new_fpd <- fpd[fpd$parent >= 0, ] # keep lines with just comments
  new_fpd <- de_one_fpd(new_fpd)
  res_fpd <- rbind(res_fpd, new_fpd)
  if (nrow(res_fpd) > 0) {
    res_fpd <- res_fpd[order(res_fpd$pos_id), ]
  }

  deparse_data(res_fpd)
}

# Executes dead expression elimination of a fpd tree
#
# @param fpd A flat parsed data data.frame .
#
de_one_fpd <- function(fpd) {
  res_fpd <- fpd

  # exprs in functions dont have any effect. However, on the global env they
  # print on console, so just analyze function definitions
  fun_def_ids <- fpd[fpd$token == "FUNCTION", "parent"]

  # get unassigned expressions
  dead_exprs_ids <- unlist(lapply(fun_def_ids, function(act_id)
    get_unassigned_exprs(fpd, act_id)))

  # remove the ones that are last expression of functions
  return_ids <- unlist(lapply(fun_def_ids, function(act_id)
    get_fun_last_exprs(fpd, act_id)))
  dead_exprs_ids <- dead_exprs_ids[!dead_exprs_ids %in% return_ids]

  pretty_remove_nodes(res_fpd, dead_exprs_ids)
}

# Returns the ids of expressions that are not being assigned to a var.
#
# @param fpd A flat parsed data data.frame .
# @param id Numeric indicating the node ID of the function to search for
#   unassigned expressions.
#
get_unassigned_exprs <- function(fpd, id) {
  funs_body_ids <- sapply(id, function(act_id)
    utils::tail(fpd$id[fpd$parent == act_id & fpd$token == "expr"], 1))
  act_fpd <- get_children(fpd, funs_body_ids)

  # start visiting root nodes
  visit_nodes <- get_roots(act_fpd)$id
  exprs_ids <- c()
  while (length(visit_nodes) > 0) {
    new_visit <- c()
    for (act_parent in visit_nodes) {
      act_prnt_fpd <- get_children(act_fpd, act_parent)
      act_sblngs <- act_prnt_fpd[act_prnt_fpd$parent == act_parent, ]
      if (act_sblngs$token[[1]] == "'{'") {
        new_visit <- c(new_visit, act_sblngs$id[act_sblngs$token == "expr"])
      } else if (all(act_prnt_fpd$token %in%
        c(constants, ops, precedence_ops, "expr", "SYMBOL"))) {
        # it is an expression
        exprs_ids <- c(exprs_ids, act_parent)
      } else if (any(c(loops, "IF") %in% act_sblngs$token)) {
        # remove conditional expr
        body_ids <- act_sblngs$id[!act_sblngs$terminal]
        body_ids <- body_ids[seq(
          any(c("')'", "forcond") %in% act_sblngs$token) + 1, length(body_ids)
        )]
        new_visit <- c(new_visit, body_ids)
      } else {
        next
      }
    }
    visit_nodes <- new_visit
  }

  # remove assigned exprs and others
  exprs_ids <- exprs_ids[!sapply(exprs_ids, function(act_id) {
    act_sblngs <- act_fpd[act_fpd$parent ==
      act_fpd$parent[act_fpd$id == act_id], ]
    any(assigns %in% act_sblngs$token) # the expr is being assigned
  })]

  exprs_ids
}

# Returns the IDs of the exprs that can return in a function.
#
# @param fpd A flat parsed data data.frame .
# @param id Numeric indicating the fun node ID to check.
#
get_fun_last_exprs <- function(fpd, id) {
  fun_body <- fpd$id[fpd$parent == id & fpd$token == "expr"]
  act_fpd <- get_children(fpd, fun_body)

  # start visiting root nodes
  visit_nodes <- get_roots(act_fpd)$id
  last_exprs_ids <- c()
  while (length(visit_nodes) > 0) {
    new_visit <- c()
    for (act_parent in visit_nodes) {
      act_prnt_fpd <- get_children(act_fpd, act_parent)
      act_sblngs <- act_prnt_fpd[act_prnt_fpd$parent == act_parent, ]
      if (act_sblngs$token[[1]] == "'{'") {
        # has multiple exprs, check only the last one
        new_visit <- c(
          new_visit,
          utils::tail(act_sblngs$id[act_sblngs$token == "expr"], 1)
        )
      } else if (any(loops %in% act_sblngs$token)) {
        next
      } else if ("IF" %in% act_sblngs$token) {
        # visit if body and else body
        if_else_body_ids <- act_sblngs$id[act_sblngs$token == "expr"][-1]
        new_visit <- c(new_visit, if_else_body_ids)
      } else {
        last_exprs_ids <- c(last_exprs_ids, act_parent)
      }
    }
    visit_nodes <- new_visit
  }

  # returns last exprs and their children ids
  get_children(fpd, last_exprs_ids)$id
}
