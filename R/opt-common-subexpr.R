#' Optimizer: Common Subexpression Elimination
#'
#' Performs one common subexpression elimination pass.
#' Carefully examine the results after running this function!
#'
#' @param texts A list of character vectors with the code to optimize.
#'
#' @examples
#' code <- paste(
#'   "a <- b * c + g"
#'   "d = b * c * e",
#'   sep = "\n"
#' )
#' cat(opt_common_subexpr(list(code))$codes[[1]])
#' @export
#'
opt_common_subexpr <- function(texts) {
  # todo: add functions as common subexpression?
  # this can have an issue if function returns random values, i.e.,
  # a <-rnorm(1) * 8; b <-rnorm(1) * 18 , will wrongly optimize to
  # tmp <- rnorm(1); a <-tmp * 8; b <-tmp * 18
  res <- list()
  res$codes <- lapply(texts, common_subexpr_one)
  return(res)
}

# Executes common subexpression elimination on one text of code
#
# @param text A character vector with code to optimize.
#
common_subexpr_one <- function(text) {
  fpd <- parse_flat_data(text)
  fpd <- flatten_leaves(fpd)
  res_fpd <- fpd[fpd$parent < 0, ] # keep lines with just comments
  new_fpd <- fpd[fpd$parent >= 0, ] # keep lines with just comments

  # eliminate until no changes
  old_fpd <- NULL
  while (!isTRUE(all.equal(old_fpd, new_fpd))) {
    old_fpd <- new_fpd
    new_fpd <- one_common_subexpr(new_fpd)
  }

  res_fpd <- rbind(res_fpd, new_fpd)
  if (nrow(res_fpd) > 0) {
    res_fpd <- res_fpd[order(res_fpd$pos_id), ]
  }

  deparse_flat_data(res_fpd)
}

# Executes common subexpression elimination of a tree
#
# @param fpd A flat parsed data data.frame .
#
one_common_subexpr <- function(fpd) {
  res_fpd <- fpd

  # get different envs (parent env and function defs)
  env_parent_ids <- unique(
    fpd[fpd$token == "FUNCTION" | fpd$parent <= 0, "parent"])

  # for each env do the common subexpr elimination
  for (env_parent_id in env_parent_ids) {
    res_fpd <- common_subexpr_in_env(res_fpd, env_parent_id)
  }
  browser()

  return(res_fpd)
}

# Executes common subexpr elimination in the expr of an env
#
# @param fpd A flat parsed data data.frame .
# @param id Numeric indicating the node ID of the env expression.
#
common_subexpr_in_env <- function(fpd, id) {
  act_fpd <- get_children(fpd, id)
  # get and remove function definitions (as they have own env)
  fun_def_prnt_ids <- act_fpd[act_fpd$token == "FUNCTION", "parent"]
  fun_def_prnt_ids <- setdiff(fun_def_prnt_ids, id)
  env_fpd <- remove_nodes(act_fpd, fun_def_prnt_ids)

  # get common subexpressions within an env
  common_subexprs_ids <- get_common_subexprs(env_fpd, id)

  for (i in seq_along(common_subexprs_ids)) {
    act_ids <- common_subexprs_ids[[i]]
    if (!all(act_ids %in% env_fpd$id)) {
      next
    }
    env_fpd <- subexpr_elim(env_fpd, act_ids)
  }
  browser()
}

# Returns a list, where each field is a vector of 2 IDs that use the same expr
#
# @param fpd A flat parsed data data.frame .
# @param id Numeric indicating the node ID of the env expression.
#
get_common_subexprs <- function(fpd, id) {
  # get all subexprs (dont have assignment, while, if, etc)
  env_exprs_ids <- fpd[fpd$token == "expr", "id"]
  env_subexprs <-
    do.call(rbind, lapply(env_exprs_ids, function(env_exprs_id) {
      aux_fpd <- get_children(fpd, env_exprs_id)
      res <- NULL
      if (all(aux_fpd$token %in%
              c(constants, ops, precedence_ops, "expr", "SYMBOL"))) {
        res <- c(
          expr_id = env_exprs_id,
          text = paste(aux_fpd[aux_fpd$terminal, "text"], collapse = " ")
        )
      }
      res
    }))

  # get common subexpressions, and return a list with their ids
  common_subexprs <- env_subexprs[duplicated(env_subexprs[, "text"]), ,
                                  drop = FALSE]
  do.call(c, lapply(
    unique(common_subexprs[, "text"]), function(act_common) {
      act_ids <- env_subexprs[env_subexprs[, "text"] == act_common, "expr_id"]
      combns <- combn(act_ids, 2)
      lapply(seq_len(ncol(combns)), function(i) combns[, i])
    }
  ))
}

# If possible, returns a new fpd where common subexpressions were replaced by a
# new `tmp` variable
#
# @param fpd A flat parsed data data.frame .
# @param ids Numeric vector indicating the node IDs of each common subexpr.
#
subexpr_elim <- function(fpd, ids) {
  # get vars involved in the subexprs
  vars_inv <- get_children(fpd, ids[[1]])
  vars_inv <- vars_inv[vars_inv$token == "SYMBOL", "text"]

  # get subexprs parents
  subexprs_parents <- lapply(ids, function(id) get_all_parents(fpd, id))
  # get the first parent in common
  common_parent <- subexprs_parents[[1]]
  for (i in seq_along(subexprs_parents)[-1]) {
    common_parent <- intersect(common_parent, subexprs_parents[[i]])
  }

  # get vars involved assignations
  vars_inv_fpd <- fpd[fpd$id %in% get_assigns_ids(fpd, common_parent), ]
  vars_inv_fpd <- vars_inv_fpd[vars_inv_fpd$text %in% vars_inv, ]
  browser()

  # get functions used in this env
  funs_fpd <- fpd[fpd$id %in% get_fun_call_ids(fpd, common_parent), ]

}

# Returns the id of all the parents of a node
#
# @param fpd A flat parsed data data.frame .
# @param id Numeric indicating the node ID to get parents.
#
get_all_parents <- function(fpd, id) {
  res <- act_id <- id
  while (act_id > 0) {
    act_id <- fpd[fpd$id == act_id, "parent"]
    res <- c(res, act_id)
  }
  res
}

# Returns the ids of the fpd SYMBOLs that are being assigned
#
# @param fpd A flat parsed data data.frame .
# @param id Numeric indicating the node ID to find assigns.
#
get_assigns_ids <- function(fpd, id) {
  act_fpd <-get_children(fpd, id)
  # get parents of <- <<- -> ->> and =
  assign_exprs_prnts <- act_fpd[
    act_fpd$token %in% assigns & act_fpd$text != ":=",
    "parent"]
  # get the assigned SYMBOL fpd id
  sapply(assign_exprs_prnts, function(assign_exprs_prnt) {
    aux <- act_fpd[act_fpd$parent == assign_exprs_prnt, ]
    if (aux$token[[2]] == "RIGHT_ASSIGN") {
      res <- aux[3, ]
    } else {
      res <- aux[1, ]
    }
    res[res$token == "SYMBOL", "id"]
  })
}

# Returns the ids of the parents of function calls
#
# @param fpd A flat parsed data data.frame .
# @param id Numeric indicating the node ID to find fun calls.
#
get_fun_call_ids <- function(fpd, id) {
  act_fpd <- get_children(fpd, id)
  act_fpd[act_fpd$token == "SYMBOL_FUNCTION_CALL", "id"]
}
