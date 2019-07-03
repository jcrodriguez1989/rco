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
  new_fpd <- one_common_subexpr(new_fpd)
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

  res_fpd
}

# Executes common subexpr elimination in the expr of an env
#
# @param fpd A flat parsed data data.frame .
# @param id Numeric indicating the node ID of the env expression.
#
common_subexpr_in_env <- function(fpd, id) {
  res_fpd <- remove_nodes(fpd, id)
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

  res_fpd <- rbind(res_fpd, env_fpd, get_children(act_fpd, fun_def_prnt_ids))
  res_fpd[order(res_fpd$pos_id), ]
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
  res <- lapply(unique(common_subexprs[, "text"]), function(act_common) {
    env_subexprs[env_subexprs[, "text"] == act_common, "expr_id"]
  })

  res[order(sapply(res, length), decreasing = TRUE)]
}

# If possible, returns a new fpd where common subexpressions were replaced by a
# new `tmp` variable
#
# @param fpd A flat parsed data data.frame .
# @param ids Numeric vector indicating the node IDs of each common subexpr.
#
subexpr_elim <- function(fpd, ids) {
  res_fpd <- fpd
  # get subexprs parents
  subexprs_parents <- lapply(ids, function(id) get_all_parents(fpd, id))
  # get the first parent in common
  common_parent <- subexprs_parents[[1]]
  for (i in seq_along(subexprs_parents)[-1]) {
    common_parent <- intersect(common_parent, subexprs_parents[[i]])
  }
  common_parent <- common_parent[[1]]

  # get place where tmp var should go (we assume pos_ids are ordered, and it
  # will go before the following expr)
  fst_expr_parents <- subexprs_parents[[1]]
  fst_expr_place <- fpd[
    fpd$id == fst_expr_parents[which(fst_expr_parents == common_parent) - 1], ]

  # get vars involved in the subexprs
  vars_inv <- get_children(fpd, ids[[1]])
  vars_inv <- vars_inv[vars_inv$token == "SYMBOL", "text"]

  # get vars involved assignations
  vars_inv_fpd <- fpd[fpd$id %in% get_assigns_ids(fpd, common_parent), ]
  vars_inv_fpd <- vars_inv_fpd[vars_inv_fpd$text %in% vars_inv, ]
  # get functions used in this env
  funs_fpd <- fpd[fpd$id %in% get_fun_call_ids(fpd, common_parent), ]
  # set them as trouble points
  tr_points <- rbind(vars_inv_fpd, funs_fpd)
  if (nrow(tr_points) > 0) {
    tr_points <- tr_points[
      tr_points$pos_id >= fst_expr_place$pos_id &
        tr_points$pos_id <= fpd$pos_id[fpd$id == tail(ids, 1)],
      ]
  }

  if (nrow(tr_points) > 0) {
    # split subexpr ids
    browser()
  }

  # create temp var
  new_var_name <- create_new_var(fpd)
  new_var_expr <- deparse_flat_data(get_children(fpd, ids[[1]]))
  new_var_expr <- sub("^\n*", "", new_var_expr)
  new_var <- paste0(new_var_name, " <- ", new_var_expr)
  new_var_fpd <- flatten_leaves(parse_flat_data(new_var))
  new_var_fpd$id <- paste0(new_var_name, "_", new_var_fpd$id)
  new_var_fpd$parent <- paste0(new_var_name, "_", new_var_fpd$parent)
  new_var_fpd[new_var_fpd$id == get_roots(new_var_fpd)$id, "parent"] <-
    common_parent
  new_var_fpd[nrow(new_var_fpd), "next_lines"] <- 1
  fst_expr_fpd <- get_children(fpd, fst_expr_place$id)
  new_var_fpd[new_var_fpd$terminal, "prev_spaces"][[1]] <-
    fst_expr_fpd[fst_expr_fpd$terminal, "prev_spaces"][[1]]
  new_var_fpd$pos_id <- create_new_pos_id(fpd, nrow(new_var_fpd),
                                          to_id = fst_expr_place$id)
  res_fpd <- rbind(res_fpd, new_var_fpd)

  # replace temp var in common subexprs
  repl_fpd <- flatten_leaves(parse_flat_data(new_var_name))
  for (act_id in ids) {
    act_fpd <- get_children(fpd, act_id)
    res_fpd <- remove_nodes(res_fpd, act_id)
    res_fpd <- rbind(res_fpd, replace_pd(act_fpd, repl_fpd))
  }

  res_fpd[order(res_fpd$pos_id), ]
}

# Returns the id of all the parents of a node
#
# @param fpd A flat parsed data data.frame .
# @param id Numeric indicating the node ID to get parents.
#
get_all_parents <- function(fpd, id) {
  res <- act_id <- id
  while (length(act_id) > 0 && act_id > 0) {
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

# Creates a new var name not used before in the fpd
#
# @param fpd A flat parsed data data.frame .
#
create_new_var <- function(fpd, prefix = "cs_") {
  prefix <- make.names(prefix)
  term_texts <- fpd[fpd$terminal, "text"]
  ptrn <- paste0("^", prefix)
  term_texts <- term_texts[grepl(ptrn, term_texts)] # startsWith()
  var_numbs <- sub(ptrn, "", term_texts)
  prev_num <- suppressWarnings(max(c(0, as.numeric(var_numbs)), na.rm = TRUE))
  paste0(prefix, prev_num + 1)
}

# Given a fpd and from or to id, it creates n new pos_ids
#
# @param fpd A flat parsed data data.frame .
# @param n Numeric indicating the number of pos_ids to create.
# @param from_id Numeric indicating the node ID to find fun calls.
# @param to_id Numeric indicating the node ID to find fun calls.
#
create_new_pos_id <- function(fpd, n, from_id = "", to_id = "") {
  fpd <- fpd[order(fpd$pos_id), ]
  from_pos_id <- fpd[fpd$id == from_id, "pos_id"]
  to_pos_id <- fpd[fpd$id == to_id, "pos_id"]
  if (length(from_pos_id) == 0) {
    from_pos_id <- c(fpd[which(fpd$id == to_id) - 1, "pos_id"],
                     to_pos_id -1 )[[1]]
  }
  from_pos_id + (10e-5 * seq_len(n))
}
