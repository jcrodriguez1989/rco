#' Optimizer: Common Subexpression Elimination
#'
#' Performs one common subexpression elimination pass.
#' Carefully examine the results after running this function!
#'
#' @param texts A list of character vectors with the code to optimize.
#' @param n_values Numeric indicating the minimum number of values to consider
#'   a subexpression.
#'
#' @examples
#' code <- paste(
#'   "a <- b * c + g",
#'   "d = b * c * e",
#'   sep = "\n"
#' )
#' cat(opt_common_subexpr(list(code))$codes[[1]])
#' @export
#'
opt_common_subexpr <- function(texts, n_values = 2) {
  # todo: add functions as common subexpression?
  # this can have an issue if function returns random values, i.e.,
  # a <-rnorm(1) * 8; b <-rnorm(1) * 18 , will wrongly optimize to
  # tmp <- rnorm(1); a <-tmp * 8; b <-tmp * 18
  # todo: check which functions modify the parent env. In this way, function
  # calls wont stop optimization
  res <- list()
  res$codes <- lapply(texts, cs_one_file, n_values = n_values)
  return(res)
}

# Executes common subexpression elimination on one file of code
#
# @param text A character vector with code to optimize.
# @param n_values Numeric indicating the minimum number of values to consider
#   a subexpression.
#
cs_one_file <- function(text, n_values) {
  fpd <- parse_flat_data(text)
  fpd <- flatten_leaves(fpd)
  res_fpd <- fpd[fpd$parent < 0, ] # keep lines with just comments
  new_fpd <- fpd[fpd$parent >= 0, ] # keep lines with just comments
  new_fpd <- cs_one_fpd(new_fpd, n_values)
  res_fpd <- rbind(res_fpd, new_fpd)
  if (nrow(res_fpd) > 0) {
    res_fpd <- res_fpd[order(res_fpd$pos_id), ]
  }

  deparse_flat_data(res_fpd)
}

# Executes common subexpression elimination of a fpd tree
#
# @param fpd A flat parsed data data.frame .
# @param n_values Numeric indicating the minimum number of values to consider
#   a subexpression.
#
cs_one_fpd <- function(fpd, n_values) {
  res_fpd <- fpd

  # get different envs (parent env and function defs)
  env_parent_ids <- unique(
    fpd[fpd$token == "FUNCTION" | fpd$parent <= 0, "parent"])

  # for each env do the common subexpr elimination
  for (env_parent_id in env_parent_ids) {
    res_fpd <- common_subexpr_in_env(res_fpd, env_parent_id, n_values)
  }

  res_fpd
}

# Executes common subexpr elimination in the expr of an env
#
# @param fpd A flat parsed data data.frame .
# @param id Numeric indicating the node ID of the env expression.
# @param n_values Numeric indicating the minimum number of values to consider
#   a subexpression.
#
common_subexpr_in_env <- function(fpd, id, n_values) {
  res_fpd <- remove_nodes(fpd, id)
  act_fpd <- get_children(fpd, id)
  # get and remove function definitions (as they have own env)
  fun_def_prnt_ids <- act_fpd[act_fpd$token == "FUNCTION", "parent"]
  fun_def_prnt_ids <- setdiff(fun_def_prnt_ids, id)
  env_fpd <- remove_nodes(act_fpd, fun_def_prnt_ids)

  # get common subexpressions within an env
  common_subexprs_ids <- get_common_subexprs(env_fpd, id, n_values)

  for (i in seq_along(common_subexprs_ids)) {
    act_ids <- common_subexprs_ids[[i]]
    if (!all(act_ids %in% env_fpd$id)) {
      next
    }
    env_fpd <- subexpr_elim(env_fpd, act_ids, n_values)
  }

  res_fpd <- rbind(res_fpd, env_fpd,
                   unique(get_children(act_fpd, fun_def_prnt_ids)))
  res_fpd[order(res_fpd$pos_id), ]
}

# Returns a list, where each field is a vector of 2 IDs that use the same expr
#
# @param fpd A flat parsed data data.frame .
# @param id Numeric indicating the node ID of the env expression.
# @param n_values Numeric indicating the minimum number of values to consider
#   a subexpression.
#
get_common_subexprs <- function(fpd, id, n_values) {
  # get all subexprs (dont have assignment, while, if, etc)
  env_exprs_ids <- fpd[fpd$token == "expr", "id"]
  env_subexprs <-
    do.call(rbind, lapply(env_exprs_ids, function(env_exprs_id) {
      aux_fpd <- get_children(fpd, env_exprs_id)
      res <- NULL
      if (all(aux_fpd$token %in%
              c(constants, ops, precedence_ops, "expr", "SYMBOL")) &&
          sum(aux_fpd$token %in% c(constants, "SYMBOL")) >= n_values) {
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
# @param n_values Numeric indicating the minimum number of values to consider
#   a subexpression.
#
subexpr_elim <- function(fpd, ids, n_values) {
  if (sum(get_children(fpd, ids[[1]])$token %in% c(constants, "SYMBOL")) <
      n_values) {
    return(fpd)
  }

  # get subexprs parents
  subexprs_parents <- lapply(ids, function(id) get_all_parents(fpd, id))
  # get the first parent in common
  common_parents <- Reduce(intersect, subexprs_parents)
  common_parent <- common_parents[[1]]

  # get place where temp var should go (we assume pos_ids are ordered; temp
  # will go before the following expr)
  fst_expr_place <- get_temp_var_pos(fpd, subexprs_parents[[1]], common_parents)

  # get split points
  splitted_ids <- split_ids(fpd, common_parent, fst_expr_place$pos_id, ids)

  res <- fpd
  if (length(splitted_ids) == 1 && length(splitted_ids[[1]]) == length(ids)) {
    # do the subexpr replacement
    res <- apply_subexpr_elim(fpd, common_parent, fst_expr_place$id, ids)
  } else {
    for (act_ids in splitted_ids) {
      res <- subexpr_elim(res, act_ids, n_values)
    }
  }
  res
}

# Splits the ids in which common subexprs might be split due to involved vars
# assignation or function calls
#
# @param fpd A flat parsed data data.frame .
# @param parent_id Numeric indicating the node ID of the subexprs common parent.
# @param fst_expr_pos_id Numeric indicating the pos_id where temp var should go.
# @param ids Numeric vector indicating common subexprs IDs.
#
split_ids <- function(fpd, parent_id, fst_expr_pos_id, ids) {
  # get vars involved in the subexprs
  vars_inv <- get_children(fpd, ids[[1]])
  vars_inv <- vars_inv[vars_inv$token == "SYMBOL", "text"]

  # get vars involved assignations
  vars_inv_fpd <- fpd[fpd$id %in% get_assigns_ids(fpd, parent_id), ]
  vars_inv_fpd <- vars_inv_fpd[vars_inv_fpd$text %in% vars_inv, ]

  # get functions used in this env
  funs_fpd <- fpd[fpd$id %in% get_fun_call_ids(fpd, parent_id), ]

  # set them as trouble points
  split_points <- rbind(vars_inv_fpd, funs_fpd)

  # move one pos those splits that are vars_inv <- cs_
  split_points$pos_id <- sapply(seq_len(nrow(split_points)), function(i) {
    act_split <- split_points[i, ]
    res <- act_split$pos_id
    aux_fpd <- get_children(fpd, act_split$parent)
    subexprs <- ids[ids %in% aux_fpd$id]
    if (length(subexprs) > 0) {
      res <- max(aux_fpd$pos_id[aux_fpd$id %in% subexprs]) + 10e-5
    }
    res
  })

  # if split points are into loops then add a new split point at loop start
  split_points <- do.call(
    rbind, lapply(seq_len(nrow(split_points)), function(i) {
      res <- split_points[i, ]
      act_sblngs <- fpd[fpd$parent %in% get_all_parents(fpd, res$id), ]
      res <- rbind(res, act_sblngs[act_sblngs$token %in% loops, ])
      res
    }))

  # split subexpr ids
  cs_pos <- fpd$pos_id[fpd$id %in% ids]
  split_pos <- unique(split_points$pos_id)
  id_splits <- split(ids, cut(cs_pos, c(-Inf, split_pos, Inf)))
  id_splits[sapply(id_splits, length) > 1]
}

# Creates a new fpd where the common subexpr has been applied
#
# @param fpd A flat parsed data data.frame .
# @param parent_id Numeric indicating the node ID of the subexprs common parent.
# @param fst_expr_id Numeric indicating the id where temp var should go.
# @param ids Numeric vector indicating common subexprs IDs.
#
apply_subexpr_elim <- function(fpd, parent_id, fst_expr_id, ids) {
  # create temp var fpd
  new_var <- create_temp_var(fpd, parent_id, fst_expr_id, ids)
  res_fpd <- rbind(fpd, new_var$fpd)

  # replace temp var in common subexprs
  repl_fpd <- flatten_leaves(parse_flat_data(new_var$name))
  for (act_id in ids) {
    res_fpd <- rbind(
      remove_nodes(res_fpd, act_id),
      replace_pd(get_children(res_fpd, act_id), repl_fpd)
    )
  }
  res_fpd[order(res_fpd$pos_id), ]
}

# Create a fpd for the temp var that is assigned the subexpr
#
# @param fpd A flat parsed data data.frame .
# @param parent_id Numeric indicating the node ID of the subexprs common parent.
# @param fst_expr_id Numeric indicating the id where temp var should go.
# @param ids Numeric vector indicating common subexprs IDs.
#
create_temp_var <- function(fpd, parent_id, fst_expr_id, ids) {
  # create temp var name, and assignation with the corresponding subexpr
  var_name <- create_new_var(fpd)
  var_expr <- deparse_flat_data(get_children(fpd, ids[[1]]))
  var_expr <- sub("^\n*", "", var_expr)
  var <- paste0(var_name, " <- ", var_expr)
  # create the fpd
  var_fpd <- flatten_leaves(parse_flat_data(var))
  # fix ids and parents
  var_fpd$id <- paste0(var_name, "_", var_fpd$id)
  var_fpd$parent <- paste0(var_name, "_", var_fpd$parent)
  var_fpd$parent[var_fpd$id == get_roots(var_fpd)$id] <- parent_id

  var_fpd[nrow(var_fpd), "next_lines"] <- 1
  fst_expr_fpd <- get_children(fpd, fst_expr_id)
  var_fpd[var_fpd$terminal, "prev_spaces"][[1]] <-
    fst_expr_fpd[fst_expr_fpd$terminal, "prev_spaces"][[1]]
  var_fpd$pos_id <- create_new_pos_id(fpd, nrow(var_fpd), to_id = fst_expr_id)
  list(fpd = var_fpd, name = var_name)
}

# Returns the fpd row where new temp var should go before
#
# @param fpd A flat parsed data data.frame .
# @param fst_expr_prnts Numeric vector with the parents ID of the first subexpr.
# @param common_parents Numeric vector with the IDs of the subexprs common
#   parents.
#
get_temp_var_pos <- function(fpd, fst_expr_prnts, common_parents) {
  # remove common parents that are function call
  fun_call_prnts <- which(sapply(common_parents, function(comn_prnt) {
    comn_prnt_fpd <- fpd[fpd$parent %in% comn_prnt, ]
    "SYMBOL_FUNCTION_CALL" %in% comn_prnt_fpd$token ||
      # case when we have pkg::fun(...)
      (all(comn_prnt_fpd$token[1:2] == c("expr", "'('")) &&
         "SYMBOL_FUNCTION_CALL" %in% fpd$token[
           fpd$parent == comn_prnt_fpd$id[[1]]])
  }))
  fst_parent <- common_parents[[1]]
  if (length(fun_call_prnts) > 0) {
    fst_parent <- common_parents[max(fun_call_prnts) + 1]
  }
  fpd[fpd$id == fst_expr_prnts[which(fst_expr_prnts == fst_parent) - 1], ]
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
