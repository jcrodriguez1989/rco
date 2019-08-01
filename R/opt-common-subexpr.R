#' Optimizer: Common Subexpression Elimination.
#'
#' Performs one common subexpression elimination pass.
#' Carefully examine the results after running this function!
#'
#' @param texts A list of character vectors with the code to optimize.
#' @param n_values A numeric indicating the minimum number of values to consider
#'   a subexpression.
#' @param in_fun_call A logical indicating whether it should propagate in
#'   function calls. Note: this could change the semantics of the program.
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
opt_common_subexpr <- function(texts, n_values = 2, in_fun_call = FALSE) {
  # todo: add functions as common subexpression?
  # this can have an issue if function returns random values, i.e.,
  # a <-rnorm(1) * 8; b <-rnorm(1) * 18 , will wrongly optimize to
  # tmp <- rnorm(1); a <-tmp * 8; b <-tmp * 18
  # todo: check which functions modify the parent env. In this way, function
  # calls wont stop optimization
  res <- list()
  res$codes <- lapply(texts, cs_one_file,
    n_values = n_values,
    in_fun_call = in_fun_call
  )
  res
}

# Executes common subexpression elimination on one file of code.
#
# @param text A character vector with code to optimize.
# @param n_values A numeric indicating the minimum number of values to consider
#   a subexpression.
# @param in_fun_call A logical indicating whether it should propagate in
#   function calls. Note: this could change the semantics of the program.
#
cs_one_file <- function(text, n_values, in_fun_call) {
  fpd <- parse_text(text)
  fpd <- flatten_leaves(fpd)
  res_fpd <- fpd[fpd$parent < 0, ] # keep lines with just comments
  new_fpd <- fpd[fpd$parent >= 0, ] # keep lines with just comments
  new_fpd <- cs_one_fpd(new_fpd, n_values, in_fun_call)
  res_fpd <- rbind(res_fpd, new_fpd)
  if (nrow(res_fpd) > 0) {
    res_fpd <- res_fpd[order(res_fpd$pos_id), ]
  }

  deparse_data(res_fpd)
}

# Executes common subexpression elimination of a fpd.
#
# @param fpd A flatten parsed data data.frame.
# @param n_values A numeric indicating the minimum number of values to consider
#   a subexpression.
# @param in_fun_call A logical indicating whether it should propagate in
#   function calls. Note: this could change the semantics of the program.
#
cs_one_fpd <- function(fpd, n_values, in_fun_call) {
  res_fpd <- fpd

  # get different envs (parent env and function defs)
  env_parent_ids <- unique(
    fpd$parent[fpd$token == "FUNCTION" | fpd$parent <= 0]
  )

  # for each env do the common subexpr elimination
  for (env_parent_id in env_parent_ids) {
    res_fpd <- common_subexpr_in_env(
      res_fpd, env_parent_id, n_values,
      in_fun_call
    )
  }

  res_fpd
}

# Executes common subexpr elimination in the expr of an env.
#
# @param fpd A flatten parsed data data.frame.
# @param id A numeric indicating the node ID of the env expression.
# @param n_values A numeric indicating the minimum number of values to consider
#   a subexpression.
# @param in_fun_call A logical indicating whether it should propagate in
#   function calls. Note: this could change the semantics of the program.
#
common_subexpr_in_env <- function(fpd, id, n_values, in_fun_call) {
  res_fpd <- remove_nodes(fpd, id)
  act_fpd <- get_children(fpd, id)
  # get and remove function definitions (as they have own env)
  fun_def_prnt_ids <- act_fpd$parent[act_fpd$token == "FUNCTION"]
  fun_def_prnt_ids <- setdiff(fun_def_prnt_ids, id)
  env_fpd <- remove_nodes(act_fpd, fun_def_prnt_ids)

  # if it is a function def, then dont CSE in parameters
  if ("FUNCTION" %in% act_fpd$token[act_fpd$parent == id]) {
    id <- utils::tail(env_fpd$id[env_fpd$parent == id], 1)
  }

  # get common subexpressions within an env
  common_subexprs_ids <- get_common_subexprs(env_fpd, id, n_values, in_fun_call)

  for (i in seq_along(common_subexprs_ids)) {
    act_ids <- common_subexprs_ids[[i]]
    if (!all(act_ids %in% env_fpd$id)) {
      next
    }
    env_fpd <- subexpr_elim(env_fpd, act_ids, n_values)
  }

  res_fpd <- rbind(
    res_fpd, env_fpd,
    unique(get_children(act_fpd, fun_def_prnt_ids))
  )
  res_fpd[order(res_fpd$pos_id), ]
}

# Returns a list, where each field is a vector of 2 IDs that use the same expr.
#
# @param fpd A flatten parsed data data.frame.
# @param id A numeric indicating the node ID of the env expression.
# @param n_values A numeric indicating the minimum number of values to consider
#   a subexpression.
# @param in_fun_call A logical indicating whether it should propagate in
#   function calls. Note: this could change the semantics of the program.
#
get_common_subexprs <- function(fpd, id, n_values, in_fun_call) {
  # get all subexprs (dont have assignment, while, if, etc)
  act_fpd <- get_children(fpd, id)
  if (!in_fun_call) {
    act_fpd <- remove_nodes(act_fpd, act_fpd$parent[
      act_fpd$token == "SYMBOL_FUNCTION_CALL"
    ])
  }

  env_exprs_ids <- act_fpd$id[act_fpd$token == "expr"]
  env_subexprs <-
    do.call(rbind, lapply(env_exprs_ids, function(env_exprs_id) {
      aux_fpd <- get_children(act_fpd, env_exprs_id)
      res <- NULL
      if (all(aux_fpd$token %in%
        c(constants, ops, precedence_ops, "expr", "SYMBOL")) &&
        sum(aux_fpd$token %in% c(constants, "SYMBOL")) >= n_values) {
        res <- c(
          expr_id = env_exprs_id,
          text = paste(aux_fpd$text[aux_fpd$terminal], collapse = " ")
        )
      }
      res
    }))

  # get common subexpressions, and return a list with their ids
  common_subexprs <- env_subexprs[duplicated(env_subexprs[, "text"]), ,
    drop = FALSE
  ]
  res <- lapply(unique(common_subexprs[, "text"]), function(act_common) {
    env_subexprs[env_subexprs[, "text"] == act_common, "expr_id"]
  })

  res[order(sapply(res, length), decreasing = TRUE)]
}

# If possible, returns a new fpd where common subexpressions were replaced by a
# new `tmp` variable.
#
# @param fpd A flatten parsed data data.frame.
# @param ids A numeric vector indicating the node IDs of each common subexpr.
# @param n_values A numeric indicating the minimum number of values to consider
#   a subexpression.
#
subexpr_elim <- function(fpd, ids, n_values) {
  if (sum(get_children(fpd, ids[[1]])$token %in% c(constants, "SYMBOL")) <
    n_values) {
    return(fpd)
  }

  # get subexprs parents
  subexprs_parents <- lapply(ids, function(id) get_ancestors(fpd, id))
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
# assignation or function calls.
#
# @param fpd A flatten parsed data data.frame.
# @param parent_id A numeric indicating the node ID of the subexprs common
#   parent.
# @param fst_expr_pos_id A numeric indicating the pos_id where temp var should
#   go.
# @param ids A numeric vector indicating common subexprs IDs.
#
split_ids <- function(fpd, parent_id, fst_expr_pos_id, ids) {
  # get vars involved in the subexprs
  vars_inv <- get_children(fpd, ids[[1]])
  vars_inv <- vars_inv$text[vars_inv$token == "SYMBOL"]

  # get vars involved assignations
  vars_inv_fpd <- fpd[fpd$id %in% get_assigned_vars_ids(fpd, parent_id), ]
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
      res <- max(aux_fpd$pos_id[aux_fpd$id %in% subexprs]) + 10e-4
    }
    res
  })

  # if split points are into loops then add a new split point at loop start
  split_points <- do.call(
    rbind, lapply(seq_len(nrow(split_points)), function(i) {
      res <- split_points[i, ]
      act_sblngs <- fpd[fpd$parent %in% get_ancestors(fpd, res$id), ]
      res <- rbind(res, act_sblngs[act_sblngs$token %in% loops, ])
      res
    })
  )

  # split subexpr ids
  cs_pos <- fpd$pos_id[fpd$id %in% ids]
  split_pos <- unique(split_points$pos_id)
  id_splits <- split(ids, cut(cs_pos, c(-Inf, split_pos, Inf)))
  id_splits[sapply(id_splits, length) > 1]
}

# Creates a new fpd where the common subexpr has been applied.
#
# @param fpd A flatten parsed data data.frame.
# @param parent_id A numeric indicating the node ID of the subexprs common
#   parent.
# @param fst_expr_id A numeric indicating the ID where temp var should go.
# @param ids A numeric vector indicating common subexprs IDs.
#
apply_subexpr_elim <- function(fpd, parent_id, fst_expr_id, ids) {
  # add braces if it was a function def, and it did not have
  fpd <- add_braces(fpd, fst_expr_id)

  # create temp var fpd
  new_var <- create_temp_var(fpd, parent_id, fst_expr_id, ids)
  res_fpd <- rbind(fpd, new_var$fpd)

  # replace temp var in common subexprs
  repl_fpd <- flatten_leaves(parse_text(new_var$name))
  for (act_id in ids) {
    res_fpd <- rbind(
      remove_nodes(res_fpd, act_id),
      replace_pd(get_children(res_fpd, act_id), repl_fpd)
    )
  }

  res_fpd[order(res_fpd$pos_id), ]
}

# Create a fpd for the temp var that is assigned the subexpr.
#
# @param fpd A flatten parsed data data.frame.
# @param parent_id A numeric indicating the node ID of the subexprs common
#   parent.
# @param fst_expr_id A numeric indicating the ID where temp var should go.
# @param ids A numeric vector indicating common subexprs IDs.
#
create_temp_var <- function(fpd, parent_id, fst_expr_id, ids) {
  # create temp var name, and assignation with the corresponding subexpr
  var_name <- create_new_var(fpd)
  var_expr <- deparse_data(get_children(fpd, ids[[1]]))
  var_expr <- sub("^\n*", "", var_expr)
  var <- paste0(var_name, " <- ", var_expr)
  # create the fpd
  var_fpd <- flatten_leaves(parse_text(var))
  # fix ids and parents
  var_fpd$id <- paste0(var_name, "_", var_fpd$id)
  var_fpd$parent <- paste0(var_name, "_", var_fpd$parent)
  var_fpd$parent[var_fpd$id == get_roots(var_fpd)$id] <- parent_id

  var_fpd$next_lines[nrow(var_fpd)] <- 1
  fst_expr_fpd <- get_children(fpd, fst_expr_id)
  var_fpd$prev_spaces[var_fpd$terminal][[1]] <-
    fst_expr_fpd$prev_spaces[fst_expr_fpd$terminal][[1]]
  var_fpd$pos_id <- create_new_pos_id(
    fpd, nrow(var_fpd),
    from_id = "from_start", to_id = fst_expr_id
  )
  list(fpd = var_fpd, name = var_name)
}

# Returns and fpd where its node body has been embraced if it was not.
#
# @param fpd A flatten parsed data data.frame.
# @param id A numeric indicating the node ID of the body.
#
add_braces <- function(fpd, id) {
  node_sblngs <- fpd[fpd$parent == fpd$parent[fpd$id == id], ]
  # if fpd new var goes in a function def with no '{', '}', then add them
  if (!any(c(loops, "IF", "ELSE", "FUNCTION") %in% node_sblngs$token)) {
    return(fpd)
  }

  if ("'{'" %in% fpd$token[fpd$parent == id]) {
    return(fpd)
  }

  res <- rbind(
    remove_nodes(fpd, id),
    add_braces_to_expr(fpd, id)
  )
  res[order(res$pos_id), ]
}

# Returns and fpd whichs expr has been embraced.
#
# @param fpd A flatten parsed data data.frame.
# @param id A numeric indicating the node ID of the expr.
#
add_braces_to_expr <- function(fpd, id) {
  # get old code, add braces and parse the fpd again
  old_fpd <- get_children(fpd, id)
  new_code <- deparse_data(old_fpd)
  new_code <- paste0("{\n  ", sub("^\n*", "", new_code), "\n}")
  new_fpd <- flatten_leaves(parse_text(new_code))

  new_fpd <- replace_pd(old_fpd, new_fpd)
  # add spaces to braces
  new_fpd$prev_spaces[new_fpd$parent == id & new_fpd$token == "'}'"] <-
    new_fpd$prev_spaces[new_fpd$parent == id & new_fpd$token == "'{'"]
  # fix ids to keep the old ones
  old_fpd_rows <- which(new_fpd$id == id |
    (new_fpd$parent == id & new_fpd$token != "expr"))
  new_fpd$id[old_fpd_rows] <- paste0("emb_", id, "_", c("expr", "{", "}"))
  new_fpd$id[-old_fpd_rows] <- old_fpd$id
  new_fpd$parent[-old_fpd_rows] <- old_fpd$parent
  new_fpd$parent[new_fpd$id == id] <- paste0("emb_", id, "_", "expr")
  new_fpd$parent[new_fpd$id %in% paste0("emb_", id, "_", c("{", "}"))] <-
    paste0("emb_", id, "_", "expr")

  # fix pos ids
  new_fpd$pos_id[-old_fpd_rows] <- old_fpd$pos_id
  new_fpd$pos_id[1:2] <- create_new_pos_id(old_fpd, 2,
    from_id = old_fpd$id[[1]]
  )
  new_fpd$pos_id[3] <- create_new_pos_id(old_fpd, 1, to_id = old_fpd$id[[2]])
  new_fpd$pos_id[nrow(new_fpd)] <-
    create_new_pos_id(old_fpd, 1, from_id = utils::tail(old_fpd$id, 1))
  new_fpd
}

# Returns the fpd row where new temp var should go before.
#
# @param fpd A flatten parsed data data.frame.
# @param fst_expr_prnts A numeric vector with the parents ID of the first
#   subexpr.
# @param common_parents A numeric vector with the IDs of the subexprs common
#   parents.
#
get_temp_var_pos <- function(fpd, fst_expr_prnts, common_parents) {
  # get ids which childs can be sequence of exprs
  exprlist_ids <- unique(c(
    # parent env
    fpd$parent[fpd$parent == 0],
    # sub envs
    unlist(sapply(
      fpd$parent[fpd$token %in% c("FUNCTION", loops)],
      function(act_id) utils::tail(fpd$id[fpd$parent == act_id], 1)
    )),
    unlist(sapply(
      fpd$parent[fpd$token == "IF"],
      function(act_id) {
        if ("ELSE" %in% fpd$token[fpd$parent == act_id]) {
          utils::tail(fpd$id[fpd$parent == act_id & fpd$token != "ELSE"], 2)
        } else {
          utils::tail(fpd$id[fpd$parent == act_id], 1)
        }
      }
    )),
    fpd$parent[fpd$token == "'{'"],
    fpd$id[fpd$token == "exprlist"]
  ))

  fst_parent <- intersect(common_parents, exprlist_ids)[[1]]
  res <- fpd[fpd$id %in% fst_expr_prnts & fpd$parent == fst_parent, ]
  if (res$parent != 0 && !any(c("exprlist", "'{'", "';'") %in%
    fpd$token[fpd$parent == res$parent])) {
    res <- fpd[fpd$id %in% fst_expr_prnts & fpd$id == fst_parent, ]
  }
  res
}

# Returns the ids of the fpd SYMBOLs that are being assigned.
#
# @param fpd A flatten parsed data data.frame.
# @param id A numeric indicating the node ID to find assigns.
#
get_assigned_vars_ids <- function(fpd, id) {
  act_fpd <- get_children(fpd, id)
  # get parents of <- <<- -> ->> and =
  assign_exprs_prnts <- act_fpd[
    act_fpd$token %in% assigns & act_fpd$text != ":=",
    "parent"
  ]
  # get the assigned SYMBOL fpd id
  unlist(lapply(assign_exprs_prnts, function(assign_exprs_prnt) {
    aux <- act_fpd[act_fpd$parent == assign_exprs_prnt, ]
    if (aux$token[[2]] == "RIGHT_ASSIGN") {
      res <- get_children(act_fpd, aux$id[3])
    } else {
      res <- get_children(act_fpd, aux$id[1])
    }
    res$id[res$token == "SYMBOL"][[1]] # in case it is a[i] <- ...
  }))
}

# Returns the ids of function calls.
#
# @param fpd A flatten parsed data data.frame.
# @param id A numeric indicating the node ID to find fun calls.
#
get_fun_call_ids <- function(fpd, id) {
  act_fpd <- get_children(fpd, id)
  act_fpd$id[act_fpd$token == "SYMBOL_FUNCTION_CALL"]
}

# Creates a new var name not used before in the fpd.
#
# @param fpd A flatten parsed data data.frame.
# @param prefix A character vector indicating the prefix of the new name.
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
