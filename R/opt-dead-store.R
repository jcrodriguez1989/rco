#' Optimizer: Dead Store Elimination
#'
#' Performs one dead store elimination pass.
#' Carefully examine the results after running this function!
#'
#' @param texts A list of character vectors with the code to optimize.
#'
#' @examples
#' code <- paste(
#'   "foo <- function() {",
#'   "  x <- 128 ^ 2",
#'   "  return(TRUE)",
#'   "}",
#'   sep = "\n"
#' )
#' cat(opt_dead_store(list(code))$codes[[1]])
#' @export
#'
opt_dead_store <- function(texts) {
  # todo: implement intelligent dead store? for example:
  # a <- 2; a <- 3; return(a) # remove first assign
  # todo: remove all variables that do not affect the returned value
  res <- list()
  res$codes <- lapply(texts, dead_store_one)
  return(res)
}

# Executes dead store elimination on one text of code
#
# @param text A character vector with code to optimize.
#
dead_store_one <- function(text) {
  fpd <- parse_flat_data(text, include_text = TRUE)
  fpd <- flatten_leaves(fpd)
  res_fpd <- fpd[fpd$parent < 0, ] # keep lines with just comments
  new_fpd <- fpd[fpd$parent >= 0, ] # keep lines with just comments

  # eliminate until no changes
  old_fpd <- NULL
  while (!isTRUE(all.equal(old_fpd, new_fpd))) {
    old_fpd <- new_fpd
    new_fpd <- one_dead_store(new_fpd)
  }
  res_fpd <- rbind(res_fpd, new_fpd)
  if (nrow(res_fpd) > 0) {
    res_fpd <- res_fpd[order(res_fpd$pos_id), ]
  }

  deparse_flat_data(res_fpd)
}

# Executes dead store elimination of a tree
#
# @param fpd A flat parsed data data.frame .
#
one_dead_store <- function(fpd) {
  res_fpd <- fpd
  # dead store happens only into functions, so get the expr of each function
  fun_ids <- get_ids_of_token(fpd, "FUNCTION")
  fun_prnt_ids <- fpd$parent[fpd$id %in% fun_ids]
  # get each function expression
  fun_expr_ids <- sapply(fun_prnt_ids, function(act_prnt_id) {
    rev(fpd$id[fpd$parent == act_prnt_id &
      fpd$token %in% c("expr", constants)])[[1]]
  })

  # for each function expr do a dead store removal
  for (id in fun_expr_ids) {
    if (!id %in% res_fpd$id) {
      next
    }
    act_fpd <- get_children(res_fpd, id)
    # if has a function call then we cant determine if vars are dead store or
    # not. As they can be used in these functions
    if (ods_has_function_call(act_fpd, id)) {
      next
    }

    # eliminate dead stores from a function, and replace it in res_fpd
    ds_elim_fun <- dead_store_in_fun(act_fpd)
    res_fpd <- rbind(
      remove_nodes(res_fpd, id),
      ds_elim_fun
    )
  }

  return(res_fpd)
}

# Executes dead store elimination in the expr of a function definition
#
# @param fpd A flat parsed data data.frame .
#
dead_store_in_fun <- function(fpd) {
  res_fpd <- fpd
  expr_id <- get_roots(fpd)$id

  # we are going to remove the variables that are assigned, but not used
  ass_vars <- ods_get_assigned_vars(fpd, expr_id)
  used_vars <- get_used_vars(fpd, expr_id)
  ass_to_remove <- setdiff(ass_vars, used_vars)

  for (act_var in ass_to_remove) {
    act_prnt_ids <- res_fpd[res_fpd$text == act_var, "parent"]
    for (act_prnt_id in act_prnt_ids) {
      # eliminate each assignation of the dead store
      if (!act_prnt_id %in% res_fpd$id) {
        next
      }
      ass_fpd <- get_children(res_fpd, act_prnt_id)
      new_ass_fpd <- ass_fpd
      act_prnt <- ass_fpd[ass_fpd$id == act_prnt_id, ]
      act_sblngs <- ass_fpd[ass_fpd$parent == act_prnt_id, ]

      # keep only the expression
      keep_fpd <- act_sblngs[3, ]
      if (act_sblngs$token[[2]] == "RIGHT_ASSIGN") {
        keep_fpd <- act_sblngs[1, ]
      }
      keep_fpd$parent <- act_prnt$parent
      new_ass_fpd <- new_ass_fpd[!new_ass_fpd$id %in%
        c(act_prnt_id, act_sblngs$id), ]
      new_ass_fpd <- rbind(new_ass_fpd, keep_fpd)
      new_ass_fpd <- new_ass_fpd[order(new_ass_fpd$pos_id), ]
      new_ass_fpd <- replace_pd(ass_fpd, new_ass_fpd)
      res_fpd <- rbind(
        remove_nodes(res_fpd, act_prnt_id),
        new_ass_fpd
      )
    }
  }
  return(res_fpd)
}

# Returns a logical indicating if a node has a function call different than
# `return`
#
# @param fpd a flat parsed data data.frame .
# @param id Numeric indicating the node ID.
#
ods_has_function_call <- function(fpd, id) {
  # todo: here, we could check if the function call is inside a function def
  # then we may not do a next. (research)
  act_fpd <- get_children(fpd, id)
  any(act_fpd$token == "SYMBOL_FUNCTION_CALL" & act_fpd$text != "return")
}

# Returns the names of the vars that are beign assigned in an expr
# `=` , `<-`, `->` . Discards `<<-` and `->>`
#
# @param fpd a flat parsed data data.frame .
# @param id Numeric indicating the node ID.
#
ods_get_assigned_vars <- function(fpd, id) {
  act_fpd <- get_children(fpd, id)
  ass_prnt_ids <- act_fpd[
    act_fpd$token %in% assigns &
      !act_fpd$text %in% c("<<-", "->>"),
    "parent"
  ]
  # return all the SYMBOL texts from the right/left of '->'/'<-','=' assinments
  res <- sapply(ass_prnt_ids, function(act_prnt) {
    ass_sblngs <- act_fpd[act_fpd$parent == act_prnt, ]
    if (ass_sblngs$token[[2]] == "RIGHT_ASSIGN") {
      res_fpd <- get_children(act_fpd, ass_sblngs$id[[3]])
    } else {
      res_fpd <- get_children(act_fpd, ass_sblngs$id[[1]])
    }
    return(res_fpd[res_fpd$token == "SYMBOL", "text"])
  })
  unique(res[res != ""])
}

# Returns the names of the vars that are beign used in an expr.
# Not counting assignations.
#
# @param fpd a flat parsed data data.frame .
# @param id Numeric indicating the node ID.
#
get_used_vars <- function(fpd, id) {
  act_fpd <- get_children(fpd, id)
  ass_prnt_ids <- act_fpd[act_fpd$token %in% assigns, "parent"]
  # remove SYMBOLs that are being assigned
  assigned_ids <- sapply(ass_prnt_ids, function(act_prnt) {
    ass_sblngs <- act_fpd[act_fpd$parent == act_prnt, ]
    if (ass_sblngs$token[[2]] == "RIGHT_ASSIGN") {
      res_fpd <- get_children(act_fpd, ass_sblngs$id[[3]])
    } else {
      res_fpd <- get_children(act_fpd, ass_sblngs$id[[1]])
    }
    # these ids must be removed
    return(res_fpd[res_fpd$token == "SYMBOL", "id"])
  })
  res <- act_fpd[
    act_fpd$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL") &
      !act_fpd$id %in% assigned_ids, "text"
  ]
  unique(res[res != ""])
}
