#' Optimizer: Dead Code Elimination.
#'
#' Performs one dead code elimination pass.
#' Carefully examine the results after running this function!
#'
#' @param texts A list of character vectors with the code to optimize.
#'
#' @examples
#' code <- paste(
#'   "while (TRUE) {",
#'   "  break",
#'   "  dead_code()",
#'   "}",
#'   sep = "\n"
#' )
#' cat(opt_dead_code(list(code))$codes[[1]])
#' @export
#'
opt_dead_code <- function(texts) {
  res <- list()
  res$codes <- lapply(texts, dc_one_file)
  res
}

# Executes dead code elimination on one text of code.
#
# @param text A character vector with code to optimize.
#
dc_one_file <- function(text) {
  fpd <- parse_text(text)
  fpd <- flatten_leaves(fpd)
  res_fpd <- fpd[fpd$parent < 0, ] # keep lines with just comments
  new_fpd <- fpd[fpd$parent >= 0, ] # keep lines with just comments
  new_fpd <- dc_one_fpd(new_fpd)
  res_fpd <- rbind(res_fpd, new_fpd)
  if (nrow(res_fpd) > 0) {
    res_fpd <- res_fpd[order(res_fpd$pos_id), ]
  }
  deparse_data(res_fpd)
}

# Executes dead code elimination of a fpd.
#
# @param fpd A flatten parsed data data.frame.
#
dc_one_fpd <- function(fpd) {
  # first remove code that is after (and equally nested) next, break, or return
  new_fpd <- remove_after_interruption(fpd)

  # work on constant `while` and `if` conditions
  new_fpd <- remove_constant_conds(new_fpd)

  new_fpd
}

# Returns a new fpd where equally nested code after interruption commands is
# removed ( break, next, return(...) ).
# Assumes `return` base function has not been overwritten.
#
# @param fpd A flatten parsed data data.frame.
#
remove_after_interruption <- function(fpd) {
  res_fpd <- fpd
  # get nodes that are interruption commands
  # get function def bodies
  fun_def_ids <- fpd$parent[fpd$token == "FUNCTION"]
  fun_body_ids <- sapply(fun_def_ids, function(act_id) {
    utils::tail(fpd$id[fpd$parent == act_id], 1)
  })
  # get returns inside fun defs
  ret_ids <- sapply(fun_body_ids, function(act_id) {
    act_fpd <- get_children(fpd, act_id)
    # remove function calls that are not returns
    act_fpd <- remove_nodes(act_fpd, act_fpd$parent[
      act_fpd$token == "SYMBOL_FUNCTION_CALL" & act_fpd$text != "return"
    ])
    # get only returns not in fun calls
    act_fpd$id[
      act_fpd$token == "SYMBOL_FUNCTION_CALL" & act_fpd$text == "return"
    ]
  })
  return_calls <- fpd[fpd$id %in% ret_ids, ]

  # `return` parent is an expression
  intr <- fpd[fpd$id %in% return_calls$parent, ]
  intr <- rbind(intr, fpd[fpd$token %in% c("BREAK", "NEXT"), ])

  # for each interruption parent, delete children after interruption
  for (i in seq_len(nrow(intr))) {
    if (!intr$id[[i]] %in% res_fpd$id) {
      next
    }
    id <- intr$parent[[i]]
    intr_sibl <- res_fpd[res_fpd$parent == id, ]
    if (nrow(intr_sibl) > 5 &&
      all(intr_sibl$token[c(1, 6)] == c("IF", "ELSE"))) {
      # if these are intr siblings, then it is something like
      # if (cond) return(...) else ...
      # so dont remove
      next
    }

    keep_ids <- intr_sibl[seq_len(which(intr_sibl$id == intr$id[[i]])), "id"]
    # for each opening precedence op, keep one closing
    prec_sibl <- intr_sibl[intr_sibl$token %in% precedence_ops, "id"]
    keep_ids <- c(
      keep_ids,
      rev(rev(prec_sibl)[seq_len(sum(keep_ids %in% prec_sibl))])
    )
    remove_ids <- setdiff(intr_sibl$id, keep_ids)

    if (length(remove_ids) == 0) {
      next
    }

    # to keep last terminal spaces and new lines
    keep_last_id <- intr$id[[i]]
    remove_fpd <- get_children(res_fpd, remove_ids)
    remove_last_id <- utils::tail(remove_fpd$id[remove_fpd$terminal], 1)

    res_fpd <- remove_nodes(res_fpd, remove_ids)

    # put last terminal spaces and new lines
    res_fpd$next_spaces[res_fpd$id == keep_last_id] <-
      remove_fpd$next_spaces[remove_fpd$id == remove_last_id]
    res_fpd$next_lines[res_fpd$id == keep_last_id] <-
      remove_fpd$next_lines[remove_fpd$id == remove_last_id]
  }
  res_fpd
}

# Returns a new fpd where constant conditionals were replaced
# ( if(TRUE), while (FALSE), etc ).
#
# @param fpd A flatten parsed data data.frame.
#
remove_constant_conds <- function(fpd) {
  new_fpd <- remove_false_while(fpd)
  new_fpd <- remove_false_if(new_fpd)
  new_fpd <- remove_true_if(new_fpd)
  new_fpd
}

# Returns a new fpd where `while (FALSE) { EXPR }` were removed.
#
# @param fpd A flatten parsed data data.frame.
#
remove_false_while <- function(fpd) {
  res_fpd <- fpd
  # get nodes that are `while`, and their parent
  while_nodes <- fpd[fpd$token == "WHILE", ]

  # for each interruption parent, delete children after interruption
  for (i in seq_len(nrow(while_nodes))) {
    if (!while_nodes$id[[i]] %in% res_fpd$id) {
      next
    }
    id <- while_nodes$parent[[i]]
    intr_sibl <- res_fpd[res_fpd$parent == id, ]
    # WHILE '(' expr ')' expr_or_assign
    if (is_constant_or_minus(res_fpd, intr_sibl$id[[3]]) &&
      !eval(parse(text = intr_sibl$text[[3]]))) {
      # the condition was a constant and evaluated to FALSE
      res_fpd <- remove_nodes(res_fpd, id)
    }
  }
  res_fpd
}

# Returns a new fpd where `if (FALSE) { EXPR }` were removed.
#
# @param fpd A flatten parsed data data.frame.
#
remove_false_if <- function(fpd) {
  res_fpd <- fpd
  # get nodes that are `if`
  if_nodes <- fpd[fpd$token == "IF", ]

  # for each interruption parent, delete children after interruption
  for (i in seq_len(nrow(if_nodes))) {
    if (!if_nodes$id[[i]] %in% res_fpd$id) {
      next
    }
    id <- if_nodes$parent[[i]]
    intr_sibl <- res_fpd[res_fpd$parent == id, ]
    # IF '(' expr ')' expr_or_assign ELSE expr_or_assign
    if (is_constant_or_minus(res_fpd, intr_sibl$id[[3]]) &&
      !eval(parse(text = intr_sibl$text[[3]]))) {
      # the condition was a constant and evaluated to FALSE
      res_fpd <- remove_nodes(res_fpd, id)
      # if it has an else statement, then keep only its `expr`
      else_fpd <- get_ifelse_expr(fpd, id)
      res_fpd <- rbind(res_fpd, else_fpd)
      res_fpd <- res_fpd[order(res_fpd$pos_id), ]
    }
  }
  res_fpd
}

# Returns a new fpd where `if (TRUE) { EXPR }` were replaced by EXPR.
#
# @param fpd A flatten parsed data data.frame.
#
remove_true_if <- function(fpd) {
  res_fpd <- fpd
  # get nodes that are `if`
  if_nodes <- fpd[fpd$token == "IF", ]

  # for each interruption parent, delete children after interruption
  for (i in seq_len(nrow(if_nodes))) {
    if (!if_nodes$id[[i]] %in% res_fpd$id) {
      next
    }
    id <- if_nodes$parent[[i]]
    intr_sibl <- res_fpd[res_fpd$parent == id, ]
    # IF '(' expr ')' expr_or_assign ELSE expr_or_assign
    if (is_constant_or_minus(res_fpd, intr_sibl$id[[3]]) &&
      eval(parse(text = intr_sibl$text[[3]]))) {
      # the condition was a constant and evaluated to TRUE
      res_fpd <- remove_nodes(res_fpd, id)
      # if it has an else statement, then keep only its `expr`
      if_fpd <- get_ifelse_expr(fpd, id, get_if = TRUE)
      res_fpd <- rbind(res_fpd, if_fpd)
      res_fpd <- res_fpd[order(res_fpd$pos_id), ]
    }
  }
  res_fpd
}

# Returns a new fpd where the if/else was replaced by its expr.
#
# @param fpd A flatten parsed data data.frame.
# @param id A numeric indicating the node ID of the expr that contains the
#   if/else.
# @param get_if A logical indicating if the `if` should be obtained (`else`
#   otherwise).
#
get_ifelse_expr <- function(fpd, id, get_if = FALSE) {
  token <- ifelse(get_if, "IF", "ELSE")
  pos <- ifelse(get_if, 5, 7)
  res_fpd <- NULL
  act_fpd <- get_children(fpd, id)
  fst_child <- act_fpd[act_fpd$parent == id, ]
  if (get_if ||
    (nrow(fst_child) > 5 && fst_child$token[[6]] == "ELSE")) {
    # change `expr` parent by `if () { expr } else expr` parent
    if_fpd <- get_children(act_fpd, fst_child$id[[pos]])
    if_expr_fpd <- if_fpd[if_fpd$parent == fst_child$id[[pos]], ]
    if (nrow(if_expr_fpd) > 1 &&
      if_expr_fpd$token[[1]] == "'{'" &&
      if_expr_fpd$token[[nrow(if_expr_fpd)]] == "'}'") {
      # remove `{}` from if/else expr
      if_fpd <- get_children(
        act_fpd,
        if_expr_fpd$id[-c(1, nrow(if_expr_fpd))]
      )
    }
    if (nrow(if_fpd) > 0) {
      # check if it was not an empty expr, i.e., if(cond) {}
      if_fpd[if_fpd$id == fst_child$id[[pos]], "parent"] <-
        act_fpd[act_fpd$id == id, "parent"]
      if_fpd <- unindent_fpd(
        if_fpd,
        fst_child[fst_child$token == token, "prev_spaces"]
      )
      # copy previous text next spaces and lines
      if_fpd[nrow(if_fpd), c("next_spaces", "next_lines")] <-
        act_fpd[nrow(act_fpd), c("next_spaces", "next_lines")]
    }
    res_fpd <- if_fpd
  }
  res_fpd
}

# Returns a new fpd where new line terminals start at the same position as their
# parent.
#
# @param fpd A flatten parsed data data.frame.
# @param parent_spaces A numeric indicating prev_spaces the parent had.
#
unindent_fpd <- function(fpd, parent_spaces) {
  # get which are the terminals that start in a new line
  fpd_terms <- fpd[fpd$terminal, ]
  prnt_diff <- max(0, fpd_terms[1, "prev_spaces"] - parent_spaces)
  new_line_ids <- fpd_terms[!duplicated(fpd_terms$line1), "id"]
  # and remove the identation between them and parent
  fpd[fpd$id %in% new_line_ids, "prev_spaces"] <-
    fpd[fpd$id %in% new_line_ids, "prev_spaces"] - prnt_diff
  fpd
}
