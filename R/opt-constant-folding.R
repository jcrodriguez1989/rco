#' Optimizer: Constant Folding
#'
#' Performs one constant folding pass.
#' Carefully examine the results after running this function!
#'
#' @param texts A list of character vectors with the code to optimize.
#' @param fold_floats Logical indicating if floating-point results should be
#'   folded (will reduce precision).
#' @param in_fun_call Logical indicating whether it should propagate in function
#'   calls. Note: this could change the semantics of the program.
#'
#' @examples
#' code <- paste(
#'   "i <- 320 * 200 * 32",
#'   "x <- i * 20 + 100",
#'   sep = "\n"
#' )
#' cat(opt_constant_folding(list(code))$codes[[1]])
#' @export
#'
opt_constant_folding <- function(texts, fold_floats = FALSE,
                                 in_fun_call = FALSE) {
  # todo: implement intelligent constant folding? for example: fold 0 * x to 0
  # todo: reorder vars in associativity?
  # todo: try constant fold known-functions with constants?
  res <- list()
  res$codes <- lapply(texts, constant_fold_one, fold_floats = fold_floats,
                      in_fun_call = in_fun_call)
  return(res)
}

# Performs a constant folding pass on one text.
#
# @param text A character vector with code to optimize.
# @param fold_floats Logical indicating if floating-point results should be
#   folded (will reduce precision).
# @param in_fun_call Logical indicating whether it should propagate in function
#   calls. Note: this could change the semantics of the program.
#
constant_fold_one <- function(text, fold_floats, in_fun_call) {
  pd <- parse_flat_data(text)
  pd <- flatten_leaves(pd)
  if (nrow(pd) > 0) {
    pd <- one_fold(pd, fold_floats, in_fun_call)
  }
  deparse_flat_data(pd)
}

# Performs the constant folding pass.
#
# @param pd A parse data data.frame with code to optimize.
# @param fold_floats Logical indicating if floating-point results should be
#   folded (will reduce precision).
# @param in_fun_call Logical indicating whether it should propagate in function
#   calls. Note: this could change the semantics of the program.
#
one_fold <- function(pd, fold_floats, in_fun_call) {
  # keep but dont fold comments ( < 0 )
  new_pd <- pd[pd$parent < 0, ]
  pd <- pd[pd$parent >= 0, ]

  if (!in_fun_call) {
    # remove function calls
    fun_calls_ids <- pd$parent[pd$token == "SYMBOL_FUNCTION_CALL"]
    new_pd <- rbind(new_pd, unique(get_children(pd, fun_calls_ids)))
    pd <- remove_nodes(pd, fun_calls_ids)
  }

  # start visiting root nodes
  visit_nodes <- get_roots(pd)$id
  while (length(visit_nodes) > 0) {
    new_visit_nodes <- c()
    for (act_parent in visit_nodes) {
      act_pd <- get_children(pd, act_parent)
      if (all(act_pd$token %in% c(constants, ops, precedence_ops, "expr")) &&
        !is_minus_constant(act_pd, act_parent)) {
        # all the children are terminals or ops. try to evaluate it
        # And it is not just -constant
        act_code_pd <- pd[pd$id == act_parent, ]
        folded_fpd <- get_folded_fpd(act_code_pd, fold_floats)
        if (!is.null(folded_fpd)) {
          # it is a constant or -constant
          # replace the parent expr by the new expr (folded)
          act_new_fpd <- replace_pd(act_pd, folded_fpd)
          new_pd <- rbind(new_pd, act_new_fpd)
          next
        }
      }
      # it could not be folded, so save parent, and terminal childs
      new_pd <- rbind(new_pd, pd[pd$id == act_parent, ])
      new_pd <- rbind(new_pd, pd[pd$parent == act_parent & pd$terminal, ])
      # continue visiting child exprs
      new_visit_nodes <- c(
        new_visit_nodes,
        pd[pd$parent == act_parent & !pd$terminal, "id"]
      )
    }
    visit_nodes <- new_visit_nodes
  }

  new_pd <- new_pd[order(new_pd$pos_id), ]
  return(new_pd)
}

# Returns a folded fpd, if it only had constants and operators.
# If it could not fold then it returns NULL
#
# @param fpd a flat parsed data data.frame .
# @param fold_floats Logical indicating if floating-point results should be
#   folded (will reduce precision).
#
get_folded_fpd <- function(fpd, fold_floats) {
  if (fpd$token %in% constants) {
    return(NULL)
  }

  eval_val <- try({
    eval(parse(text = fpd$text))
  }, silent = TRUE)
  if (inherits(eval_val, "try-error")) {
    return(NULL)
  }

  # it was correctly evaluated then create the fpd of the eval val
  eval_val_str <- eval_val
  if (is.null(eval_val)) {
    # there was a bug when eval_val was NULL
    eval_val_str <- "NULL"
  } else if (length(eval_val) == 0) {
    # there was a bug when eval_val was logical(0)
    eval_val_str <- deparse(eval_val)
  } else if (is.na(eval_val)) {
    # there was a bug when eval_val was NA
    eval_val_str <- na_to_correct_str(eval_val)
  } else if (is.character(eval_val)) {
    # there was a bug when evaluated `expr` returned a string
    eval_val_str <- deparse(eval_val)
  } else if (is.integer(eval_val)) {
    # if it is an integer, then dont remove the "L"
    eval_val_str <- paste0(eval_val, "L")
  }
  if (is.numeric(eval_val) && !is.na(eval_val) && eval_val < 0) {
    # put parentheses to negative numbers
    eval_val_str <- paste0("(", eval_val_str, ")")
  }

  res <- parse_flat_data(eval_val_str)
  res <- flatten_leaves(res)
  if (grepl("^\\{.+\\}$", fpd$text)) {
    # if it was `{expr}`, then add spaces in both sides
    # there was a bug when folding `if(TRUE){-3}else{NULL}`
    n_terms <- sum(res$terminal)
    res[res$terminal, ][1, "prev_spaces"] <- 1
    res[res$terminal, ][n_terms, "next_spaces"] <- 1
  }
  if (!all(res$token %in%
    c("expr", "'-'", "'('", "')'", constants, "SYMBOL_FUNCTION_CALL"))) {
    # SYMBOL_FUNCTION_CALL for logical(0)
    return(NULL)
  }

  # it is a constant or -constant
  if (!fold_floats && "NUM_CONST" %in% res$token && length(eval_val) != 0 &&
    !is.na(eval_val) && floor(eval_val) != eval_val) {
    return(NULL)
  }
  return(res)
}

# Returns the corresponding string for NA value
# NA_character_ if class(na) == "character"
#
# @param na a NA value.
#
na_to_correct_str <- function(na) {
  if (!is.na(na)) {
    return(na)
  }
  switch(class(na),
    "character" = "NA_character_",
    "complex" = "NA_complex_",
    "integer" = "NA_integer_",
    "numeric" = "NA_real_",
    "logical" = "NA"
  )
}

# Returns a logical indicating if a node is -constant
# Can have precedence ops
#
# @param fpd a flat parsed data data.frame .
# @param id Numeric indicating the node ID.
#
is_minus_constant <- function(fpd, id) {
  act_fpd <- get_children(fpd, id)
  all(act_fpd$token %in% c(constants, ops, precedence_ops, "expr")) &&
    sum(act_fpd$token %in% constants) == 1 &&
    sum(act_fpd$token == "'-'") == 1
}
