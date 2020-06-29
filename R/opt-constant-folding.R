#' Optimizer: Constant Folding.
#'
#' Performs one constant folding pass.
#' Carefully examine the results after running this function!
#'
#' @param texts A list of character vectors with the code to optimize.
#' @param fold_floats A logical indicating if floating-point results should be
#'   folded (will reduce precision).
#' @param in_fun_call A logical indicating whether it should propagate in
#'   function calls. Note: this could change the semantics of the program.
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
  res$codes <- lapply(texts, cf_one_file,
    fold_floats = fold_floats,
    in_fun_call = in_fun_call
  )
  res
}

# Performs a constant folding pass on one text.
#
# @param text A character vector with code to optimize.
# @param fold_floats A logical indicating if floating-point results should be
#   folded (will reduce precision).
# @param in_fun_call A logical indicating whether it should propagate in
#   function calls. Note: this could change the semantics of the program.
#
cf_one_file <- function(text, fold_floats, in_fun_call) {
  fpd <- parse_text(text)
  fpd <- flatten_leaves(fpd)
  if (nrow(fpd) > 0) {
    fpd <- cf_one_fpd(fpd, fold_floats, in_fun_call)
  }
  deparse_data(fpd)
}

# Performs the constant folding pass.
#
# @param fpd A flatten parsed data data.frame.
# @param fold_floats A logical indicating if floating-point results should be
#   folded (will reduce precision).
# @param in_fun_call A logical indicating whether it should propagate in
#   function calls. Note: this could change the semantics of the program.
#
cf_one_fpd <- function(fpd, fold_floats, in_fun_call) {
  # keep but dont fold comments ( < 0 )
  new_fpd <- fpd[fpd$parent < 0, ]
  fpd <- fpd[fpd$parent >= 0, ]

  in_fun_call_ids <- c()
  if (!in_fun_call) {
    # get ids of exprs in fun calls
    in_fun_call_ids <- get_children(
      fpd, fpd$parent[fpd$token == "SYMBOL_FUNCTION_CALL"]
    )$id
  }

  # start visiting root nodes
  visit_nodes <- get_roots(fpd)$id
  while (length(visit_nodes) > 0) {
    new_visit_nodes <- c()
    for (act_parent in visit_nodes) {
      act_fpd <- get_children(fpd, act_parent)
      if (all(act_fpd$token %in% c(constants, ops, precedence_ops, "expr")) &&
        !is_minus_constant(act_fpd, act_parent) &&
        !act_parent %in% in_fun_call_ids) {
        # all the children are terminals or ops. try to evaluate it
        # And it is not just -constant
        act_code_fpd <- fpd[fpd$id == act_parent, ]
        folded_fpd <- get_folded_fpd(act_code_fpd, fold_floats)
        if (!is.null(folded_fpd)) {
          # it is a constant or -constant
          # replace the parent expr by the new expr (folded)
          act_new_fpd <- replace_pd(act_fpd, folded_fpd)
          new_fpd <- rbind(new_fpd, act_new_fpd)
          next
        }
      }
      # it could not be folded, so save parent, and terminal childs
      new_fpd <- rbind(new_fpd, fpd[fpd$id == act_parent, ])
      new_fpd <- rbind(new_fpd, fpd[fpd$parent == act_parent & fpd$terminal, ])
      # continue visiting child exprs
      new_visit_nodes <- c(
        new_visit_nodes,
        fpd[fpd$parent == act_parent & !fpd$terminal, "id"]
      )
    }
    visit_nodes <- new_visit_nodes
  }

  new_fpd[order(new_fpd$pos_id), ]
}

# Returns a folded fpd, if it only had constants and operators.
# If it could not fold then it returns NULL.
#
# @param fpd A flatten parsed data data.frame.
# @param fold_floats A logical indicating if floating-point results should be
#   folded (will reduce precision).
#
get_folded_fpd <- function(fpd, fold_floats) {
  if (fpd$token %in% constants) {
    return()
  }

  eval_val <- try(
    {
      eval(parse(text = fpd$text))
    },
    silent = TRUE
  )
  if (inherits(eval_val, "try-error")) {
    return()
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

  res <- parse_text(eval_val_str)
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
    return()
  }

  # it is a constant or -constant
  if (!fold_floats && "NUM_CONST" %in% res$token && length(eval_val) != 0 &&
    !is.na(eval_val) && floor(eval_val) != eval_val) {
    return()
  }
  res
}

# Returns the corresponding string for NA value.
# E.g., NA_character_ if class(na) == "character"
#
# @param na A NA value.
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

# Returns a logical indicating if a node is -constant.
# Can have precedence ops.
#
# @param pd A parsed data data.frame.
# @param id A numeric indicating the node ID.
#
is_minus_constant <- function(pd, id) {
  act_pd <- get_children(pd, id)
  all(act_pd$token %in% c(constants, ops, precedence_ops, "expr")) &&
    sum(act_pd$token %in% constants) == 1 &&
    sum(act_pd$token == "'-'") == 1
}
