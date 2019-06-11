#' Optimizer: Constant Folding
#'
#' Performs one constant folding pass.
#' Carefully examine the results after running this function!
#'
#' @param texts A list of character vectors with the code to optimize.
#' @param fold_floats Logical indicating if floating-point results should be
#'   folded (will reduce precision).
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
opt_constant_folding <- function(texts, fold_floats = FALSE) {
  # todo: implement intelligent constant folding? for example: fold 0 * x to 0
  # todo: reorder vars in associativity?
  # todo: try constant fold knoww-functions with constants?
  res <- list()
  res$codes <- lapply(texts, constant_fold_one, fold_floats = fold_floats)
  return(res)
}

# Performs a constant folding pass on one text.
#
# @param text A character vector with code to optimize.
# @param fold_floats Logical indicating if floating-point results should be
#   folded (will reduce precision).
#
constant_fold_one <- function(text, fold_floats) {
  pd <- parse_flat_data(text, include_text = TRUE)
  pd <- flatten_leaves(pd)
  if (nrow(pd) > 0) {
    pd <- one_fold(pd, fold_floats)
  }
  deparse_flat_data(pd)
}

# Performs the constant folding pass.
#
# @param pd A parse data data.frame with code to optimize.
# @param fold_floats Logical indicating if floating-point results should be
#   folded (will reduce precision).
#
one_fold <- function(pd, fold_floats) {
  # keep but dont fold comments ( < 0 )
  new_pd <- pd[pd$parent < 0, ]
  pd <- pd[pd$parent >= 0, ]

  # start visiting root nodes
  visit_nodes <- get_roots(pd)$id
  while (length(visit_nodes) > 0) {
    new_visit_nodes <- c()
    for (act_parent in visit_nodes) {
      act_pd <- get_children(pd, act_parent)
      if (all(act_pd$token %in% c(constants, ops, precedence_ops, "expr"))) {
        # all the children are terminals or ops. try to evaluate it
        act_code_pd <- pd[pd$id == act_parent,]
        if (act_code_pd$token %in% c("STR_CONST", "NULL_CONST")) {
          eval_val <- act_code_pd$text
        } else {
          eval_val <- try({
            eval(parse(text = act_code_pd$text))
          }, silent = TRUE)
        }
        if (!inherits(eval_val, "try-error")) {
          # it was correctly evaluated then create the fpd of the eval val
          if (is.null(eval_val)) {
            # there was a bug when eval_val was NULL
            eval_val <- "NULL"
          }
          res <- parse_flat_data(eval_val, include_text = TRUE)
          res <- flatten_leaves(res)
          if (all(res$token %in% c("expr", "'-'", constants))) {
            # it is a constant or -constant
            # replace the parent expr by the new expr (folded)
            if (fold_floats || !"NUM_CONST" %in% res$token ||
              floor(eval_val) == eval_val) {
              act_new_fpd <- replace_pd(act_pd, res)
              new_pd <- rbind(new_pd, act_new_fpd)
              next
            }
          }
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
