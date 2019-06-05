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
#' code <- paste("i <- 320 * 200 * 32",
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

  # fold until no changes
  old_pd <- NULL
  while (!isTRUE(all.equal(old_pd, pd))) {
    old_pd <- pd
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
  new_pd <- pd[pd$parent < 0,]
  pd <- pd[pd$parent >= 0,]

  # start visiting root nodes
  visit_nodes <- get_roots(pd)$id
  while (length(visit_nodes) > 0) {
    new_visit_nodes <- c()
    for (act_parent in visit_nodes) {
      act_pd <- get_children(pd, act_parent)
      if (all(act_pd$token %in% c(constants, ops, precedence_ops, "expr"))) {
        # all the children are terminals or ops. try to evaluate it
        act_code <- pd[pd$id == act_parent, "text"]
        eval_val <- try({
          eval(parse(text = act_code))
        }, silent = TRUE)
        if (!inherits(eval_val, "try-error")) {
          # it was correctly evaluated then create the fpd of the eval val
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
      new_pd <- rbind(new_pd, pd[pd$parent == act_parent & pd$terminal,])
      # continue visiting child exprs
      new_visit_nodes <- c(new_visit_nodes,
                           pd[pd$parent == act_parent & !pd$terminal, "id"])
    }
    visit_nodes <- new_visit_nodes
  }

  new_pd <- new_pd[order(new_pd$pos_id), ]
  return(new_pd)
}

# todo: delete this code alternative
# @param pd A parse data data.frame with code to optimize.
one_fold2 <- function(pd, fold_floats) {
  # parents of constants
  const_parents <- unique(pd[pd$token %in% constants, "parent"])
  for (id in const_parents) {
    act_parent <- pd[pd$id == id, ]
    act_expr <- pd[pd$parent == id, ]

    # if all the children are terminals, and we have more than one
    if (any(!act_expr$terminal) || nrow(act_expr) <= 1) {
      next
    }
    # all the children are terminals, and we have more than one
    # try to evaluate it
    eval_val <- try({
      eval(parse(text = act_parent$text))
    }, silent = TRUE)

    if (inherits(eval_val, "try-error")) {
      next
    }
    # it was correctly evaluated then
    res <- parse_flat_data(eval_val, include_text = TRUE)
    # only fold if it parses to two rows: a constant, and an expr as its parent
    if (nrow(res) != 2 ||
      !"expr" %in% res$token || !any(constants %in% res$token)) {
      next
    }
    # replace the parent and its childs by the new constant (folded)
    res <- res[res$token %in% constants, ]
    if (!fold_floats && res$token == "NUM_CONST" &&
      floor(as.numeric(res$text)) != as.numeric(res$text)) {
      next
    }
    pd[pd$id == id, c("token", "terminal", "text")] <-
      res[, c("token", "terminal", "text")]
    pd <- pd[!pd$parent %in% id, ]
  }
  return(pd)
}
