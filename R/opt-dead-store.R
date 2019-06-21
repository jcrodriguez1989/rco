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
  new_fpd <- one_dead_store(new_fpd)
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
  browser()
  res_fpd <- fpd
  # dead store happens only into functions, so get the expr of each function
  fun_ids <- get_ids_of_token(fpd, "FUNCTION")
  fun_prnt_ids <- fpd$parent[fpd$id %in% fun_ids]
  fun_expr_ids <- sapply(fun_prnt_ids, function(act_prnt_id) {
    rev(fpd$id[fpd$parent == act_prnt_id & fpd$token == "expr"])[[1]]
  })

  # for each function expr do a dead store removal
  for (id in fun_expr_ids) {
    if (!id %in% res_fpd$id) {
      next
    }
    act_fpd <- get_children(res_fpd, id)
    ass_vars <- ods_get_assigned_vars(act_fpd, id)
    # get used vars
    # get if function calls
  }


  return(new_fpd)
}

# Returns the names of the vars that are beign assigned in an expr
#
# @param fpd a flat parsed data data.frame .
# @param id Numeric indicating the node ID.
#
ods_get_assigned_vars <- function(fpd, id) {
  act_ids <- id
  res <- c()
  while (length(act_ids) > 0) {
    res <- c(res, sapply(act_ids, get_assigned_var, fpd = fpd))
    act_ids <- fpd[fpd$parent %in% act_ids & !fpd$terminal, "id"]
  }
  unique(res[res != ""])
}
