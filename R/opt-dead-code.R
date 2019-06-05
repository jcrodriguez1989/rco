#' Optimizer: Dead Code Elimination
#'
#' Performs one dead code elimination pass.
#' Carefully examine the results after running this function!
#'
#' @param texts A list of character vectors with the code to optimize.
#'
#' @examples
#' # todo: add example
#' code <- paste(
#'   sep = "\n"
#' )
#' cat(opt_dead_code(list(code))$codes[[1]])
#' @export
#'
opt_dead_code <- function(texts) {
  res <- list()
  res$codes <- lapply(texts, dead_code_one)
  return(res)
}

# Executes dead code elimination on one text of code
#
# @param text A character vector with code to optimize.
#
dead_code_one <- function(text) {
  fpd <- parse_flat_data(text, include_text = TRUE)
  fpd <- flatten_leaves(fpd)
  # fpd <- eq_assign_to_expr(fpd)
  res_fpd <- fpd[fpd$parent < 0,] # keep lines with just comments
  new_fpd <- fpd[fpd$parent >= 0,] # keep lines with just comments

  # eliminate dead code until no changes
  # old_fpd <- NULL
  # while (!isTRUE(all.equal(old_fpd, new_fpd))) {
    # old_fpd <- new_fpd
    new_fpd <- one_dead_code(new_fpd)
  # }
  res_fpd <- rbind(res_fpd, new_fpd)
  res_fpd <- res_fpd[order(res_fpd$pos_id),]
  deparse_flat_data(res_fpd)
}

# Executes dead code elimination of a tree
#
# @param fpd a flat parsed data data.frame .
#
one_dead_code <- function(fpd) {
  # first remove code that is after (and equally nested) next, break, or return
  new_fpd <- remove_after_interruption(fpd)

  return(new_fpd)
}

# Returns a new fpd where equally nested code after interruption commands was
# removed ( break, next, return(...) )
# Assumes `return` base function has not been overwritten
#
# @param fpd a flat parsed data data.frame .
#
remove_after_interruption <- function(fpd) {
  res_fpd <- fpd
  # get nodes that are interruption commands
  return_calls <- fpd[
    fpd$token == "SYMBOL_FUNCTION_CALL" & fpd$text == "return",]
  # return parent is an expression
  intr <- fpd[fpd$id %in% return_calls$parent,]
  intr <- rbind(intr, fpd[fpd$token %in% c("BREAK", "NEXT"),])

  # for each interruption parent, delete children after interruption
  for (i in seq_len(nrow(intr))) {
    if (!intr$id[[i]] %in% res_fpd$id) {
      next
    }
    id <- intr$parent[[i]]
    intr_prnt <- res_fpd[res_fpd$id == id,]
    intr_sibl <- res_fpd[res_fpd$parent == id,]
    keep_ids <- intr_sibl[seq_len(which(intr_sibl$id == intr$id[[i]])), "id"]
    # for each opening precedence op, keep one closing
    prec_sibl <- intr_sibl[intr_sibl$token %in% precedence_ops, "id"]
    keep_ids <- c(keep_ids,
                  rev(rev(prec_sibl)[seq_len(sum(keep_ids %in% prec_sibl))]))
    intr_sibl[intr_sibl$id %in% keep_ids,]
    remove_ids <- setdiff(intr_sibl$id, keep_ids)
    res_fpd <- remove_nodes(res_fpd, remove_ids)
  }
  return(res_fpd)
}
