# Parses text to a flat parsed data data.frame
#
# @param text Text to parse.
#
parse_flat_data <- function(text, include_text = NA) {
  parsed_text <- base::parse(text = text, keep.source = TRUE)
  pd <- utils::getParseData(parsed_text, includeText = include_text)

  # pos_id to reorder code text
  pd$pos_id <- seq(1L, nrow(pd))

  # next_spaces and lines after each text
  pd$next_spaces[pd$terminal] <- 0
  pd$next_lines[pd$terminal] <- 0
  pd$prev_spaces[pd$terminal] <- 0

  pd_terms <- pd[pd$terminal,]

  # next_spaces will be the difference between the column next text starts, and
  # the column this text ends
  pd_terms$next_spaces[-nrow(pd_terms)] <-
    pd_terms$col1[-1] - pd_terms$col2[-nrow(pd_terms)] -1
  pd[pd$terminal, "next_spaces"] <- pmax(0, pd_terms$next_spaces)

  # next_lines will be the difference between the line next text starts, and
  # the line this text ends
  pd_terms$next_lines[-nrow(pd_terms)] <-
    pd_terms$line1[-1] - pd_terms$line2[-nrow(pd_terms)]
  pd[pd$terminal, "next_lines"] <- pmax(0, pd_terms$next_lines)

  # prev_spaces will be the column where each text starts, if the previous text
  # had a new line
  pd_terms$prev_spaces[which(pd_terms$next_lines > 0) +1] <-
    pd_terms[which(pd_terms$next_lines > 0) +1, "col1"] -1
  pd[pd$terminal, "prev_spaces"] <- pd_terms$prev_spaces

  # pd$token_before <- NA
  # pd$token_after <- NA

  # terminals <- pd[pd$terminal,]
  # terminals$token_after <- c(terminals$token[seq(2, nrow(terminals))], NA)
  # terminals$token_before <- c(NA, terminals$token[seq(1, nrow(terminals)-1)])
  # pd[pd$terminal,] <- terminals
  return(pd)
}

# Deparses a flat parsed data data.frame to text
#
# @param fpd a flat parsed data data.frame to deparse.
#
deparse_flat_data <- function(fpd) {
  fpd_terms <- fpd[fpd$terminal, ]

  res <- ""
  for (i in seq_len(nrow(fpd_terms))) {
    act_pd <- fpd_terms[i, ]
    res <- paste0(res, paste0(
      paste0(rep(" ", act_pd$prev_spaces), collapse = ""),
      act_pd$text,
      paste0(rep(" ", act_pd$next_spaces), collapse = ""),
      paste0(rep("\n", act_pd$next_lines), collapse = "")
    ))
  }
  return(res)
}

# Gets a sub fpd with all the children from a node ID
#
# @param fpd a flat parsed data data.frame to deparse.
# @param id Numeric indicating the parent ID.
# @param include_father Logical indicating if keep father node.
#
get_children <- function(fpd, id, include_father = TRUE) {
  act_fpd <- fpd[fpd$id %in% id,]
  if (!include_father) {
    act_fpd <- NULL
  }
  act_childs <- fpd[fpd$parent %in% id, "id"]
  while (length(act_childs) > 0) {
    act_fpd <- rbind(act_fpd, fpd[fpd$id %in% act_childs,])
    act_childs <- fpd[fpd$parent %in% fpd[fpd$id %in% act_childs, "id"], "id"]
  }
  act_fpd <- act_fpd[order(act_fpd$pos_id), ]

  return(act_fpd)
}

# Replaces exprs with just one child, by its child
#
# @param fpd a flat parsed data data.frame .
#
flatten_leaves <- function(fpd) {
  parent_ids <- fpd$parent
  one_child_parents <- parent_ids[ # parents with unique child
    !(duplicated(parent_ids) | duplicated(parent_ids, fromLast = TRUE))]
  new_fpd <- fpd[!fpd$id %in% one_child_parents,] # remove one_child_parents
  one_child_parents <- one_child_parents[one_child_parents > 0]
  for (ocp in one_child_parents) { # new parent will be the grandpa
    new_fpd[new_fpd$parent == ocp, "parent"] <- fpd[fpd$id == ocp, "parent"]
  }
  return(new_fpd)
}

# Returns the fpd with only roots
#
# @param fpd a flat parsed data data.frame .
#
get_roots <- function(fpd) {
  fpd[!fpd$parent %in% fpd$id, ]
}

# Converts equal_assign to an expr
#
# @param fpd a flat parsed data data.frame .
#
eq_assign_to_expr <- function(fpd) {
  fpd <- fpd[order(fpd$pos_id),] # pos_id is important here
  eq_assign_ids <- fpd[fpd$token == "EQ_ASSIGN", "id"]
  new_fpd <- fpd
  # equal_assign : expr EQ_ASSIGN (expr | equal_assign)
  for (i in sort(eq_assign_ids, decreasing = TRUE)) {
    act_idx <- which(new_fpd$id == i)
    act_fpd <- new_fpd[act_idx + -1:1,]
    new_fpd <- new_fpd[-(act_idx + -1:1),]
    expr_fpd <- act_fpd[1,]
    act_fpd[1, "pos_id"] <- act_fpd[1, "pos_id"] + 10e-5
    expr_fpd$token <- "expr"
    expr_fpd$terminal <- FALSE
    expr_fpd$text <- paste(act_fpd$text, collapse = " ")
    expr_fpd$id <- paste0(expr_fpd$id, "_EQ_ASS")
    act_fpd$parent <- expr_fpd$id
    new_fpd <- rbind(new_fpd, expr_fpd, act_fpd)
    new_fpd <- new_fpd[order(new_fpd$pos_id),]
  }
  new_fpd
}
