# Parses text to a flat parsed data data.frame
#
# @param text Text to parse.
#
parse_flat_data <- function(text) {
  parsed_text <- base::parse(text = text, keep.source = TRUE)
  pd <- utils::getParseData(parsed_text, includeText = TRUE)

  if (nrow(pd) == 0) {
    return(pd)
  }

  # pos_id to reorder code text
  pd$pos_id <- seq(1L, nrow(pd))

  # next_spaces and lines after each text
  pd$next_spaces[pd$terminal] <- 0
  pd$next_lines[pd$terminal] <- 0
  pd$prev_spaces[pd$terminal] <- 0

  pd_terms <- pd[pd$terminal, ]

  # next_spaces will be the difference between the column next text starts, and
  # the column this text ends
  pd_terms$next_spaces[-nrow(pd_terms)] <-
    pd_terms$col1[-1] - pd_terms$col2[-nrow(pd_terms)] - 1
  pd[pd$terminal, "next_spaces"] <- pmax(0, pd_terms$next_spaces)

  # next_lines will be the difference between the line next text starts, and
  # the line this text ends
  pd_terms$next_lines[-nrow(pd_terms)] <-
    pd_terms$line1[-1] - pd_terms$line2[-nrow(pd_terms)]
  pd[pd$terminal, "next_lines"] <- pmax(0, pd_terms$next_lines)

  # prev_spaces will be the column where each text starts, if the previous text
  # had a new line
  pd_terms$prev_spaces[which(pd_terms$next_lines > 0) + 1] <-
    pd_terms[which(pd_terms$next_lines > 0) + 1, "col1"] - 1
  pd[pd$terminal, "prev_spaces"] <- pd_terms$prev_spaces

  return(pd)
}

# Deparses a flat parsed data data.frame to text
#
# @param fpd a flat parsed data data.frame to deparse.
#
deparse_flat_data <- function(fpd) {
  fpd_terms <- fpd[fpd$terminal, ]

  # adding trailing new lines
  prev_lines <- ifelse(nrow(fpd_terms) > 0, fpd_terms$line1[[1]] - 1, 0)

  res <- ifelse(prev_lines > 0, paste(rep("\n", prev_lines), collapse = ""), "")
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
# @param ids Numeric indicating the parent ID.
# @param include_father Logical indicating if keep father node.
#
get_children <- function(fpd, ids, include_father = TRUE) {
  act_fpd <- NULL
  if (include_father) {
    act_fpd <- fpd[fpd$id %in% ids, ]
  }
  act_childs <- fpd[fpd$parent %in% ids, "id"]
  while (length(act_childs) > 0) {
    act_fpd <- rbind(act_fpd, fpd[fpd$id %in% act_childs, ])
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
    !(duplicated(parent_ids) | duplicated(parent_ids, fromLast = TRUE))
  ]
  new_fpd <- fpd[!fpd$id %in% one_child_parents, ] # remove one_child_parents
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
  # some R versions (e.g. 3.5.2) dont use the token `equal_assign`, so we
  # create it.
  # first convert `equal_assign` token to expr
  fpd$token <- sub("^equal_assign$", "expr", fpd$token)
  eq_ass_prnts_id <- fpd[fpd$token == "EQ_ASSIGN", "parent"]
  eq_ass_prnts <- fpd[fpd$id %in% eq_ass_prnts_id, ]
  if (all(eq_ass_prnts_id > 0) && # all of them have a parent
    all(eq_ass_prnts$token == "expr") && # all parents are expressions
    all(sapply(eq_ass_prnts$id, function(id) sum(fpd$parent == id) == 3))) {
    # all EQ_ASSIGN have 2 siblings ( expr EQ_ASSIGN expr_or_assign )
    return(fpd)
  }
  # if not, for each EQ_ASSIGN create its `expr` parent
  fpd <- fpd[order(fpd$pos_id), ] # pos_id is important here
  eq_assign_ids <- fpd[fpd$token == "EQ_ASSIGN", "id"]
  new_fpd <- fpd
  # equal_assign : expr EQ_ASSIGN (expr | equal_assign)
  for (i in sort(eq_assign_ids, decreasing = TRUE)) {
    act_idx <- which(new_fpd$id == i)
    act_fpd <- new_fpd[act_idx + -1:1, ]
    new_fpd <- new_fpd[-(act_idx + -1:1), ]
    expr_fpd <- act_fpd[1, ]
    act_fpd[1, "pos_id"] <- act_fpd[1, "pos_id"] + 10e-5
    expr_fpd$token <- "expr"
    expr_fpd$terminal <- FALSE
    expr_fpd$text <- paste(act_fpd$text, collapse = " ")
    expr_fpd$id <- paste0(expr_fpd$id, "_EQ_ASS")
    act_fpd$parent <- expr_fpd$id
    new_fpd <- rbind(new_fpd, expr_fpd, act_fpd)
    new_fpd <- new_fpd[order(new_fpd$pos_id), ]
  }
  new_fpd
}

# Copies relevant information from a pdf to a new pdf
#
# @param fpd_from a flat parsed data data.frame from which to take parent, etc.
# @param fpd_replace a fpd that will replace fpd_from.
#
replace_pd <- function(fpd_from, fpd_replace) {
  fpd_replace <- fpd_replace[order(fpd_replace$pos_id), ]
  from_root <- get_roots(fpd_from) # it must be one row
  replace_root <- get_roots(fpd_replace) # it must be one row
  new_fpd <- fpd_replace

  # from old fpd parent, copy to new parent: id, parent, and pos
  new_fpd[fpd_replace$id == replace_root$id, c("id", "parent", "pos_id")] <-
    from_root[, c("id", "parent", "pos_id")]

  # new fpd first childs have to point to new parent id
  new_fpd[fpd_replace$parent == replace_root$id, "parent"] <- from_root$id

  # create a fake ids to every node except parent
  new_fpd[fpd_replace$id != replace_root$id, "id"] <-
    paste0(from_root$id, "_", new_fpd[fpd_replace$id != replace_root$id, "id"])

  # fix parents for new fpd (not parent, nor first childs)
  new_fpd[fpd_replace$id != replace_root$id &
    fpd_replace$parent != replace_root$id, "parent"] <-
    paste0(
      from_root$id, "_",
      new_fpd[fpd_replace$id != replace_root$id &
        fpd_replace$parent != replace_root$id, "parent"]
    )

  # fix pos_ids
  new_fpd$pos_id <- from_root$pos_id + seq(0, nrow(new_fpd) - 1) * 10e-5

  # copy first prev_spaces, and last next_spaces and lines
  from_terms <- fpd_from[fpd_from$terminal, ]
  fst_term <- from_terms[which.min(from_terms$pos_id), ]
  last_term <- from_terms[which.max(from_terms$pos_id), ]
  new_terms <- new_fpd[new_fpd$terminal, "id"]
  if (fst_term$prev_spaces != 0) {
    new_fpd[new_fpd$id == new_terms[[1]], "prev_spaces"] <-
      fst_term$prev_spaces
  }
  if (last_term$next_spaces != 0) {
    new_fpd[new_fpd$id == new_terms[[length(new_terms)]], "next_spaces"] <-
      last_term$next_spaces
  }
  if (last_term$next_lines != 0) {
    new_fpd[new_fpd$id == new_terms[[length(new_terms)]], "next_lines"] <-
      last_term$next_lines
  }

  return(new_fpd)
}

# Returns the fpd, where branches starting from ids were removed
#
# @param fpd a flat parsed data data.frame .
# @param ids an ids vector of branches id to remove.
#
remove_nodes <- function(fpd, ids) {
  to_remove_fpd <- get_children(fpd, ids)
  new_fpd <- fpd[!fpd$id %in% to_remove_fpd$id, ]
  return(new_fpd)
}

# Returns the ids of the specified tokens
#
# @param fpd a flat parsed data data.frame .
# @param tokens a character vector of tokens.
#
get_ids_of_token <- function(fpd, tokens) {
  fpd$id[fpd$token %in% tokens]
}
