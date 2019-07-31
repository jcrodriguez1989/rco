# Parses text to a flat parsed data data.frame
#
# @param text Text to parse.
#
parse_flat_data <- function(text) {
  parsed_text <- base::parse(text = text, keep.source = TRUE)
  pd <- utils::getParseData(parsed_text, includeText = TRUE)

  # bug related to styler bug #216
  if (any(grepl("^\\[[[:digit:]]+ chars quoted with '.']$", pd$text))) {
    stop("Bug #22 : Your code has a really long string.", call. = FALSE)
  }

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

  if (nrow(act_fpd) > 0) {
    act_fpd <- act_fpd[order(act_fpd$pos_id), ]
  }

  return(act_fpd)
}

# Returns the id of ancestors of a node (parents and their parents)
#
# @param fpd A flat parsed data data.frame .
# @param id Numeric indicating the node ID to get parents.
#
get_ancestors <- function(fpd, id) {
  res <- act_id <- id
  while (length(act_id) > 0 && act_id > 0) {
    act_id <- fpd[fpd$id == act_id, "parent"]
    res <- c(res, act_id)
  }
  res
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
    act_fpd$pos_id[[1]] <- act_fpd$pos_id[[1]] + 10e-4
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
  new_fpd$pos_id <- create_new_pos_id(fpd_from, nrow(new_fpd), from_root$id)

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

# Given a fpd and from or to id, it creates n new pos_ids
#
# @param fpd A flat parsed data data.frame .
# @param n Numeric indicating the number of pos_ids to create.
# @param from_id Numeric indicating the node ID to find fun calls.
# @param to_id Numeric indicating the node ID to find fun calls.
#
create_new_pos_id <- function(fpd, n, from_id = "", to_id = "") {
  fpd <- fpd[order(fpd$pos_id), ]
  from_pos <- which(fpd$id == from_id)
  to_pos <- which(fpd$id == to_id)
  from_pos_id <- utils::head(c(fpd$pos_id[from_pos], fpd$pos_id[to_pos - 1]), 1)
  to_pos_id <- utils::head(c(fpd$pos_id[to_pos], fpd$pos_id[from_pos + 1]), 1)

  if (from_id != "" && length(from_pos_id) > 0) {
    from_pos_id + (10e-4 * seq_len(n))
  } else if (to_id != "" && length(to_pos_id) > 0) {
    rev(to_pos_id - (10e-4 * seq_len(n)))
  }
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

# Returns the fpd, where branches starting from ids were removed, but kees new
# lines and spaces
#
# @param fpd a flat parsed data data.frame .
# @param ids an ids vector of branches id to remove.
#
pretty_remove_nodes <- function(fpd, ids) {
  to_remove_fpd <- get_children(fpd, ids)
  new_fpd <- fpd[!fpd$id %in% to_remove_fpd$id, ]

  for (act_id in ids) {
    act_rm_fpd <- get_children(to_remove_fpd, act_id)

    # check if parent is loop or if, and doesnt have '{ }'
    rm_sblngs <- new_fpd[new_fpd$parent ==
      act_rm_fpd$parent[act_rm_fpd$id == act_id], ]
    if (any(c("IF", "ELSE", loops) %in% rm_sblngs$token) &&
      !"'{'" %in% rm_sblngs$token) {
      # add an {} after loop or if
      new_fpd <- rbind(new_fpd, replace_pd(act_rm_fpd, parse_flat_data("{}")))
      next
    }

    rm_last_term_id <- utils::tail(act_rm_fpd$id[act_rm_fpd$terminal], 1)
    rm_last_term <- act_rm_fpd[act_rm_fpd$id == rm_last_term_id, ]
    kp_last_term_id <- utils::tail(new_fpd$id[new_fpd$terminal &
      new_fpd$pos_id < rm_last_term$pos_id], 1)
    kp_last_term <- new_fpd[new_fpd$id == kp_last_term_id, ]
    new_fpd$next_lines[new_fpd$id == kp_last_term_id] <-
      max(rm_last_term$next_lines, kp_last_term$next_lines)
    new_fpd$next_spaces[new_fpd$id == kp_last_term_id] <-
      max(rm_last_term$next_spaces, kp_last_term$next_spaces)
  }

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
