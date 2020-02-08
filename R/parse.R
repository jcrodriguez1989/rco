# Parses text to a parsed data data.frame.
#
# @param text A character vector with text to parse.
#
parse_text <- function(text) {
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

  pd
}

# Deparses a parsed data data.frame to text with the code.
#
# @param pd A parsed data data.frame to deparse.
#
deparse_data <- function(pd) {
  pd_terms <- pd[pd$terminal, ]

  # adding trailing new lines
  prev_lines <- ifelse(nrow(pd_terms) > 0, pd_terms$line1[[1]] - 1, 0)

  res <- ifelse(prev_lines > 0, paste(rep("\n", prev_lines), collapse = ""), "")
  for (i in seq_len(nrow(pd_terms))) {
    act_pd <- pd_terms[i, ]
    res <- paste0(res, paste0(
      paste0(rep(" ", act_pd$prev_spaces), collapse = ""),
      act_pd$text,
      paste0(rep(" ", act_pd$next_spaces), collapse = ""),
      paste0(rep("\n", act_pd$next_lines), collapse = "")
    ))
  }
  res
}

# Gets a sub pd with all the children from a node ID.
#
# @param pd A parsed data data.frame.
# @param ids A numeric vector indicating the parent IDs.
# @param include_father A logical indicating if father node must be included.
#
get_children <- function(pd, ids, include_father = TRUE) {
  act_pd <- NULL
  if (include_father) {
    act_pd <- pd[pd$id %in% ids, ]
  }
  act_childs <- pd$id[pd$parent %in% ids]
  while (length(act_childs) > 0) {
    act_pd <- rbind(act_pd, pd[pd$id %in% act_childs, ])
    act_childs <- pd$id[pd$parent %in% pd$id[pd$id %in% act_childs]]
  }

  if (nrow(act_pd) > 0) {
    act_pd <- act_pd[order(act_pd$pos_id), ]
  }

  act_pd
}

# Returns the ids of ancestors of a node (parents and their parents).
#
# @param pd A parsed data data.frame.
# @param id A numeric indicating the node ID to get ancestors.
#
get_ancestors <- function(pd, id) {
  res <- act_id <- id
  while (length(act_id) > 0 && act_id > 0) {
    act_id <- pd$parent[pd$id == act_id]
    res <- c(res, act_id)
  }
  res
}

# Replaces exprs with just one child, by its child.
#
# @param pd A parsed data data.frame.
#
flatten_leaves <- function(pd) {
  parent_ids <- pd$parent
  one_child_parents <- parent_ids[ # parents with unique child
    !(duplicated(parent_ids) | duplicated(parent_ids, fromLast = TRUE))
  ]
  fpd <- pd[!pd$id %in% one_child_parents, ] # remove one-child-parents
  one_child_parents <- one_child_parents[one_child_parents > 0]
  for (ocp in one_child_parents) { # new parent will be the grandpa
    fpd$parent[fpd$parent == ocp] <- pd$parent[pd$id == ocp]
  }
  fpd
}

# Returns the pd with only roots.
#
# @param pd a parsed data data.frame.
#
get_roots <- function(pd) {
  pd[!pd$parent %in% pd$id, ]
}

# Converts equal_assign to an expr.
#
# @param pd A parsed data data.frame.
#
eq_assign_to_expr <- function(pd) {
  # different R versions have different tokens for `equal_assign`
  # first convert `equal_assign` and `expr_or_assign_or_help` token to expr
  pd$token <- sub("^equal_assign$|^expr_or_assign_or_help$", "expr", pd$token)
  eq_ass_prnts_id <- pd$parent[pd$token == "EQ_ASSIGN"]
  eq_ass_prnts <- pd[pd$id %in% eq_ass_prnts_id, ]
  if (all(eq_ass_prnts_id > 0) && # all of them have a parent
    all(eq_ass_prnts$token == "expr") && # all parents are expressions
    all(sapply(eq_ass_prnts$id, function(id) sum(pd$parent == id) == 3))) {
    # all EQ_ASSIGN have 2 siblings ( expr EQ_ASSIGN expr_or_assign )
    return(pd)
  }
  # if not, for each EQ_ASSIGN create its `expr` parent
  pd <- pd[order(pd$pos_id), ] # pos_id is important here
  eq_assign_ids <- pd[pd$token == "EQ_ASSIGN", "id"]
  new_pd <- pd
  # equal_assign : expr EQ_ASSIGN (expr | equal_assign)
  for (i in sort(eq_assign_ids, decreasing = TRUE)) {
    act_idx <- which(new_pd$id == i)
    act_pd <- new_pd[act_idx + -1:1, ]
    new_pd <- new_pd[-(act_idx + -1:1), ]
    expr_pd <- act_pd[1, ]
    act_pd$pos_id[[1]] <- act_pd$pos_id[[1]] + 10e-4
    expr_pd$token <- "expr"
    expr_pd$terminal <- FALSE
    expr_pd$text <- paste(act_pd$text, collapse = " ")
    expr_pd$id <- paste0(expr_pd$id, "_EQ_ASS")
    act_pd$parent <- expr_pd$id
    new_pd <- rbind(new_pd, expr_pd, act_pd)
    new_pd <- new_pd[order(new_pd$pos_id), ]
  }
  new_pd
}

# Copies relevant information from a pd to a new pd.
#
# @param pd_from A parsed data data.frame from which to copy information.
# @param pd_replace A parsed data data.frame to which paste information.
#
replace_pd <- function(pd_from, pd_replace) {
  pd_replace <- pd_replace[order(pd_replace$pos_id), ]
  from_root <- get_roots(pd_from) # it must be one row
  replace_root <- get_roots(pd_replace) # it must be one row
  new_pd <- pd_replace

  # from old pd parent, copy to new parent: id, parent, and pos
  new_pd[pd_replace$id == replace_root$id, c("id", "parent", "pos_id")] <-
    from_root[, c("id", "parent", "pos_id")]

  # new pd first childs have to point to new parent id
  new_pd$parent[pd_replace$parent == replace_root$id] <- from_root$id

  # create a fake ids to every node except parent
  new_pd[pd_replace$id != replace_root$id, "id"] <-
    paste0(from_root$id, "_", new_pd$id[pd_replace$id != replace_root$id])

  # fix parents for new pd (not parent, nor first childs)
  new_pd$parent[
    pd_replace$id != replace_root$id & pd_replace$parent != replace_root$id
  ] <-
    paste0(
      from_root$id, "_",
      new_pd$parent[pd_replace$id != replace_root$id &
        pd_replace$parent != replace_root$id]
    )

  # fix pos_ids
  new_pd$pos_id <- create_new_pos_id(pd_from, nrow(new_pd), from_root$id)

  # copy first prev_spaces, and last next_spaces and lines
  from_terms <- pd_from[pd_from$terminal, ]
  fst_term <- from_terms[which.min(from_terms$pos_id), ]
  last_term <- from_terms[which.max(from_terms$pos_id), ]
  new_terms <- new_pd$id[new_pd$terminal]
  # if first or last token was a precedence op then add at least 1 extra space
  if (fst_term$prev_spaces != 0 || fst_term$token %in% c("'{'", "'('")) {
    new_pd$prev_spaces[new_pd$id == new_terms[[1]]] <-
      max(fst_term$prev_spaces, fst_term$token %in% c("'{'", "'('"))
  }
  if (last_term$next_spaces != 0 || last_term$token %in% c("'}'", "')'")) {
    new_pd$next_spaces[new_pd$id == new_terms[[length(new_terms)]]] <-
      max(last_term$next_spaces, last_term$token %in% c("'}'", "')'"))
  }
  if (last_term$next_lines != 0) {
    new_pd$next_lines[new_pd$id == new_terms[[length(new_terms)]]] <-
      last_term$next_lines
  }

  new_pd
}

# Given a pd and from or to id, it creates n new pos_ids.
#
# @param pd A parsed data data.frame.
# @param n A numeric indicating the number of pos_ids to create.
# @param from_id A numeric indicating starting pos_id node.
# @param to_id A numeric indicating ending pos_id node.
#
create_new_pos_id <- function(pd, n, from_id = "", to_id = "") {
  pd <- pd[order(pd$pos_id), ]
  from_pos <- which(pd$id == from_id)
  to_pos <- which(pd$id == to_id)
  from_pos_id <- utils::head(c(pd$pos_id[from_pos], pd$pos_id[to_pos - 1]), 1)
  to_pos_id <- utils::head(c(pd$pos_id[to_pos], pd$pos_id[from_pos + 1]), 1)

  if (from_id != "" && length(from_pos_id) > 0) {
    from_pos_id + (10e-4 * seq_len(n))
  } else if (to_id != "" && length(to_pos_id) > 0) {
    rev(to_pos_id - (10e-4 * seq_len(n)))
  }
}

# Returns the pd, where branches starting from ids were removed
#
# @param pd A parsed data data.frame.
# @param ids A vector of ids of branches to remove.
#
remove_nodes <- function(pd, ids) {
  to_remove_pd <- get_children(pd, ids)
  pd[!pd$id %in% to_remove_pd$id, ]
}

# Returns the pd, where branches starting from ids were removed, but keeps
# trailing lines and spaces.
#
# @param pd A parsed data data.frame.
# @param ids A vector of ids of branches to remove.
#
pretty_remove_nodes <- function(pd, ids) {
  to_remove_pd <- get_children(pd, ids)
  new_pd <- pd[!pd$id %in% to_remove_pd$id, ]

  for (act_id in ids) {
    act_rm_pd <- get_children(to_remove_pd, act_id)

    # check if parent is loop or if, and doesnt have '{ }'
    rm_sblngs <- new_pd[new_pd$parent ==
      act_rm_pd$parent[act_rm_pd$id == act_id], ]
    if (any(c("IF", "ELSE", loops) %in% rm_sblngs$token) &&
      !"'{'" %in% rm_sblngs$token) {
      # add an {} after loop or if
      new_pd <- rbind(new_pd, replace_pd(act_rm_pd, parse_text("{}")))
      next
    }

    rm_last_term_id <- utils::tail(act_rm_pd$id[act_rm_pd$terminal], 1)
    rm_last_term <- act_rm_pd[act_rm_pd$id == rm_last_term_id, ]
    kp_last_term_id <- utils::tail(new_pd$id[new_pd$terminal &
      new_pd$pos_id < rm_last_term$pos_id], 1)
    kp_last_term <- new_pd[new_pd$id == kp_last_term_id, ]
    new_pd$next_lines[new_pd$id == kp_last_term_id] <-
      max(rm_last_term$next_lines, kp_last_term$next_lines)
    new_pd$next_spaces[new_pd$id == kp_last_term_id] <-
      max(rm_last_term$next_spaces, kp_last_term$next_spaces)
  }

  new_pd
}
