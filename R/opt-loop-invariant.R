#' Optimizer: Loop-invariant Code Motion.
#'
#' Performs one loop-invariant code motion pass.
#' Carefully examine the results after running this function!
#'
#' @param texts A list of character vectors with the code to optimize.
#'
#' @examples
#' code <- paste(
#'   "i <- 0",
#'   "while (i < n) {",
#'   "  x <- y + z",
#'   "  a[i] <- 6 * i + x * x",
#'   "  i <- i + 1",
#'   "}",
#'   sep = "\n"
#' )
#' cat(opt_loop_invariant(list(code))$codes[[1]])
#' @export
#'
opt_loop_invariant <- function(texts) {
  # todo: invariant subexpressions motion?
  # while (i < n) { i <- (x * y) + 1 } is equivalent to
  # is_1 <- (x * y); while (i < n) { i <- is_1 + 1 }
  # todo: check that assigned vars that are moved, were present in parent env.
  # if not, add an `if`.
  # while (FALSE) { x <- 3 } is not equivalent to x <- 3; while (FALSE) {  }
  res <- list()
  res$codes <- lapply(texts, li_one_file)
  res
}

# Executes loop-invariant code motion on one file of code.
#
# @param text A character vector with code to optimize.
#
li_one_file <- function(text) {
  pd <- parse_text(text)
  res_pd <- pd[pd$parent < 0, ] # keep lines with just comments
  new_pd <- pd[pd$parent >= 0, ] # keep lines with just comments
  new_pd <- li_one_pd(new_pd)
  res_pd <- rbind(res_pd, new_pd)

  if (nrow(res_pd) > 0) {
    res_pd <- res_pd[order(res_pd$pos_id), ]
  }

  deparse_data(res_pd)
}

# Executes loop-invariant code motion of a pd.
#
# @param pd A parsed data data.frame.
#
li_one_pd <- function(pd) {
  res_pd <- pd

  # get loops
  # For the moment, remove `repeat` loop invariant
  loop_parent_ids <- pd$parent[pd$token %in% c("FOR", "WHILE")] # %in% loops]

  # remove loops that have function calls inside
  loop_parent_ids <- loop_parent_ids[!sapply(loop_parent_ids, function(act_prnt) {
    "SYMBOL_FUNCTION_CALL" %in% get_children(pd, act_prnt)$token
  })]

  # remove loops that have next or break calls inside
  loop_parent_ids <- loop_parent_ids[!sapply(loop_parent_ids, function(act_prnt) {
    any(c("BREAK", "NEXT") %in% get_children(pd, act_prnt)$token)
  })]

  # for each loop do the invariant code motion
  for (loop_parent_id in loop_parent_ids) {
    res_pd <- li_in_loop(res_pd, loop_parent_id)
  }

  res_pd
}

# Executes loop-invariant code motion in one loop.
#
# @param pd A parsed data data.frame.
# @param id A numeric indicating the node ID of the loop.
#
li_in_loop <- function(pd, id) {
  lv_vars <- get_loop_variant_vars(pd, id)

  # start visiting the loop body
  visit_nodes <- utils::tail(pd$id[pd$parent == id], 1)
  to_unloop_ids <- c()
  while (length(visit_nodes) > 0) {
    new_visit <- c()
    for (act_parent in visit_nodes) {
      act_pd <- get_children(pd, act_parent)
      act_sblngs <- act_pd[act_pd$parent == act_parent, ]
      if (act_sblngs$token[[1]] == "'{'" || "';'" %in% act_sblngs$token) {
        new_visit <- c(new_visit, act_sblngs$id[!act_sblngs$terminal])
      } else if (all(
        act_pd$token %in%
          c(ops, precedence_ops, constants, assigns, "expr", "SYMBOL")
      )) {
        if (!any(lv_vars %in% act_pd$text[act_pd$token == "SYMBOL"])) {
          to_unloop_ids <- c(to_unloop_ids, act_parent)
        }
      }
    }
    visit_nodes <- new_visit
  }

  unloop_expr(pd, to_unloop_ids, id)
}

# Moves expressions that are inside a loop to outside of it.
#
# @param pd A parsed data data.frame.
# @param exprs_ids A numeric vector indicating the node IDs of the expressions.
# @param loop_id A numeric indicating the node ID of the parent loop.
#
unloop_expr <- function(pd, exprs_ids, loop_id) {
  if (length(exprs_ids) == 0) {
    return(pd)
  }

  res_pd <- remove_nodes(pd, exprs_ids)
  loop_pd <- get_children(pd, loop_id)
  exprs_pd <- get_children(loop_pd, exprs_ids)
  exprs <- deparse_data(exprs_pd)
  exprs <- sub("^\n*", "", exprs)

  loop_token <- pd$token[pd$parent == loop_id][[1]]
  if (loop_token == "WHILE") {
    loop_cond_id <- pd$id[pd$parent == loop_id & pd$token == "expr"][[1]]
    loop_cond <- sub(
      "^\n*", "",
      deparse_data(get_children(pd, loop_cond_id))
    )
    new_expr <- paste0("if (", loop_cond, ") {\n", exprs, "}")
  } else if (loop_token == "FOR") {
    loop_cond_id <- pd$id[pd$parent == loop_id & pd$token == "forcond"][[1]]
    loop_cond_id <- pd$id[pd$parent == loop_cond_id & pd$token == "expr"]
    loop_cond <- sub(
      "^\n*", "",
      deparse_data(get_children(pd, loop_cond_id))
    )
    new_expr <- paste0("if (length(", loop_cond, ") > 0) {\n", exprs, "}")
  }

  new_expr_pd <- parse_text(new_expr)
  new_expr_pd$prev_spaces[new_expr_pd$terminal][[1]] <-
    loop_pd$prev_spaces[loop_pd$terminal][[1]]
  new_expr_pd$line1[new_expr_pd$terminal][[1]] <-
    loop_pd$line1[loop_pd$terminal][[1]]
  new_expr_pd$prev_spaces[nrow(new_expr_pd)] <-
    loop_pd$prev_spaces[loop_pd$terminal][[1]]
  new_expr_pd$next_lines[nrow(new_expr_pd)] <- 1


  if (any(exprs_ids %in% utils::tail(pd$id[pd$parent == loop_id]))) {
    # if we are removing the entire loop body, then remove the loop
    res_pd <- remove_nodes(res_pd, loop_id)
  }

  rbind(
    res_pd,
    replace_pd(get_children(loop_pd, loop_id), new_expr_pd)
  )
}

# Returns which variables vary depending on loop execution.
#
# @param pd A parsed data data.frame.
# @param id A numeric indicating the node ID of the loop.
#
get_loop_variant_vars <- function(pd, id) {
  act_pd <- get_children(pd, id)
  act_sblngs <- act_pd[act_pd$parent == id, ]
  assigns_ids <- pd$parent[pd$token %in% assigns]

  # remove function definitions
  act_pd <- remove_nodes(
    act_pd,
    act_pd$id[act_pd$parent == act_pd$parent[act_pd$token == "FUNCTION"]]
  )

  lv_vars_ids <- c()
  # get for condition's IN vars
  # FOR '(' forcond ')' ; where forcond ~> SYMBOL IN expr
  lv_vars_ids <- c(lv_vars_ids, act_pd$id[which(act_pd$token == "IN") - 1])

  # get updated vars, e.g., x <- x + 1
  lv_vars_ids <- c(lv_vars_ids, get_updated_vars_ids(act_pd))

  old_lv_vars_ids <- c()
  while (length(lv_vars_ids) != length(old_lv_vars_ids)) {
    old_lv_vars_ids <- lv_vars_ids
    lv_vars <- pd$text[pd$id %in% lv_vars_ids]
    for (act_id in assigns_ids) {
      used_pd <- get_children(act_pd, get_assigned_exprs_ids(act_pd, act_id))
      if (any(used_pd$text[used_pd$token == "SYMBOL"] %in% lv_vars)) {
        lv_vars_ids <- c(lv_vars_ids, get_assigned_vars_ids(act_pd, act_id))
      }
    }
    lv_vars_ids <- unique(lv_vars_ids)
  }

  unique(act_pd$text[act_pd$id %in% lv_vars_ids])
}

# Returns the node ids of all vars that are being updated, e.g., x <- x + 1.
#
# @param pd A parsed data data.frame.
#
get_updated_vars_ids <- function(pd) {
  assigned_ids <- get_assigned_vars_ids(pd, get_roots(pd)$id)
  assigns_ids <- pd$parent[pd$token %in% assigns]

  assigned_ids[sapply(assigned_ids, function(act_id) {
    ancestors_ids <- get_ancestors(pd, act_id)
    ass_prnt_id <- intersect(assigns_ids, ancestors_ids)
    ass_expr_pd <- get_children(pd, get_assigned_exprs_ids(pd, ass_prnt_id))
    pd$text[pd$id == act_id] %in%
      ass_expr_pd$text[ass_expr_pd$token == "SYMBOL"] &&
      sum(ass_expr_pd$terminal) > 1
  })]
}

# Returns the ids of the pd exprs that are being assigned.
#
# @param pd A parsed data data.frame.
# @param id A numeric indicating the node ID to find assigns.
#
get_assigned_exprs_ids <- function(pd, id) {
  act_pd <- get_children(pd, id)
  # get parents of <- <<- -> ->> and =
  assign_exprs_prnts <- act_pd[
    act_pd$token %in% assigns & act_pd$text != ":=",
    "parent"
  ]
  # get the assigned expr pd id
  sapply(assign_exprs_prnts, function(assign_exprs_prnt) {
    aux <- act_pd[act_pd$parent == assign_exprs_prnt, ]
    while (any(assigns %in% aux$token)) {
      if (aux$token[[2]] == "RIGHT_ASSIGN") {
        aux <- act_pd[act_pd$parent == aux$id[[1]], ]
      } else {
        aux <- act_pd[act_pd$parent == aux$id[[3]], ]
      }
    }
    unique(aux$parent)
  })
}
