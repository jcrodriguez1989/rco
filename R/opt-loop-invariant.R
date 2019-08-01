#' Optimizer: Loop-invariant Code Motion
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

# Executes loop-invariant code motion on one file of code
#
# @param text A character vector with code to optimize.
#
li_one_file <- function(text) {
  fpd <- parse_text(text)
  res_fpd <- fpd[fpd$parent < 0, ] # keep lines with just comments
  new_fpd <- fpd[fpd$parent >= 0, ] # keep lines with just comments
  new_fpd <- li_one_fpd(new_fpd)
  res_fpd <- rbind(res_fpd, new_fpd)

  if (nrow(res_fpd) > 0) {
    res_fpd <- res_fpd[order(res_fpd$pos_id), ]
  }

  deparse_data(res_fpd)
}

# Executes loop-invariant code motion of a fpd tree
#
# @param fpd A flat parsed data data.frame .
#
li_one_fpd <- function(fpd) {
  res_fpd <- fpd

  # get loops
  # loop_parent_ids <- fpd$parent[fpd$token %in% loops]
  # For the moment, remove `repeat` loop invariant
  loop_parent_ids <- fpd$parent[fpd$token %in% c("FOR", "WHILE")]

  # remove loops that have function calls inside
  loop_parent_ids <- loop_parent_ids[!sapply(loop_parent_ids, function(act_prnt)
    "SYMBOL_FUNCTION_CALL" %in% get_children(fpd, act_prnt)$token)]

  # remove loops that have next or break calls inside
  loop_parent_ids <- loop_parent_ids[!sapply(loop_parent_ids, function(act_prnt)
    any(c("BREAK", "NEXT") %in% get_children(fpd, act_prnt)$token))]

  # for each loop do the invariant code motion
  for (loop_parent_id in loop_parent_ids) {
    res_fpd <- li_in_loop(res_fpd, loop_parent_id)
  }

  res_fpd
}

# Executes loop-invariant code motion in one loop
#
# @param fpd A flat parsed data data.frame .
# @param id Numeric indicating the node ID of the loop.
#
li_in_loop <- function(fpd, id) {
  lv_vars <- get_loop_variant_vars(fpd, id)

  # start visiting the loop body
  visit_nodes <- utils::tail(fpd$id[fpd$parent == id], 1)
  to_unloop_ids <- c()
  while (length(visit_nodes) > 0) {
    new_visit <- c()
    for (act_parent in visit_nodes) {
      act_pd <- get_children(fpd, act_parent)
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

  unloop_expr(fpd, to_unloop_ids, id)
}

# Moves expressions that are inside a loop to outside of it
#
# @param fpd A flat parsed data data.frame .
# @param exprs_ids Numeric indicating the node IDs of the expressions.
# @param loop_id Numeric indicating the node ID of the parent loop.
#
unloop_expr <- function(fpd, exprs_ids, loop_id) {
  if (length(exprs_ids) == 0) {
    return(fpd)
  }

  res_fpd <- remove_nodes(fpd, exprs_ids)
  loop_fpd <- get_children(fpd, loop_id)
  exprs_fpd <- get_children(loop_fpd, exprs_ids)
  exprs <- deparse_data(exprs_fpd)
  exprs <- sub("^\n*", "", exprs)

  loop_token <- fpd$token[fpd$parent == loop_id][[1]]
  if (loop_token == "WHILE") {
    loop_cond_id <- fpd$id[fpd$parent == loop_id & fpd$token == "expr"][[1]]
    loop_cond <- sub(
      "^\n*", "",
      deparse_data(get_children(fpd, loop_cond_id))
    )
    new_expr <- paste0("if (", loop_cond, ") {\n", exprs, "}")
  } else if (loop_token == "FOR") {
    loop_cond_id <- fpd$id[fpd$parent == loop_id & fpd$token == "forcond"][[1]]
    loop_cond_id <- fpd$id[fpd$parent == loop_cond_id & fpd$token == "expr"]
    loop_cond <- sub(
      "^\n*", "",
      deparse_data(get_children(fpd, loop_cond_id))
    )
    new_expr <- paste0("if (length(", loop_cond, ") > 0) {\n", exprs, "}")
  }

  new_expr_fpd <- parse_text(new_expr)
  new_expr_fpd$prev_spaces[new_expr_fpd$terminal][[1]] <-
    loop_fpd$prev_spaces[loop_fpd$terminal][[1]]
  new_expr_fpd$line1[new_expr_fpd$terminal][[1]] <-
    loop_fpd$line1[loop_fpd$terminal][[1]]
  new_expr_fpd$prev_spaces[nrow(new_expr_fpd)] <-
    loop_fpd$prev_spaces[loop_fpd$terminal][[1]]
  new_expr_fpd$next_lines[nrow(new_expr_fpd)] <- 1


  if (any(exprs_ids %in% utils::tail(fpd$id[fpd$parent == loop_id]))) {
    # if we are removing the entire loop body, then remove the loop
    res_fpd <- remove_nodes(res_fpd, loop_id)
  }

  rbind(
    res_fpd,
    replace_pd(get_children(loop_fpd, loop_id), new_expr_fpd)
  )
}

# Returns which variables vary depending on loop execution
#
# @param fpd A flat parsed data data.frame .
# @param id Numeric indicating the node ID of the loop.
#
get_loop_variant_vars <- function(fpd, id) {
  act_fpd <- get_children(fpd, id)
  act_sblngs <- act_fpd[act_fpd$parent == id, ]
  assigns_ids <- fpd$parent[fpd$token %in% assigns]

  # remove function definitions
  act_fpd <- remove_nodes(
    act_fpd,
    act_fpd$id[act_fpd$parent == act_fpd$parent[act_fpd$token == "FUNCTION"]]
  )

  lv_vars_ids <- c()
  # get for condition's IN vars
  # FOR '(' forcond ')' ; where forcond ~> SYMBOL IN expr
  lv_vars_ids <- c(lv_vars_ids, act_fpd$id[which(act_fpd$token == "IN") - 1])

  # get updated vars, e.g., x <- x + 1
  lv_vars_ids <- c(lv_vars_ids, get_updated_vars_ids(act_fpd))

  old_lv_vars_ids <- c()
  while (length(lv_vars_ids) != length(old_lv_vars_ids)) {
    old_lv_vars_ids <- lv_vars_ids
    lv_vars <- fpd$text[fpd$id %in% lv_vars_ids]
    for (act_id in assigns_ids) {
      used_fpd <- get_children(act_fpd, get_assigned_exprs_ids(act_fpd, act_id))
      if (any(used_fpd$text[used_fpd$token == "SYMBOL"] %in% lv_vars)) {
        lv_vars_ids <- c(lv_vars_ids, get_assigned_vars_ids(act_fpd, act_id))
      }
    }
    lv_vars_ids <- unique(lv_vars_ids)
  }

  unique(act_fpd$text[act_fpd$id %in% lv_vars_ids])
}

# Returns the node ids of all vars that are being updated, e.g., x <- x + 1
#
# @param fpd A flat parsed data data.frame .
#
get_updated_vars_ids <- function(fpd) {
  assigned_ids <- get_assigned_vars_ids(fpd, get_roots(fpd)$id)
  assigns_ids <- fpd$parent[fpd$token %in% assigns]

  assigned_ids[sapply(assigned_ids, function(act_id) {
    ancestors_ids <- get_ancestors(fpd, act_id)
    ass_prnt_id <- intersect(assigns_ids, ancestors_ids)
    ass_expr_fpd <- get_children(fpd, get_assigned_exprs_ids(fpd, ass_prnt_id))
    fpd$text[fpd$id == act_id] %in%
      ass_expr_fpd$text[ass_expr_fpd$token == "SYMBOL"] &&
      sum(ass_expr_fpd$terminal) > 1
  })]
}

# Returns the ids of the fpd exprs that are being assigned
#
# @param fpd A flat parsed data data.frame .
# @param id Numeric indicating the node ID to find assigns.
#
get_assigned_exprs_ids <- function(fpd, id) {
  act_fpd <- get_children(fpd, id)
  # get parents of <- <<- -> ->> and =
  assign_exprs_prnts <- act_fpd[
    act_fpd$token %in% assigns & act_fpd$text != ":=",
    "parent"
  ]
  # get the assigned expr fpd id
  sapply(assign_exprs_prnts, function(assign_exprs_prnt) {
    aux <- act_fpd[act_fpd$parent == assign_exprs_prnt, ]
    while (any(assigns %in% aux$token)) {
      if (aux$token[[2]] == "RIGHT_ASSIGN") {
        aux <- act_fpd[act_fpd$parent == aux$id[[1]], ]
      } else {
        aux <- act_fpd[act_fpd$parent == aux$id[[3]], ]
      }
    }
    unique(aux$parent)
  })
}
