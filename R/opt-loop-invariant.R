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
  return(res)
}

# Executes loop-invariant code motion on one file of code
#
# @param text A character vector with code to optimize.
#
li_one_file <- function(text) {
  fpd <- parse_flat_data(text)
  # fpd <- flatten_leaves(fpd)
  res_fpd <- fpd[fpd$parent < 0, ] # keep lines with just comments
  new_fpd <- fpd[fpd$parent >= 0, ] # keep lines with just comments
  new_fpd <- li_one_fpd(new_fpd)
  res_fpd <- rbind(res_fpd, new_fpd)

  if (nrow(res_fpd) > 0) {
    res_fpd <- res_fpd[order(res_fpd$pos_id), ]
  }

  deparse_flat_data(res_fpd)
}

# Executes loop-invariant code motion of a fpd tree
#
# @param fpd A flat parsed data data.frame .
#
li_one_fpd <- function(fpd) {
  res_fpd <- fpd

  # get loops
  loop_parent_ids <- fpd$parent[fpd$token %in% loops]

  # remove loops that have function calls inside
  loop_parent_ids <- loop_parent_ids[!sapply(loop_parent_ids, function(act_prnt)
    "SYMBOL_FUNCTION_CALL" %in% get_children(fpd, act_prnt)$token)]

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
  res_fpd <- fpd
  lv_vars <- get_loop_variant_vars(fpd, id)

  # start visiting the loop body
  visit_nodes <- utils::tail(res_fpd$id[res_fpd$parent == id], 1)
  while (length(visit_nodes) > 0) {
    new_visit <- c()
    for (act_parent in visit_nodes) {
      act_pd <- get_children(res_fpd, act_parent)
      act_sblngs <- act_pd[act_pd$parent == act_parent, ]
      if (act_sblngs$token[[1]] == "'{'" || "';'" %in% act_sblngs$token) {
        new_visit <- c(new_visit, act_sblngs$id[!act_sblngs$terminal])
      } else if (any(c(loops, "IF") %in% act_sblngs$token)) {
        new_visit <- c(
          new_visit,
          utils::tail(act_sblngs$id[!act_sblngs$terminal], 1)
        )
      } else if (all(
        act_pd$token %in%
          c(ops, precedence_ops, constants, assigns, "expr", "SYMBOL")
      )) {
        if (!any(lv_vars %in% get_read_vars(act_pd, act_parent))) {
          res_fpd <- unloop_expr(res_fpd, act_parent, id)
        }
      }
    }
    visit_nodes <- new_visit
  }
  res_fpd
}

# Moves an expression that is inside a loop to outside of it
#
# @param fpd A flat parsed data data.frame .
# @param expr_id Numeric indicating the node ID of the expression.
# @param loop_id Numeric indicating the node ID of the parent loop.
#
unloop_expr <- function(fpd, expr_id, loop_id) {
  expr_fpd <- get_children(fpd, expr_id)
  res_fpd <- remove_nodes(fpd, expr_id)
  expr_fpd$prev_spaces[expr_fpd$terminal][[1]] <-
    res_fpd$prev_spaces[res_fpd$terminal][[1]]
  expr_fpd$line1[expr_fpd$terminal][[1]] <- res_fpd$line1[res_fpd$terminal][[1]]
  rbind(
    res_fpd,
    replace_pd(get_children(res_fpd, loop_id), expr_fpd)
  )
}

# Returns which variables vary within a loop
#
# @param fpd A flat parsed data data.frame .
# @param id Numeric indicating the node ID of the loop.
#
get_loop_variant_vars <- function(fpd, id) {
  act_fpd <- get_children(fpd, id)
  act_sblngs <- act_fpd[act_fpd$parent == id, ]
  lv_vars <- c()

  # remove function definitions
  act_fpd <- remove_nodes(
    act_fpd,
    act_fpd$id[act_fpd$parent == act_fpd$parent[act_fpd$token == "FUNCTION"]]
  )

  # get for condition's IN vars
  # FOR '(' forcond ')' ; where forcond ~> SYMBOL IN expr
  lv_vars <- c(lv_vars, act_fpd$id[which(act_fpd$token == "IN") - 1])

  lv_vars <- c(
    lv_vars,
    unlist(get_assigns_ids(act_fpd, act_sblngs$id[!act_sblngs$terminal]))
  )

  unique(act_fpd$text[act_fpd$id %in% lv_vars])
}

# Returns the names of the vars which value is read in an expr.
#
# @param fpd a flat parsed data data.frame .
# @param id Numeric indicating the node ID.
#
get_read_vars <- function(fpd, id) {
  act_fpd <- get_children(fpd, id)

  # get assignation exprs ids
  ass_prnt_ids <- act_fpd$parent[act_fpd$token %in% assigns]

  # remove SYMBOLs that are being assigned
  assigned_ids <- unlist(lapply(ass_prnt_ids, function(act_prnt) {
    ass_sblngs <- act_fpd[act_fpd$parent == act_prnt, ]
    ass_idx <- -1
    if ("RIGHT_ASSIGN" %in% ass_sblngs$token) {
      ass_idx <- 1
    }
    sapply(which(ass_sblngs$token %in% assigns), function(expr_idx) {
      act_chld <- get_children(act_fpd, ass_sblngs$id[expr_idx + ass_idx])
      act_chld$id[act_chld$token == "SYMBOL"][[1]]
    })
  }))

  res <- act_fpd[
    act_fpd$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL") &
      !act_fpd$id %in% assigned_ids, "text"
  ]
  unique(res)
}
