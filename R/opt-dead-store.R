#' Optimizer: Dead Store Elimination.
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
  # todo: implement intelligent dead store? for example:
  # a <- 2; a <- 3; return(a) # remove first assign
  # todo: remove all variables that do not affect the returned value
  res <- list()
  res$codes <- lapply(texts, ds_one_file)
  res
}

# Executes dead store elimination on one text of code.
#
# @param text A character vector with code to optimize.
#
ds_one_file <- function(text) {
  fpd <- parse_text(text)
  fpd <- flatten_leaves(fpd)
  res_fpd <- fpd[fpd$parent < 0, ] # keep lines with just comments
  new_fpd <- fpd[fpd$parent >= 0, ] # keep lines with just comments

  # eliminate until no changes
  old_fpd <- NULL
  while (!isTRUE(all.equal(old_fpd, new_fpd))) {
    old_fpd <- new_fpd
    new_fpd <- ds_one_fpd(new_fpd)
  }

  res_fpd <- rbind(res_fpd, new_fpd)
  if (nrow(res_fpd) > 0) {
    res_fpd <- res_fpd[order(res_fpd$pos_id), ]
  }

  deparse_data(res_fpd)
}

# Executes dead store elimination of a fpd.
#
# @param fpd A flatten parsed data data.frame.
#
ds_one_fpd <- function(fpd) {
  res_fpd <- fpd
  # dead store happens only into functions, so get the expr of each function
  fun_ids <- fpd$id[fpd$token == "FUNCTION"]
  fun_prnt_ids <- fpd$parent[fpd$id %in% fun_ids]

  # for each function expr do a dead store removal
  for (id in fun_prnt_ids) {
    # eliminate dead stores from a function, and replace it in res_fpd
    ds_elim_fun <- dead_store_in_fun(res_fpd, id)
    res_fpd <- rbind(
      remove_nodes(res_fpd, id),
      ds_elim_fun
    )
  }

  res_fpd
}

# Executes dead store elimination in the expr of a function definition.
#
# @param fpd A flatten parsed data data.frame.
# @param id A numeric indicating the node ID of the function def expression.
#
dead_store_in_fun <- function(fpd, id) {
  # get the expression of the function
  expr_id <- rev(fpd$id[fpd$parent == id &
    fpd$token %in% c("expr", "SYMBOL", constants)])[[1]]

  # we are going to remove the variables that are assigned, but not used
  ass_vars <- ods_get_assigned_vars(fpd, expr_id)
  used_vars <- get_used_vars(fpd, expr_id)
  ass_to_remove <- setdiff(ass_vars, used_vars)

  res_fpd <- get_children(fpd, id)
  remove_assigns(res_fpd, ass_to_remove)
}

# Returns the names of the vars that are being assigned in an expr
# `=` , `<-`, `->` . Discards `<<-`, `->>`, `:=`.
#
# @param fpd A flatten parsed data data.frame.
# @param id A numeric indicating the node ID.
#
ods_get_assigned_vars <- function(fpd, id) {
  act_fpd <- get_children(fpd, id)

  # get assignation exprs ids
  ass_prnt_ids <- act_fpd[
    act_fpd$token %in% assigns &
      !act_fpd$text %in% c("<<-", "->>", ":="),
    "parent"
  ]

  # return the SYMBOL text from the right/left of '->'/'<-','=' assinment
  res <- unlist(sapply(ass_prnt_ids, function(act_prnt) {
    ass_sblngs <- act_fpd[act_fpd$parent == act_prnt, ]
    ass_idx <- 1
    if (ass_sblngs$token[[2]] == "RIGHT_ASSIGN") {
      ass_idx <- 3
    }
    var <- NULL
    if (ass_sblngs$token[[ass_idx]] == "SYMBOL") {
      var <- ass_sblngs$text[[ass_idx]]
    }
    var
  }))
  unique(res)
}

# Returns the names of the vars that are being used in an expr.
# Not counting assignations.
#
# @param fpd A flatten parsed data data.frame.
# @param id A numeric indicating the node ID.
#
get_used_vars <- function(fpd, id) {
  act_fpd <- get_children(fpd, id)

  # get assignation exprs ids
  ass_prnt_ids <- act_fpd$parent[act_fpd$token %in% assigns &
    act_fpd$text != ":="]

  # remove SYMBOLs that are being assigned
  assigned_ids <- unlist(sapply(ass_prnt_ids, function(act_prnt) {
    ass_sblngs <- act_fpd[act_fpd$parent == act_prnt, ]
    ass_idx <- 1
    if (ass_sblngs$token[[2]] == "RIGHT_ASSIGN") {
      ass_idx <- 3
    }
    var <- NULL
    if (ass_sblngs$token[[ass_idx]] == "SYMBOL") {
      var <- ass_sblngs$id[[ass_idx]]
    }
    var
  }))

  res <- act_fpd[
    act_fpd$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL") &
      !act_fpd$id %in% assigned_ids, "text"
  ]

  unique(res)
}

# Returns a new fpd with desired assignations removed.
#
# @param fpd A flatten parsed data data.frame.
# @param vars Character vector with names of vars to remove.
#
remove_assigns <- function(fpd, vars) {
  for (act_var in vars) {
    act_prnt_ids <- fpd[fpd$text == act_var & fpd$token == "SYMBOL", "parent"]
    # remove `<<-` `->>` and `:=` parents
    act_prnt_ids <- fpd$parent[
      fpd$parent %in% act_prnt_ids &
        fpd$token %in% assigns &
        fpd$text %in% c("<-", "=", "->")
    ]

    for (act_prnt_id in act_prnt_ids) {
      # eliminate each assignation of the dead store
      if (!act_prnt_id %in% fpd$id) {
        next
      }

      ass_fpd <- get_children(fpd, act_prnt_id)
      new_ass_fpd <- ass_fpd
      act_prnt <- ass_fpd[ass_fpd$id == act_prnt_id, ]
      act_sblngs <- ass_fpd[ass_fpd$parent == act_prnt_id, ]

      # keep only the expression
      keep_fpd <- act_sblngs[3, ]
      if (act_sblngs$token[[2]] == "RIGHT_ASSIGN") {
        keep_fpd <- act_sblngs[1, ]
      }

      # remove assignment parent expr and siblings
      new_ass_fpd <- new_ass_fpd[!new_ass_fpd$id %in%
        c(act_prnt_id, act_sblngs$id), ]
      new_ass_fpd <- rbind(new_ass_fpd, keep_fpd)

      # the expr to keep will skip the assignment expr in the fpd
      new_ass_fpd[new_ass_fpd$id == keep_fpd$id, "parent"] <- act_prnt$parent

      # some fixes on the resulting fpd
      new_ass_fpd <- new_ass_fpd[order(new_ass_fpd$pos_id), ]
      new_ass_fpd[, c("next_spaces", "next_lines", "prev_spaces")] <-
        replace_pd(ass_fpd, new_ass_fpd)[
          , c("next_spaces", "next_lines", "prev_spaces")
        ]
      fpd <- rbind(
        remove_nodes(fpd, act_prnt_id),
        new_ass_fpd
      )
    }
  }
  fpd
}
