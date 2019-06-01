#' Optimizer: Constant Propagation
#'
#' Performs one constant propagation pass.
#' Carefully examine the results after running this function!
#'
#' @param texts A list of character vectors with the code to optimize.
#'
#' @examples
#' code <- paste(
#'   "i <- 170",
#'   "x <- -170",
#'   "y <- x + 124",
#'   "z <- i - 124",
#'   sep = "\n"
#' )
#' opt_constant_propagation(list(code))
#' @export
#'
opt_constant_propagation <- function(texts) {
  res <- list()
  res$codes <- lapply(texts, constant_prop_one)
  return(res)
}

# Executes constant propagation on one text of code code
#
# @param text A character vector with code to optimize.
#
constant_prop_one <- function(text, fold_floats) {
  pd <- parse_flat_data(text, include_text = TRUE)
  pd <- flatten_leaves(pd)
  pd <- eq_assign_to_expr(pd)
  new_pd <- pd[pd$parent < 0,] # keep lines with just comments
  # todo: keep propagating till no change
  new_pd <- rbind(new_pd, one_propagate(pd, c())$fpd)
  new_pd <- new_pd[order(new_pd$pos_id),]
  deparse_flat_data(new_pd)
}

# Executes constant propagation of a tree
#
# @param fpd a flat parsed data data.frame .
# @param values A named vector of variables and their value .
#
one_propagate <- function(fpd, values) {
  act_nodes <- get_roots(fpd)
  new_fpd <- act_nodes[act_nodes$terminal,] # they are {, }, (, )
  act_nodes <- act_nodes[!act_nodes$terminal,]

  for (i in seq_len(nrow(act_nodes))) {
    act_node <- act_nodes[i,]
    act_fpd <- get_children(fpd, act_node$id)
    if (is_constant_var_expr(act_fpd, act_node$id)) {
      # if is constant var ( e.g. x <- 3 ) add it to values
      act_val <- get_constant_var(act_fpd, act_node$id)
      values[names(act_val)] <- act_val # add act_val to values
      new_fpd <- rbind(new_fpd, get_children(act_fpd, act_node$id))
    } else if (only_uses_ops(act_fpd, act_node$id, names(values))) {
      # if uses a constant var ( e.g. x + 3 ) then replace expression
      # if it doesnt use a constant var, it returns the same
      new_fpd <- rbind(new_fpd,
                       replace_constant_vars(act_fpd, act_node$id, values))
      # replace the constant var by the value and replace in fpd
    } else if (is_function_call(act_fpd, act_node$id)) {
      childs <- get_children(act_fpd, act_node$id)
      new_fpd <- rbind(new_fpd,
                       replace_constant_vars(childs, act_node$id, values))
      # if function call, then empty the values  :'(
      # as from functions, parent envs can be modified
      values <- c()
    } else if (is_loop(act_fpd, act_node$id)) {
      # if it is a loop, then remove the in-loop assigned variables from values
      loop_ass_vars <- get_assigned_vars(act_fpd, act_node$id)
      values <- values[!names(values) %in% loop_ass_vars]
      childs <- act_fpd[act_fpd$parent == act_node$id,]
      new_fpd <- rbind(new_fpd, childs[childs$terminal,])
      exprs_fpd <- childs[!childs$terminal,]
      for (i in seq_len(nrow(exprs_fpd))) {
        res <- one_propagate(get_children(act_fpd, exprs_fpd[i, "id"]), values)
        new_fpd <- rbind(new_fpd, res$fpd)
        values <- res$values
      }
    } else {
      # propagate on act_node childs
      # get children w/o parent, and keep propagating
      child_pd <- get_children(act_fpd, act_node$id)
      child_pd <- child_pd[child_pd$id != act_node$id,] # remove parent
      new_fpd <- rbind(new_fpd, act_node)
      if (is_assignment(act_fpd, act_node$id)) {
        # note that it is an assignment, but not of constant value
        childs <- child_pd[child_pd$parent == act_node$id,]
        assignation_idxs <- 1:2
        if (childs$token[[2]] == "RIGHT_ASSIGN") {
          assignation_idxs <- 2:3
        }
        new_fpd <- rbind(new_fpd, childs[assignation_idxs,])
        child_pd <- child_pd[!child_pd$id %in% childs[assignation_idxs, "id"],]
        if (get_roots(child_pd)$token != "expr") {
          # it is a SYMBOL flattened by us
          res <- list(
            fpd = replace_constant_vars(child_pd, act_node$id, values),
            values = values
          )
        } else {
          # it is an expr
          res <- one_propagate(child_pd, values)
        }
        values <- res$values
        # remove assigned var from values
        values <- values[names(values) !=
                           get_assigned_var(act_fpd, act_node$id)]
      } else if (is_function_def(act_fpd, act_node$id)) {
        # it has a new env, so no constant values
        # note that it is an assignment, but not of constant value
        childs <- child_pd[child_pd$parent == act_node$id,]
        # FUNCTION '(' formlist ')' cr expr_or_assign
        # formlist can have expr
        new_fpd <- rbind(new_fpd, childs[childs$terminal,])
        exprs_fpd <- childs[!childs$terminal,]
        for (i in seq_len(nrow(exprs_fpd)-1)) {
          # dont propagate in function arg expressions
          new_fpd <- rbind(new_fpd, get_children(child_pd, exprs_fpd[i, "id"]))
        }
        # propagate on the function body ( with new env c() )
        res <- one_propagate(
          get_children(child_pd, exprs_fpd[nrow(exprs_fpd), "id"]), c())
      } else {
        res <- one_propagate(child_pd, values)
        values <- res$values # add new values to values
      }
      new_fpd <- rbind(new_fpd, res$fpd)
    }
  }
  return(list(fpd = new_fpd, values = values))
}

# Returns a logical indicating if a node is assignment of constant to var
#
# @param fpd a flat parsed data data.frame .
# @param id Numeric indicating the node ID.
#
is_constant_var_expr <- function(fpd, id) {
  # is an assignment, and the remaining expr is a constant or -constant
  child <- fpd[fpd$parent == id,]
  is_assignment(fpd, id) && (
    is_constant_or_minus(fpd, child[3, "id"]) ||
      is_constant_or_minus(fpd, child[1, "id"]))
}

# Returns a logical indicating if a node is a constant or -constant
#
# @param fpd a flat parsed data data.frame .
# @param id Numeric indicating the node ID.
#
is_constant_or_minus <- function(fpd, id) {
  act_pd <- get_children(fpd, id)
  act_tokens <- act_pd$token
  act_tokens <- setdiff(act_tokens, c(precedence_ops, "expr"))
  (length(act_tokens) == 1 && act_tokens %in% constants) ||
    (length(act_tokens) == 2 && act_tokens[[1]] == "'-'" &&
       act_tokens[[2]] %in% constants)
}

# Returns a named value c(var name=var constant value) if is_constant_var_expr
#
# @param fpd a flat parsed data data.frame .
# @param id Numeric indicating the node ID.
#
get_constant_var <- function(fpd, id) {
  if (!is_constant_var_expr(fpd, id)) {
    return(NULL)
  }
  act_pd <- get_children(fpd, id)
  act_var <- act_pd[act_pd$token == "SYMBOL", "text"]
  act_code <- act_pd[act_pd$token %in% c("'-'", constants),]
  res <- eval(parse(text = paste0(act_code$text)))
  names(res) <- act_var
  return(res)
}

# Returns a logical indicating if a node is only operators and vars
#
# @param fpd a flat parsed data data.frame .
# @param id Numeric indicating the node ID.
# @param constant_vars A character vector with constant vars names
#
only_uses_ops <- function(fpd, id, constant_vars) {
  act_pd <- get_children(fpd, id)
  all(act_pd$token %in% c("expr", "SYMBOL", constants, ops, precedence_ops))
}

# Returns a new flat parsed data where constant vars being replaced by their
# value
#
# @param fpd a flat parsed data data.frame .
# @param id Numeric indicating the node ID.
# @param constant_vars A character vector with named constant vars
#
replace_constant_vars <- function(fpd, id, constant_vars) {
  new_fpd     <- fpd[!fpd$text %in% names(constant_vars),]
  to_edit_fpd <- fpd[fpd$text %in% names(constant_vars),]
  for (i in seq_len(nrow(to_edit_fpd))) {
    act_fpd <- to_edit_fpd[i,]
    new_act_fpd <- flatten_leaves(
      parse_flat_data(constant_vars[[act_fpd$text]]))
    # new ids will be old_id + _ + new_id
    new_act_fpd$id <- paste0(act_fpd$id, "_", new_act_fpd$id)
    # keep old parent for new fpd
    new_act_fpd$parent[new_act_fpd$parent != 0] <-
      paste0(act_fpd$id, "_", new_act_fpd$parent[new_act_fpd$parent != 0])
    new_act_fpd$parent[new_act_fpd$parent == 0] <- act_fpd$parent
    # .Machine$double.eps not working instead of 10e-5
    new_act_fpd$pos_id <- act_fpd$pos_id +
      (10e-5 * (seq_len(nrow(new_act_fpd))-1))
    new_act_fpd$next_spaces[nrow(new_act_fpd)] <- act_fpd$next_spaces
    new_act_fpd$next_lines[nrow(new_act_fpd)] <- act_fpd$next_lines
    new_act_fpd$prev_spaces[which(new_act_fpd$terminal)[[1]]] <-
      act_fpd$prev_spaces
    new_fpd <- rbind(new_fpd, new_act_fpd)
  }
  new_fpd[order(new_fpd$pos_id),]
}

# Returns a logical indicating if a node is a function call
#
# @param fpd a flat parsed data data.frame .
# @param id Numeric indicating the node ID.
#
is_function_call <- function(fpd, id) {
  act_pd <- fpd[fpd$parent == id,]
  "SYMBOL_FUNCTION_CALL" %in% act_pd$token
}

# Returns a logical indicating if a node is a loop
#
# @param fpd a flat parsed data data.frame .
# @param id Numeric indicating the node ID.
#
is_loop <- function(fpd, id) {
  act_pd <- fpd[fpd$parent == id,]
  any(loops %in% act_pd$token)
}

# Returns a logical indicating if a node is an assignment
#
# @param fpd a flat parsed data data.frame .
# @param id Numeric indicating the node ID.
#
is_assignment <- function(fpd, id) {
  # it has to be one of
  #   SYMBOL {LEFT_ASSIGN EQ_ASSIGN}_ASSIGN expr
  #   expr RIGHT_ASSIGN SYMBOL
  act_pd <- fpd[fpd$parent == id,]
  if (nrow(act_pd) != 3)
    return(FALSE)
  # nrow(act_pd) == 3 &&
    (act_pd$token[[1]] == "SYMBOL" &&
       act_pd$token[[2]] %in% c("LEFT_ASSIGN", "EQ_ASSIGN")) ||
    (act_pd$token[[2]] == "RIGHT_ASSIGN" &&
       act_pd$token[[3]] == "SYMBOL")
}

# Returns the name of the var that is beign assigned
#
# @param fpd a flat parsed data data.frame .
# @param id Numeric indicating the node ID.
#
get_assigned_var <- function(fpd, id) {
  if (!is_assignment(fpd, id)) {
    return("")
  }
  act_pd <- fpd[fpd$parent == id,]
  res <- act_pd$text[[1]]
  if (act_pd$token[[2]] == "RIGHT_ASSIGN") {
    res <- act_pd$text[[3]]
  }
  return(res)
}

# Returns the names of the vars that are beign assigned in an expr
#
# @param fpd a flat parsed data data.frame .
# @param id Numeric indicating the node ID.
#
get_assigned_vars <- function(fpd, id) {
  act_ids <- id
  res <- c()
  while(length(act_ids) > 0) {
    res <- c(res, sapply(act_ids, get_assigned_var, fpd = fpd))
    res <- c(res, sapply(act_ids, get_assigned_var_extra, fpd = fpd))
    act_ids <- fpd[fpd$parent %in% act_ids & !fpd$terminal, "id"]
  }
  unique(res)
}

# Returns the name of the var that is beign assigned by IN
#
# @param fpd a flat parsed data data.frame .
# @param id Numeric indicating the node ID.
#
get_assigned_var_extra <- function(fpd, id) {
  act_pd <- fpd[fpd$parent == id,]
  res <- ""
  if ("IN" %in% act_pd$token) {
    res <- act_pd[which("IN" == act_pd$token)-1, "text"]
  }
  return(res)
}

# Returns a logical indicating if a node is a function definition
#
# @param fpd a flat parsed data data.frame .
# @param id Numeric indicating the node ID.
#
is_function_def <- function(fpd, id) {
  act_pd <- fpd[fpd$parent == id,]
  "FUNCTION" %in% act_pd$token
}

# # Get IDs of exprs that contain assignment of a constant to a variable
# #
# # @param fpd a flat parsed data data.frame .
# #
# get_constant_var_exprs <- function(fpd) {
#   res <- c()
#   visit_nodes <- get_roots(fpd)$id
#   while (length(visit_nodes) > 0) {
#     new_visit_nodes <- c()
#     # we are only going to check expressions
#     visit_nodes <- visit_nodes[fpd[fpd$id %in% visit_nodes, "token"] == "expr"]
#     for (act_parent in visit_nodes) {
#       # act_parent <- visit_nodes[[1]]
#       if (is_constant_var_expr(fpd, act_parent)) {
#         res <- c(res, act_parent)
#       } else {
#         new_visit_nodes <- c(new_visit_nodes,
#                              fpd[fpd$parent == act_parent &
#                                    fpd$token == "expr", "id"])
#       }
#     }
#     visit_nodes <- new_visit_nodes
#   }
#   return(res)
# }
