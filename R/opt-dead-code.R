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
dead_code_one <- function(text, fold_floats) {
  pd <- parse_flat_data(text, include_text = TRUE)
  pd <- flatten_leaves(pd)
  pd <- eq_assign_to_expr(pd)
  res_pd <- pd[pd$parent < 0,] # keep lines with just comments
  new_pd <- pd[pd$parent >= 0,] # keep lines with just comments

  # fold until no changes
  old_pd <- NULL
  while (!isTRUE(all.equal(old_pd, new_pd))) {
    old_pd <- new_pd
    new_pd <- one_propagate(new_pd, c())$fpd
  }
  res_pd <- rbind(res_pd, new_pd)
  res_pd <- res_pd[order(res_pd$pos_id),]
  deparse_flat_data(res_pd)
}

# Executes constant propagation of a tree
#
# @param fpd a flat parsed data data.frame .
# @param values A named vector of variables and their value .
#
one_propagate <- function(fpd, values) {
  act_nodes <- get_roots(fpd)
  res_fpd <- act_nodes[act_nodes$terminal,] # they are {, }, (, )
  act_nodes <- act_nodes[!act_nodes$terminal,]

  for (i in seq_len(nrow(act_nodes))) {
    act_node <- act_nodes[i,]
    act_fpd <- get_children(fpd, act_node$id)
    if (is_constant_var_expr(fpd, act_node$id)) {
      # if is constant var ( e.g. x <- 3 ) add it to values
      act_val <- get_constant_var(act_fpd, act_node$id)
      values[names(act_val)] <- act_val # add act_val to values
      res_fpd <- rbind(res_fpd, act_fpd)
    } else if (only_uses_ops(fpd, act_node$id)) {
      # if uses a constant var ( e.g. x + 3 ) then replace expression
      # if it doesnt use a constant var, it returns the same
      res_fpd <- rbind(res_fpd,
                       replace_constant_vars(act_fpd, act_node$id, values))
      # replace the constant var by the value and replace in fpd
    } else if (is_function_call(fpd, act_node$id)) {
      # if function call, then empty the values  :'(
      # ( e.g. rm(list=ls()) )
      res_fpd <- rbind(res_fpd,
                       replace_constant_vars(act_fpd, act_node$id, values))
      values <- c()
    } else if (is_loop(fpd, act_node$id)) {
      # if it is a loop, then remove the in-loop assigned variables from values
      loop_ass_vars <- get_assigned_vars(fpd, act_node$id)
      values <- values[!names(values) %in% loop_ass_vars]
      childs <- fpd[fpd$parent == act_node$id,]
      res_fpd <- rbind(res_fpd, act_node)
      res_fpd <- rbind(res_fpd, childs[childs$terminal,])
      # work on loop condition, and on body
      exprs_fpd <- childs[!childs$terminal,]
      for (i in seq_len(nrow(exprs_fpd))) {
        res <- one_propagate(get_children(fpd, exprs_fpd[i, "id"]), values)
        res_fpd <- rbind(res_fpd, res$fpd)
        values <- res$values
      }
    } else if (is_assignment(fpd, act_node$id)) {
      # note that it is an assignment, but not of constant value
      # and can be x <- y <- z * 7
      res_fpd <- rbind(res_fpd, act_node) # keep root node
      childs <- fpd[fpd$parent == act_node$id,]
      # get indexes that are not the expr to assing
      save_idxs <- get_assign_indexes(childs$token)
      res_fpd <- rbind(res_fpd, childs[save_idxs,])
      childs <- childs[-save_idxs,]
      child_pd <- get_children(fpd, childs$id)
      if (nrow(childs) == 1 && childs$token == "expr") {
        # it is an expr
        res <- one_propagate(child_pd, values)
        res_fpd <- rbind(res_fpd, res$fpd)
        values <- res$values
      } else {
        # it is a SYMBOL flattened by us
        res_fpd <- rbind(res_fpd,
                         replace_constant_vars(child_pd, act_node$id, values))
      }
      # remove assigned var from values
      values <- values[names(values) !=
                         get_assigned_var(act_fpd, act_node$id)]
    } else if (is_function_def(act_fpd, act_node$id)) {
      # it has a new env, so dont pass constant values
      # note that it is an assignment, but not of constant value
      childs <- fpd[fpd$parent == act_node$id,]
      # FUNCTION '(' formlist ')' cr expr_or_assign
      # formlist can have exprs
      res_fpd <- rbind(res_fpd, act_node)
      res_fpd <- rbind(res_fpd, childs[childs$terminal,])
      exprs_fpd <- childs[!childs$terminal,]
      for (i in seq_len(nrow(exprs_fpd)-1)) {
        # dont propagate in function arg expressions
        res_fpd <- rbind(res_fpd, get_children(act_fpd, exprs_fpd[i, "id"]))
      }
      # propagate on the function body ( with new env c() )
      res <- one_propagate(
        get_children(act_fpd, exprs_fpd[nrow(exprs_fpd), "id"]), c())
      res_fpd <- rbind(res_fpd, res$fpd)
    } else {
      # propagate on act_node childs
      # get children w/o parent, and keep propagating
      childs <- fpd[fpd$parent == act_node$id,]
      res_fpd <- rbind(res_fpd, act_node)
      res_fpd <- rbind(res_fpd, childs[childs$terminal,])
      res <- one_propagate(
        get_children(fpd, childs[!childs$terminal, "id"]),
        values)
      values <- res$values
      res_fpd <- rbind(res_fpd, res$fpd)
    }
  }
  res_fpd <- res_fpd[order(res_fpd$pos_id),]
  return(list(fpd = res_fpd, values = values))
}

# Returns a logical indicating if a node is assignment of constant to var
#
# @param fpd a flat parsed data data.frame .
# @param id Numeric indicating the node ID.
#
is_constant_var_expr <- function(fpd, id) {
  # is an assignment (might be recursive),
  # and the remaining expr is a constant or -constant
  if (!is_assignment(fpd, id)) {
    return(FALSE)
  }
  act_pd <- fpd[fpd$parent == id,]
  if (nrow(act_pd) != 3) {
    browser()
  }
  if (act_pd$token[[1]] %in% c("'('", "'{'") &&
      act_pd$token[[3]] %in% c("')'", "'}'")) {
    return(is_constant_var_expr(fpd, act_pd[2, "id"]))
  }
  is_constant_or_minus(fpd, act_pd[3, "id"]) ||
    is_constant_or_minus(fpd, act_pd[1, "id"]) ||
    is_constant_var_expr(fpd, act_pd[3, "id"]) ||
    is_constant_var_expr(fpd, act_pd[1, "id"])
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
  act_pd <- fpd[fpd$parent == id,]
  act_pd <- get_children(fpd, id)
  act_var <- act_pd[act_pd$token == "SYMBOL", "text"]
  act_code <- act_pd[act_pd$token %in% c("'-'", constants),]
  res <- eval(parse(text = paste0(act_code$text)))
  res <- rep(res, length(act_var))
  names(res) <- act_var
  return(res)
}

# Returns a logical indicating if a node is only operators and vars
#
# @param fpd a flat parsed data data.frame .
# @param id Numeric indicating the node ID.
#
only_uses_ops <- function(fpd, id) {
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
  child <- get_children(fpd, id)
  return(
    length(unique(child$token[child$token %in% assigns])) == 1 &&
      nrow(act_pd) == 3 && (
        (act_pd$token[[1]] %in% c("'('", "'{'") &&
           act_pd$token[[3]] %in% c("')'", "'}'") &&
           is_assignment(fpd, act_pd[2, "id"])) ||
          (act_pd$token[[1]] == "SYMBOL" &&
             act_pd$token[[2]] %in% c("LEFT_ASSIGN", "EQ_ASSIGN")) ||
          (
            act_pd$token[[3]] == "SYMBOL" &&
              act_pd$token[[2]] == "RIGHT_ASSIGN"
          )
      )
  )
}

# From a vector of tokens returns the index of everything except the value
#
# @param tokens Vector of tokens
#
get_assign_indexes <- function(tokens) {
  idxs <- which(tokens %in% assigns)
  aux <- rep(-1, length(idxs))
  if (all(tokens[idxs] == "RIGHT_ASSIGN")) {
    aux <- -aux
  }
  idxs <- c(idxs, idxs+aux)
  idxs <- c(idxs, which(tokens %in% precedence_ops))
  return(idxs)
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
