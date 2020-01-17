#' Optimizer: Constant Propagation.
#'
#' Performs one constant propagation pass.
#' Carefully examine the results after running this function!
#'
#' @param texts A list of character vectors with the code to optimize.
#' @param in_fun_call A logical indicating whether it should propagate in
#'   function calls. Note: this could change the semantics of the program.
#'
#' @examples
#' code <- paste(
#'   "i <- 170",
#'   "x <- -170",
#'   "y <- x + 124",
#'   "z <- i - 124",
#'   sep = "\n"
#' )
#' cat(opt_constant_propagation(list(code))$codes[[1]])
#' @export
#'
opt_constant_propagation <- function(texts, in_fun_call = FALSE) {
  res <- list()
  res$codes <- lapply(texts, cp_one_file, in_fun_call = in_fun_call)
  res
}

# Executes constant propagation on one text of code.
#
# @param text A character vector with code to optimize.
# @param in_fun_call A logical indicating whether it should propagate in
#   function calls. Note: this could change the semantics of the program.
#
cp_one_file <- function(text, in_fun_call) {
  fpd <- parse_text(text)
  fpd <- flatten_leaves(fpd)
  fpd <- eq_assign_to_expr(fpd)
  res_fpd <- fpd[fpd$parent < 0, ] # keep lines with just comments
  new_fpd <- fpd[fpd$parent >= 0, ] # keep lines with just comments
  if (nrow(new_fpd) == 0) {
    return(deparse_data(res_fpd))
  }

  # propagate until no changes
  old_fpd <- NULL
  while (!isTRUE(all.equal(old_fpd, new_fpd))) {
    old_fpd <- new_fpd
    new_fpd <- cp_one_fpd(new_fpd, list(), in_fun_call)$fpd
  }
  res_fpd <- rbind(res_fpd, new_fpd)
  res_fpd <- res_fpd[order(res_fpd$pos_id), ]
  deparse_data(res_fpd)
}

# Executes constant propagation of a fpd.
#
# @param fpd A flatten parsed data data.frame.
# @param values A named list of variables and their value.
# @param in_fun_call A logical indicating whether it should propagate in
#   function calls. Note: this could change the semantics of the program.
#
cp_one_fpd <- function(fpd, values, in_fun_call) {
  act_nodes <- get_roots(fpd)
  res_fpd <- act_nodes[act_nodes$terminal, ] # they are {, }, (, )
  act_nodes <- act_nodes[!act_nodes$terminal, ]

  for (i in seq_len(nrow(act_nodes))) {
    act_node <- act_nodes[i, ]
    act_fpd <- get_children(fpd, act_node$id)
    if (is_constant_var_expr(fpd, act_node$id)) {
      # if is constant var ( e.g. x <- 3 ) add it to values
      act_val <- get_constant_var(act_fpd, act_node$id)
      values[names(act_val)] <- act_val # add act_val to values
      res_fpd <- rbind(res_fpd, act_fpd)
    } else if (only_uses_ops(fpd, act_node$id)) {
      # if uses a constant var ( e.g. x + 3 ) then replace expression
      # if it doesnt use a constant var, it returns the same
      res_fpd <- rbind(
        res_fpd,
        replace_constant_vars(act_fpd, act_node$id, values)
      )
      # replace the constant var by the value and replace in fpd
    } else if (is_function_call(fpd, act_node$id)) {
      # if function call, then empty the values :'(
      # ( e.g. rm(list=ls()) )
      fun_defs <- act_fpd$parent[act_fpd$token == "FUNCTION"]
      if (length(fun_defs) > 1) {
        # remove if a fun def is child of another
        fun_defs <- fun_defs[sapply(fun_defs, function(x) {
          sum(get_ancestors(act_fpd, x) %in% fun_defs) == 1
        })]
      }

      fun_call_fpd <- remove_nodes(act_fpd, fun_defs)
      if (in_fun_call) {
        fun_call_fpd <- replace_constant_vars(fun_call_fpd, act_node$id, values)
      }

      res_fpd <- rbind(
        res_fpd, fun_call_fpd,
        # should I pass values to function defs propagation?
        cp_one_fpd(get_children(act_fpd, fun_defs), list(), in_fun_call)$fpd
      )
      values <- list()
    } else if (is_loop(fpd, act_node$id)) {
      # if it is a loop, then remove the in-loop assigned variables from values
      loop_ass_vars <- ocp_get_assigned_vars(fpd, act_node$id)
      values <- values[!names(values) %in% loop_ass_vars]
      childs <- fpd[fpd$parent == act_node$id, ]
      res_fpd <- rbind(res_fpd, act_node)
      res_fpd <- rbind(res_fpd, childs[childs$terminal, ])
      # work on loop condition, and on body
      exprs_fpd <- childs[!childs$terminal, ]
      loop_values <- values
      for (j in seq_len(nrow(exprs_fpd))) {
        res <- cp_one_fpd(
          get_children(fpd, exprs_fpd[j, "id"]),
          loop_values, in_fun_call
        )
        res_fpd <- rbind(res_fpd, res$fpd)
        # cant keep these values, because maybe the loop is never executed
        loop_values <- res$values
      }
      if (ocp_has_function_call(fpd, act_node$id)) {
        # but if the loop had a function call, we should delete values
        values <- list()
      }
    } else if (is_if(fpd, act_node$id)) {
      childs <- fpd[fpd$parent == act_node$id, ]
      res_fpd <- rbind(res_fpd, act_node)
      res_fpd <- rbind(res_fpd, childs[childs$terminal, ])
      # work on if/else exprs
      exprs_fpd <- childs[!childs$terminal, ]
      for (j in seq_len(nrow(exprs_fpd))) {
        res <- cp_one_fpd(
          get_children(fpd, exprs_fpd[j, "id"]), values,
          in_fun_call
        )
        res_fpd <- rbind(res_fpd, res$fpd)
        # cant keep res$values, because maybe the if condition is FALSE
      }
      # if it is an if, then remove the in-if/else assigned vars from values
      if_ass_vars <- ocp_get_assigned_vars(fpd, act_node$id)
      values <- values[!names(values) %in% if_ass_vars]
      if (ocp_has_function_call(fpd, act_node$id)) {
        # but if the if/else had a function call, we should delete values
        values <- list()
      }
    } else if (is_assignment(fpd, act_node$id)) {
      # note that it is an assignment, but not of constant value
      # and can be x <- y <- z * 7
      res_fpd <- rbind(res_fpd, act_node) # keep root node
      childs <- fpd[fpd$parent == act_node$id, ]
      # get indexes that are not the expr to assing
      save_idxs <- get_assign_indexes(childs$token)
      res_fpd <- rbind(res_fpd, childs[save_idxs, ])
      childs <- childs[-save_idxs, ]
      child_fpd <- get_children(fpd, childs$id)
      if (nrow(childs) == 1 && childs$token == "expr") {
        # it is an expr
        res <- cp_one_fpd(child_fpd, values, in_fun_call)
        res_fpd <- rbind(res_fpd, res$fpd)
        values <- res$values
      } else {
        # it is a SYMBOL flattened by us
        res_fpd <- rbind(
          res_fpd,
          replace_constant_vars(child_fpd, act_node$id, values)
        )
      }
      # remove assigned var from values
      values <- values[names(values) !=
        get_assigned_var(act_fpd, act_node$id)]
    } else if (is_function_def(act_fpd, act_node$id)) {
      # it has a new env, so dont pass constant values
      # note that it is an assignment, but not of constant value
      childs <- fpd[fpd$parent == act_node$id, ]
      # FUNCTION '(' formlist ')' cr expr_or_assign
      # formlist can have exprs
      res_fpd <- rbind(res_fpd, act_node)
      res_fpd <- rbind(res_fpd, childs[childs$terminal, ])
      exprs_fpd <- childs[!childs$terminal, ]
      for (j in seq_len(max(0, nrow(exprs_fpd) - 1))) {
        # dont propagate in function arg expressions
        res_fpd <- rbind(res_fpd, get_children(act_fpd, exprs_fpd[j, "id"]))
      }
      # propagate on the function body ( with new env c() )
      res <- cp_one_fpd(
        get_children(act_fpd, exprs_fpd[nrow(exprs_fpd), "id"]),
        list(), in_fun_call
      )
      res_fpd <- rbind(res_fpd, res$fpd)
    } else {
      # propagate on act_node childs
      # get children w/o parent, and keep propagating
      childs <- fpd[fpd$parent == act_node$id, ]
      res_fpd <- rbind(res_fpd, act_node)
      res_fpd <- rbind(res_fpd, childs[childs$terminal, ])
      res <- cp_one_fpd(
        get_children(fpd, childs[!childs$terminal, "id"]),
        values,
        in_fun_call
      )
      values <- res$values
      res_fpd <- rbind(res_fpd, res$fpd)
    }
  }
  res_fpd <- res_fpd[order(res_fpd$pos_id), ]
  list(fpd = res_fpd, values = values)
}

# Returns a logical indicating if a node is assignment of constant to var.
#
# @param fpd A flatten parsed data data.frame.
# @param id A numeric indicating the node ID.
#
is_constant_var_expr <- function(fpd, id) {
  # is an assignment (might be recursive),
  # and the remaining expr is a constant or -constant
  if (!is_assignment(fpd, id)) {
    return(FALSE)
  }
  act_fpd <- fpd[fpd$parent == id, ]
  if (act_fpd$token[[1]] %in% c("'('", "'{'") &&
    act_fpd$token[[3]] %in% c("')'", "'}'")) {
    return(is_constant_var_expr(fpd, act_fpd[2, "id"]))
  }

  if (act_fpd[2, "token"] == "RIGHT_ASSIGN") {
    is_constant_or_minus(fpd, act_fpd[1, "id"]) ||
      is_constant_var_expr(fpd, act_fpd[1, "id"])
  } else {
    is_constant_or_minus(fpd, act_fpd[3, "id"]) ||
      is_constant_var_expr(fpd, act_fpd[3, "id"])
  }
}

# Returns a logical indicating if a node is a constant or -constant.
#
# @param fpd A flatten parsed data data.frame.
# @param id A numeric indicating the node ID.
#
is_constant_or_minus <- function(fpd, id) {
  act_fpd <- get_children(fpd, id)
  act_tokens <- act_fpd$token
  act_tokens <- setdiff(act_tokens, c(precedence_ops, "expr"))
  (length(act_tokens) == 1 && act_tokens %in% constants) ||
    (length(act_tokens) == 2 && act_tokens[[1]] == "'-'" &&
      act_tokens[[2]] %in% constants)
}

# Returns a named value c(var name=var constant value) if is_constant_var_expr.
#
# @param fpd A flatten parsed data data.frame.
# @param id A numeric indicating the node ID.
#
get_constant_var <- function(fpd, id) {
  if (!is_constant_var_expr(fpd, id)) {
    return()
  }
  act_fpd <- get_children(fpd, id)
  act_var_txts <- act_fpd[act_fpd$token %in% c("SYMBOL", "STR_CONST"), "text"]
  n_assigns <- sum(act_fpd$token %in% assigns)
  act_var <- act_var_txts[seq_len(n_assigns)]
  if ("RIGHT_ASSIGN" %in% act_fpd) {
    act_var <- rev(act_var_txts)[seq_len(n_assigns)]
  }
  act_code <- act_fpd[act_fpd$token %in% c("'-'", constants), ]
  res <- eval(parse(text = paste0(act_code$text)))
  res <- rep(list(res), length(act_var))
  names(res) <- gsub("\"", "", act_var)
  res
}

# Returns a logical indicating if a node is only operators and vars.
#
# @param fpd A flatten parsed data data.frame.
# @param id A numeric indicating the node ID.
#
only_uses_ops <- function(fpd, id) {
  act_fpd <- get_children(fpd, id)
  all(act_fpd$token %in% c("expr", "SYMBOL", constants, ops, precedence_ops))
}

# Returns a new flat parsed data where constant vars being replaced by their
# value.
#
# @param fpd A flatten parsed data data.frame.
# @param id A numeric indicating the node ID.
# @param constant_vars A named list of variables and their value.
#
replace_constant_vars <- function(fpd, id, constant_vars) {
  # only edit SYMBOLS that are in the constant vars
  to_edit <- fpd$token == "SYMBOL" & fpd$text %in% names(constant_vars) &
    !sapply(fpd$parent, function(act_prnt) { # dont replace:
      "'$'" %in% fpd$token[fpd$parent == act_prnt] || # SYMBOL$SYMBOL
        any(assigns %in% fpd$token[fpd$parent == act_prnt]) # SYMBOL <- some_val
    })
  new_fpd <- fpd[!to_edit, ]
  to_edit_fpd <- fpd[to_edit, ]
  for (i in seq_len(nrow(to_edit_fpd))) {
    act_fpd <- to_edit_fpd[i, ]
    act_val <- deparse(constant_vars[[act_fpd$text]])

    new_act_fpd <- flatten_leaves(parse_text(act_val))
    # new ids will be old_id + _ + new_id
    new_act_fpd$id <- paste0(act_fpd$id, "_", new_act_fpd$id)
    # keep old parent for new fpd
    new_act_fpd$parent[new_act_fpd$parent != 0] <-
      paste0(act_fpd$id, "_", new_act_fpd$parent[new_act_fpd$parent != 0])
    new_act_fpd$parent[new_act_fpd$parent == 0] <- act_fpd$parent
    new_act_fpd$pos_id <-
      create_new_pos_id(act_fpd, nrow(new_act_fpd), act_fpd$id)
    new_act_fpd$next_spaces[nrow(new_act_fpd)] <- act_fpd$next_spaces
    new_act_fpd$next_lines[nrow(new_act_fpd)] <- act_fpd$next_lines
    new_act_fpd$prev_spaces[which(new_act_fpd$terminal)[[1]]] <-
      act_fpd$prev_spaces
    new_fpd <- rbind(new_fpd, new_act_fpd)
  }
  new_fpd[order(new_fpd$pos_id), ]
}

# Returns a logical indicating if a node is a function call.
#
# @param fpd A flatten parsed data data.frame.
# @param id A numeric indicating the node ID.
#
is_function_call <- function(fpd, id) {
  act_fpd <- fpd[fpd$parent == id, ]
  "SYMBOL_FUNCTION_CALL" %in% act_fpd$token
}

# Returns a logical indicating if a node has a function call.
#
# @param fpd A flatten parsed data data.frame.
# @param id A numeric indicating the node ID.
#
ocp_has_function_call <- function(fpd, id) {
  act_fpd <- get_children(fpd, id)
  "SYMBOL_FUNCTION_CALL" %in% act_fpd$token
}

# Returns a logical indicating if a node is a loop.
#
# @param fpd A flatten parsed data data.frame.
# @param id A numeric indicating the node ID.
#
is_loop <- function(fpd, id) {
  act_fpd <- fpd[fpd$parent == id, ]
  any(loops %in% act_fpd$token)
}

# Returns a logical indicating if a node is an `if`.
#
# @param fpd A flatten parsed data data.frame.
# @param id A numeric indicating the node ID.
#
is_if <- function(fpd, id) {
  act_fpd <- fpd[fpd$parent == id, ]
  "IF" %in% act_fpd$token
}

# Returns a logical indicating if a node is an assignment.
#
# @param fpd A flatten parsed data data.frame.
# @param id A numeric indicating the node ID.
#
is_assignment <- function(fpd, id) {
  # it has to be one of
  #   {SYMBOL STR_CONST} {LEFT_ASSIGN EQ_ASSIGN}_ASSIGN expr
  #   expr RIGHT_ASSIGN SYMBOL
  act_fpd <- fpd[fpd$parent == id, ]
  child <- get_children(fpd, id)
  return(
    length(unique(child$token[child$token %in% assigns])) == 1 &&
      nrow(act_fpd) == 3 && (
      (act_fpd$token[[1]] %in% c("'('", "'{'") &&
        act_fpd$token[[3]] %in% c("')'", "'}'") &&
        is_assignment(fpd, act_fpd[2, "id"])) ||
        (act_fpd$token[[1]] %in% c("SYMBOL", "STR_CONST") &&
          act_fpd$token[[2]] %in% c("LEFT_ASSIGN", "EQ_ASSIGN")) ||
        (
          act_fpd$token[[3]] %in% c("SYMBOL", "STR_CONST") &&
            act_fpd$token[[2]] == "RIGHT_ASSIGN"
        )
    )
  )
}

# From a vector of tokens returns the index of everything except the value.
#
# @param tokens Vector of tokens
#
get_assign_indexes <- function(tokens) {
  idxs <- which(tokens %in% assigns)
  aux <- rep(-1, length(idxs))
  if (all(tokens[idxs] == "RIGHT_ASSIGN")) {
    aux <- -aux
  }
  idxs <- c(idxs, idxs + aux)
  idxs <- c(idxs, which(tokens %in% precedence_ops))
  idxs
}

# Returns the name of the var that is being assigned.
#
# @param fpd A flatten parsed data data.frame.
# @param id A numeric indicating the node ID.
#
get_assigned_var <- function(fpd, id) {
  if (!is_assignment(fpd, id)) {
    return("")
  }
  act_fpd <- fpd[fpd$parent == id, ]
  res <- act_fpd$text[[1]]
  if (act_fpd$token[[2]] == "RIGHT_ASSIGN") {
    res <- act_fpd$text[[3]]
  }
  res
}

# Returns the names of the vars that are being assigned in an expr.
#
# @param fpd A flatten parsed data data.frame.
# @param id A numeric indicating the node ID.
#
ocp_get_assigned_vars <- function(fpd, id) {
  act_ids <- id
  res <- c()
  while (length(act_ids) > 0) {
    res <- c(res, sapply(act_ids, get_assigned_var, fpd = fpd))
    res <- c(res, sapply(act_ids, get_assigned_var_extra, fpd = fpd))
    act_ids <- fpd[fpd$parent %in% act_ids & !fpd$terminal, "id"]
  }
  unique(res)
}

# Returns the name of the var that is being assigned by `IN`.
#
# @param fpd A flatten parsed data data.frame.
# @param id A numeric indicating the node ID.
#
get_assigned_var_extra <- function(fpd, id) {
  act_fpd <- fpd[fpd$parent == id, ]
  res <- ""
  if ("IN" %in% act_fpd$token) {
    res <- act_fpd[which("IN" == act_fpd$token) - 1, "text"]
  }
  res
}

# Returns a logical indicating if a node is a function definition.
#
# @param fpd A flatten parsed data data.frame.
# @param id A numeric indicating the node ID.
#
is_function_def <- function(fpd, id) {
  act_fpd <- fpd[fpd$parent == id, ]
  "FUNCTION" %in% act_fpd$token
}
