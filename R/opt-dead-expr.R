#' Optimizer: Dead Expression Elimination.
#'
#' Performs one dead expression elimination pass.
#' Carefully examine the results after running this function!
#'
#' @param texts A list of character vectors with the code to optimize.
#'
#' @examples
#' code <- paste(
#'   "foo <- function(x) {",
#'   "  x ^ 3",
#'   "  return(x ^ 3)",
#'   "}",
#'   sep = "\n"
#' )
#' cat(opt_dead_expr(list(code))$codes[[1]])
#' @export
#'
opt_dead_expr <- function(texts) {
  # todo: check if dead expressions have never assigned vars.
  # foo() { x; return(8818) } is not equivalent to foo() { return(8818) }
  # as the first one might have an error.
  res <- list()
  res$codes <- lapply(texts, de_one_file)
  res
}

# Executes dead expression elimination on one file of code
#
# @param text A character vector with code to optimize.
#
de_one_file <- function(text) {
  pd <- parse_text(text)
  res_pd <- pd[pd$parent < 0, ] # keep lines with just comments
  new_pd <- pd[pd$parent >= 0, ] # keep lines with just comments
  new_pd <- de_one_pd(new_pd)
  res_pd <- rbind(res_pd, new_pd)
  if (nrow(res_pd) > 0) {
    res_pd <- res_pd[order(res_pd$pos_id), ]
  }

  deparse_data(res_pd)
}

# Executes dead expression elimination of a pd.
#
# @param pd A parsed data data.frame.
#
de_one_pd <- function(pd) {
  res_pd <- pd
  fpd <- flatten_leaves(pd)
  # exprs in functions dont have any effect. However, on the global env they
  # print on console, so just analyze function definitions
  fun_def_ids <- pd[pd$token == "FUNCTION", "parent"]
  
  #check if function bodies have never assigned vars
  if(length(fun_def_ids) > 0) {
    i <- 1
    for ( i in 1:length(fun_def_ids)) {
      flag <- is_var_assigned(fpd, fun_def_ids[[i]])
      if(flag[1] == TRUE)
        next
      else {
        warning("Please remove the unassigned variable or assign value. It may lead to errors.")
        k <- 3
        for(k in 3:(as.numeric(flag[2])+1)){
          print(flag[k])
        }
      }
    }
  }
  

  # get unassigned expressions
  dead_exprs_ids <- unlist(lapply(fun_def_ids, function(act_id) {
    get_unassigned_exprs(pd, act_id)
  }))

  # remove the ones that are last expression of functions
  return_ids <- unlist(lapply(fun_def_ids, function(act_id) {
    get_fun_last_exprs(pd, act_id)
  }))
  dead_exprs_ids <- dead_exprs_ids[!dead_exprs_ids %in% return_ids]

  pretty_remove_nodes(res_pd, dead_exprs_ids)
}

# Returns the ids of expressions that are not being assigned to a var.
#
# @param pd A parsed data data.frame.
# @param id A numeric indicating the node ID of the function to search for
#   unassigned expressions.
#
get_unassigned_exprs <- function(pd, id) {
  funs_body_ids <- sapply(id, function(act_id) {
    utils::tail(pd$id[pd$parent == act_id & pd$token == "expr"], 1)
  })
  act_pd <- get_children(pd, funs_body_ids)

  # start visiting root nodes
  visit_nodes <- get_roots(act_pd)$id
  exprs_ids <- c()
  while (length(visit_nodes) > 0) {
    new_visit <- c()
    for (act_parent in visit_nodes) {
      act_prnt_pd <- get_children(act_pd, act_parent)
      act_sblngs <- act_prnt_pd[act_prnt_pd$parent == act_parent, ]
      if (act_sblngs$token[[1]] == "'{'") {
        new_visit <- c(new_visit, act_sblngs$id[act_sblngs$token == "expr" | act_sblngs$token == "exprlist"])
      } else if (act_sblngs$token[[1]] == "exprlist") {
        exprlist_ids <- act_prnt_pd[act_prnt_pd$token == "exprlist", "id"]
        for (exprlist_idx in exprlist_ids) {
          separate_exprs <- act_prnt_pd[act_prnt_pd$parent == exprlist_idx & act_prnt_pd$token == "expr", "id"]
          new_visit <- c(new_visit, separate_exprs)
        }
        new_visit <- unique(new_visit)
      } else if (all(act_sblngs$token %in% c("expr", "';'"))) {
        new_visit <- c(new_visit, act_sblngs[act_sblngs$token == "expr", "id"])
      } else if (all(act_prnt_pd$token %in%
        c(constants, ops, precedence_ops, "expr", "exprlist","SYMBOL"))) {
        # it is an expression
        exprs_ids <- c(exprs_ids, act_parent)
      } else if (any(c(loops, "IF") %in% act_sblngs$token)) {
        # remove conditional expr
        body_ids <- act_sblngs$id[!act_sblngs$terminal]
        body_ids <- body_ids[seq(
          any(c("')'", "forcond") %in% act_sblngs$token) + 1, length(body_ids)
        )]

        for (body_id in body_ids) {
          act_terms <- get_children(act_prnt_pd, body_id)
          act_terms <- act_terms$text[act_terms$terminal]
          # dont eliminate in `if (cond) NULL`
          if (!(length(act_terms) == 1 && act_terms == "NULL")) {
            new_visit <- c(new_visit, body_id)
          }
        }
      } else {
        next
      }
    }
    visit_nodes <- new_visit
  }

  # remove assigned exprs and others
  exprs_ids[!sapply(exprs_ids, function(act_id) {
    act_sblngs <- act_pd[act_pd$parent ==
      act_pd$parent[act_pd$id == act_id], ]
    any(assigns %in% act_sblngs$token) # the expr is being assigned
  })]
  
  unique(exprs_ids)
}

# Returns the IDs of the exprs that can return in a function.
#
# @param pd A parsed data data.frame.
# @param id A numeric indicating the fun node ID to check.
#
get_fun_last_exprs <- function(pd, id) {
  fun_body <- pd$id[pd$parent == id & pd$token == "expr"]
  act_pd <- get_children(pd, fun_body)

  # start visiting root nodes
  visit_nodes <- get_roots(act_pd)$id
  last_exprs_ids <- c()
  while (length(visit_nodes) > 0) {
    new_visit <- c()
    for (act_parent in visit_nodes) {
      act_prnt_pd <- get_children(act_pd, act_parent)
      act_sblngs <- act_prnt_pd[act_prnt_pd$parent == act_parent, ]
      if (act_sblngs$token[[1]] == "'{'") {
        # has multiple exprs, check only the last one
        if (act_sblngs[(nrow(act_sblngs)-1), "token"] == "exprlist") {
          exprlist_id <- act_sblngs[(nrow(act_sblngs)-1), "id"]
          exprlist_expr_ids <- act_prnt_pd[act_prnt_pd$parent == exprlist_id & act_prnt_pd$token == "expr", "id"]
          new_visit <- c(new_visit, utils::tail(exprlist_expr_ids, n = 1))
        } else {
          new_visit <- c(
            new_visit,
            utils::tail(act_sblngs$id[act_sblngs$token == "expr"], 1)
          )
        }
      } else if (any(loops %in% act_sblngs$token)) {
        next
      } else if ("IF" %in% act_sblngs$token) {
        # visit if body and else body
        if_else_body_ids <- act_sblngs$id[act_sblngs$token == "expr"][-1]
        new_visit <- c(new_visit, if_else_body_ids)
      } else {
        last_exprs_ids <- c(last_exprs_ids, act_parent)
      }
    }
    visit_nodes <- new_visit
  }

  # returns last exprs and their children ids
  get_children(pd, last_exprs_ids)$id
}

# Checks whether some value is being assigned to all var.
#
# @param fpd A parsed data data.frame.
# @param id A numeric indicating the node ID of the function to search for
#   unassigned expressions.
#
is_var_assigned <- function(fpd, id)  {
  fun_ids <- sapply(id, function(act_id){
    utils::tail(fpd$id[fpd$parent == act_id & fpd$token == "expr"], 1)
  })
  
  act_fpd <- get_children(fpd, fun_ids)
  
  
  checklist_expr <- NULL
  checklist <- NULL
  checklist_var <- act_fpd[act_fpd$token == "SYMBOL" & act_fpd$next_lines == 1 & 
                             act_fpd$parent == fun_ids, ]
  expr_ids <- act_fpd[act_fpd$parent == fun_ids, ]$id
  j <- 1
  assignment_exprs <- c("LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_ASSIGN")
  sys_call <- c("SYMBOL_FUNCTION_CALL")
  for(j in seq_len(length(expr_ids))){
    test_id <- expr_ids[j]
    if (length(act_fpd[act_fpd$parent == test_id, ]$id) > 0) {
      if(!any(act_fpd[act_fpd$parent == test_id, ]$token %in% assignment_exprs) & 
         !any(act_fpd[act_fpd$parent == test_id, ]$token %in% sys_call)){
        checklist_expr <- rbind(checklist_expr, act_fpd[act_fpd$id == test_id, ])      
      }
    }
  }
  checklist <- rbind(checklist_expr, checklist_var)
  check_flag <- NULL
  if (length(checklist$id) > 0) {
    itr <- NULL
    check_flag <- FALSE
    for (itr in 1:length(checklist$id)) {
      check_flag <- append(check_flag, sprintf("Function: %s Variable: %s", (fpd[fpd$parent == fpd[fpd$id == act_fpd[1, ]$parent, ]$parent & fpd$token == "SYMBOL", ]$text), (checklist[itr, ]$text)))
    }
    check_flag <- append(check_flag, length(check_flag), 1)
  } 
  else
    check_flag <- TRUE
  check_flag
}

