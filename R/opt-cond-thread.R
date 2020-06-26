#' Optimizer: Conditional Threading.
#' 
#' Performs one conditional threading pass.
#' Carefully examine the results after running this function!
#'  
#' @param code A list of character vectors with the code to optimize.
#'  
#' @examples 
#' code <- paste(
#' "num <- sample(1:100, 1)",
#' "even_sum <- 0",
#' "odd_sum_a <- 0",
#' "odd_sum_b <- 0",
#' "if(num %% 2) {",
#' "odd_sum_a <- odd_sum_a + num",
#' "}",
#' "if(num %% 2) {",
#' "odd_num_b <- odd_num_b + num",
#' "}",
#' "if(!(num %% 2)) {",
#' "even_sum <- even_sum + num",
#' "}",
#' sep = "\n"
#' )
#' 
#' cat(opt_cond_thread(list(code))$codes[[1]])
#' @export  

opt_cond_thread <- function(code) {
  res <- list()
  res$codes <- lapply(code, cd_one_file)
  return (res)
}

# Executes conditional threading on one text of code.
#
# @param code A character vector with code to optimize.
#
cd_one_file <- function(code)
{
  parsed_dataset <- parse_text(code)
  flatten_pd <- flatten_leaves(parsed_dataset)
  result_flatten_pd <- cd_one_flatten_pd(parsed_dataset, flatten_pd)
  return (deparse_data(result_flatten_pd))
}

# Checks for IF statement in the given node_id of the given fpd.
#
# @param fpd A flatten parsed data data.frame.
# @param node_id A numeric indicating the node ID of the function def expression.
#
check_if <- function(fpd, node_id) {
  return ("IF" %in% fpd[fpd$parent == node_id, "token"])
}

# Checks whether the next line of code also contains an IF statement for a given node_id of the given fpd.
#
# @param fpd A flatten parsed data data.frame.
# @param node_id A numeric indicating the node ID of the function def expression.
#
check_if_next <- function(fpd, node_id, exam_nodes) {
  if(check_if(fpd, node_id)) {
    row_num <- which(exam_nodes$id == node_id, arr.ind = TRUE)
    curr_parents <- get_children(fpd, node_id)$id
    if(row_num != nrow(exam_nodes)) {
      for(i in exam_nodes[-seq_len(row_num), "id"]) {
        if(exam_nodes[exam_nodes$id == i, "parent"] >= 0 & !(i %in% curr_parents)) {
          return (check_if(fpd, i))
          break
        }
      }
      return (FALSE)
    }
    else {
      return (FALSE)
    }
  } else {
    return (FALSE)
  }
}

# Used to find the condition of the IF block in the given node_id of the given fpd.
#
# @param fpd A flatten parsed data data.frame.
# @param node_id A numeric indicating the node ID of the function def expression.
#
first_if_expr <- function(fpd, node_id) {
  if_pos <- get_children(fpd, node_id)[get_children(fpd, node_id)$token == "IF", "pos_id"]
  start_bracket <- get_children(fpd, node_id)[get_children(fpd, node_id)$pos_id > if_pos & 
                                                get_children(fpd, node_id)$token == "'('", "pos_id"][1]
  start_bracket_parent <- get_children(fpd, node_id)[get_children(fpd, node_id)$pos_id > if_pos &
                                                       get_children(fpd, node_id)$token == "'('", "parent"][1]
  end_bracket <- get_children(fpd, node_id)[get_children(fpd, node_id)$pos_id > if_pos &
                                              get_children(fpd, node_id)$token == "')'" &
                                              get_children(fpd, node_id)$parent == start_bracket_parent, "pos_id"]
  if_expr_id <- get_children(fpd, node_id)[get_children(fpd, node_id)$pos_id > start_bracket &
                                             get_children(fpd, node_id)$pos_id < end_bracket & 
                                             get_children(fpd, node_id)$token == "expr", "id"][1]
  return (if_expr_id)
}

# Used to find the condition of the consecutive IF block of the given node_id in the given fpd.
#
# @param fpd A flatten parsed data data.frame.
# @param node_id A numeric indicating the node ID of the function def expression.
#
consecutive_if_expr <- function(fpd, node_id, exam_nodes) {
  flag <- FALSE
  row_num <- which(exam_nodes$id == node_id, arr.ind = TRUE)
  curr_parents <- get_children(fpd, node_id)$id
  for(i in exam_nodes[-seq_len(row_num), "id"]) {
    if(exam_nodes[exam_nodes$id == i, "parent"] >= 0 & !(i %in% curr_parents)) {
      start_id <- i
      flag <- TRUE
      break
    }
  }
  if(!flag)
    return (flag)
  if("IF" %in% get_children(fpd, start_id)$token) {
    if_pos <- get_children(fpd, start_id)[get_children(fpd, start_id)$token == "IF", "pos_id"]
    start_bracket <- get_children(fpd, start_id)[get_children(fpd, start_id)$pos_id > if_pos & 
                                                   get_children(fpd, start_id)$token == "'('", "pos_id"][1]
    start_bracket_parent <- get_children(fpd, start_id)[get_children(fpd, start_id)$pos_id > if_pos & 
                                                          get_children(fpd, start_id)$token == "'('", "parent"][1]
    end_bracket <- get_children(fpd, start_id)[get_children(fpd, start_id)$pos_id > start_bracket & 
                                                 get_children(fpd, start_id)$token == "')'"& 
                                                 get_children(fpd, start_id)$parent == start_bracket_parent, "pos_id"]
    if_expr_id <- get_children(fpd, start_id)[get_children(fpd, start_id)$pos_id > start_bracket &
                                                get_children(fpd, start_id)$pos_id < end_bracket & 
                                                get_children(fpd, start_id)$token == "expr", "id"][1]
    return (if_expr_id)
  } else {
    return (FALSE)
  }
}

# Checks for exact negations in the conditions of the consecutive IF statements for the given node_id in the given fpd. 
#
# @param fpd A flatten parsed data data.frame.
# @param node1 A numeric indicating the node ID of the first IF expression.
# @param node2 A numeric indicating the node ID of the second IF expression.
#
check_negation <- function(fpd, node1, node2) {
  first_expr_a <- fpd[fpd$id == node1, "text"]
  second_expr_a <- fpd[fpd$id == node2, "text"]
  
  first_expr_b <- fpd[fpd$id == node2, "text"]
  second_expr_b <- fpd[fpd$id == node1, "text"]
  
  check_first1a <- gsub(" ", "", paste("!", "(", first_expr_a, ")", sep = ""), fixed = TRUE) 
  check_first2a <- gsub(" ", "", paste("!", first_expr_a,sep = ""), fixed = TRUE)
  check_second_a <- gsub(" ", "", second_expr_a, fixed = TRUE)
  
  check_first1b <- gsub(" ", "", paste("!", "(", first_expr_b, ")", sep = ""), fixed = TRUE) 
  check_first2b <- gsub(" ", "", paste("!", first_expr_b,sep = ""), fixed = TRUE)
  check_second_b <- gsub(" ", "", second_expr_b, fixed = TRUE)
  
  
  return (check_first1a == check_second_a | check_first2a == check_second_a | check_first1b == check_second_b | check_first2b == check_second_b)
}

# Checks for negations in the equality conditions of the consecutive IF statements for the given node_id in the given fpd. 
#
# @param fpd A flatten parsed data data.frame.
# @param node1 A numeric indicating the node ID of the first IF expression.
# @param node2 A numeric indicating the node ID of the second IF expression.
#
check_not_equal <- function(fpd, node1, node2) {
  first_expr <- get_children(fpd, node1, include_father = FALSE)
  second_expr <- get_children(fpd, node2, include_father = FALSE)
  if(nrow(first_expr) != nrow(second_expr)) {
    return (FALSE)
  }
  for(i in nrow(first_expr)) {
    if(first_expr[i, "text"] != second_expr[i, "text"]) {
      if(!((first_expr[i, "token"] == "EQ" & second_expr[i, "token"] == "NE") | (first_expr[i, "token"] == "NE" & second_expr[i, "token"] == "EQ"))) {
        return (FALSE)
      }
    }
  }
  return (TRUE)
}

# Checks for exact duplicates conditions of the consecutive IF statements for the given node_id in the given fpd. 
#
# @param fpd A flatten parsed data data.frame.
# @param node1 A numeric indicating the node ID of the first IF expression.
# @param node2 A numeric indicating the node ID of the second IF expression.
#
check_duplicate_expr <- function(fpd, node1, node2) {
  first_expr <- fpd[fpd$id == node1, "text"]
  second_expr <- fpd[fpd$id == node2, "text"]
  
  first_expr <- gsub(" ", "", first_expr, fixed = TRUE)
  second_expr <- gsub(" ", "", second_expr, fixed = TRUE)
  
  return (first_expr == second_expr)
}

# Checks for exact negations in the greater than equal to conditions of the 
# consecutive IF statements for the given node_id in the given fpd. 
#
# @param fpd A flatten parsed data data.frame.
# @param node1 A numeric indicating the node ID of the first IF expression.
# @param node2 A numeric indicating the node ID of the second IF expression.
#
check_comparsion_logic_ge <- function(fpd, node1, node2) {
  first_expr <- fpd[fpd$id == node1, "text"]
  second_expr <- fpd[fpd$id == node2, "text"]
  if(length(grep(">=", first_expr)) > 0) {
    first_expr <- gsub(" ", "", first_expr, fixed = T)
    first_expr <- gsub(">=", "<", first_expr)
    second_expr <- gsub(" ", "", second_expr, fixed = TRUE)
    if(first_expr == second_expr) {
      return (check_consecutive_if(fpd, node_id))
    } else {
      return (FALSE)
    }
  } 
  else if(length(grep(">=", second_expr)) > 0) {
    second_expr <- gsub(" ", "", second_expr, fixed = TRUE)
    first_expr <- gsub(" ", "", first_expr, fixed = TRUE)
    second_expr <- gsub(">=", "<", second_expr)
    if(first_expr == second_expr) {
      return (first_if_expr(fpd, node_id))
    } else{
      return (FALSE)
    }
  }
  else{
    return (FALSE)
  }
}

# Checks for exact negations in the lesser than equal to conditions of the 
# consecutive IF statements for the given node_id in the given fpd. 
#
# @param fpd A flatten parsed data data.frame.
# @param node1 A numeric indicating the node ID of the first IF expression.
# @param node2 A numeric indicating the node ID of the second IF expression.
#
check_comparsion_logic_le <- function(fpd, node1, node2) {
  first_expr <- fpd[fpd$id == node1, "text"]
  second_expr <- fpd[fpd$id == node2, "text"]
  if(length(grep("<=", first_expr)) > 0) {
    first_expr <- gsub(" ", "", first_expr, fixed = TRUE)
    first_expr <- gsub("<=", ">", first_expr)
    second_expr <- gsub(" ", "", second_expr, fixed = TRUE)
    return (first_expr == second_expr)
  } 
  else if(length(grep("<=", second_expr)) > 0) {
    first_expr <- gsub(" ", "", first_expr, fixed = TRUE)
    second_expr <- gsub(" ", "", second_expr, fixed = TRUE)
    second_expr <- gsub("<=", ">", second_expr)
    return (first_expr == second_expr)
  }
  else{
    return (FALSE)
  }
}

# Checks for function calls in the IF statement conditions
#
# @param fpd A flatten parsed data.frame.
# @param node1 A numeric indicating the node ID of the first IF expression.
# @param node2 A numeric indicating the node ID of the second IF expression.
#
has_func_calls <- function(fpd, node1, node2) {
  return ("SYMBOL_FUNCTION_CALL" %in% get_children(fpd, node1)$token | 
            "SYMBOL_FUNCTION_CALL" %in% get_children(fpd, node2))  
}

# Retrieves the IF block expression from the position of the node in exam_nodes
#
# @param fpd A flatten parsed data.frame
# @param exam_nodes A list of all nodes that need to be examined for the optimization
# @param itr The position of the iterator in exam_nodes
get_if_block_expr <- function(fpd, exam_nodes, itr) {
  expr_id <- exam_nodes[itr, "id"]
  if_pos <- get_children(fpd, expr_id)[get_children(fpd, expr_id)$token == "IF", "pos_id"]
  start_bracket <- get_children(fpd, expr_id)[get_children(fpd, expr_id)$pos_id > if_pos & 
                                                get_children(fpd, expr_id)$token == "'('", "pos_id"][1]
  start_bracket_parent <- get_children(fpd, expr_id)[get_children(fpd, expr_id)$pos_id > if_pos & 
                                                       get_children(fpd, expr_id)$token == "'('", "parent"][1]
  end_bracket <- get_children(fpd, expr_id)[get_children(fpd, expr_id)$pos_id > start_bracket & 
                                              get_children(fpd, expr_id)$token == "')'"& 
                                              get_children(fpd, expr_id)$parent == start_bracket_parent, "pos_id"]
  return (fpd[fpd$pos_id == (end_bracket+1) & fpd$token == "expr", "text"])
}

# Retrieves the modified IF block expression from the position of the node in modified exam_nodes
#
# @param fpd A flatten parsed data.frame
# @param exam_nodes A list of all nodes that need to be examined for the optimization
# @param index The position of the iterator in exam_nodes
get_modified_if_expr <- function(fpd, exam_nodes, index) {
  modified_fpd <- flatten_leaves(parse_text(exam_nodes[index, "text"]))
  if_pos <- modified_fpd[modified_fpd$token == "IF", "pos_id"]
  start_bracket <- modified_fpd[modified_fpd$pos_id > if_pos &
                                  modified_fpd$token == "'('", "pos_id"][1]
  start_bracket_parent <- modified_fpd[modified_fpd$pos_id > if_pos &
                                         modified_fpd$token == "'('", "parent"][1]
  end_bracket <- modified_fpd[modified_fpd$pos_id > start_bracket &
                                modified_fpd$token == "')'" &
                                modified_fpd$parent == start_bracket_parent, "pos_id"]
  return (modified_fpd[modified_fpd$pos_id == (end_bracket+1) & modified_fpd$token == "expr", "text"])
}

# Determines the nodes that have to be removed based on the iterator (itr) of the exam_nodes
#
# @param fpd A flatten parsed data.frame
# @param exam_nodes A list of all nodes that need to be examined for the optimization
# @param itr The position of the iterator in exam_nodes
node_removal_fun <- function(itr, exam_nodes, fpd) {
  return_id <- NULL
  res_list <- NULL
  curr_id <- exam_nodes[itr, "id"]
  curr_parents <- get_children(fpd, curr_id)$id
  for(i in exam_nodes[-seq_len(itr), "id"]) {
    if(exam_nodes[exam_nodes$id == i, "parent"] >= 0 & !(i %in% curr_parents))
    {
      return_id <- i
      break
    }
  }
  curr_parents2 <- get_children(fpd, return_id)$id
  for(i in exam_nodes$id) {
    if(i %in% curr_parents2) {
      res_list <- append(res_list, which(i == exam_nodes$id, arr.ind = TRUE))
    }
  }
  rest_list <- NULL
  itr_parents <- get_children(fpd, exam_nodes[itr, "id"])$id
  for(i in exam_nodes$id) {
    if(i %in% itr_parents & exam_nodes[exam_nodes$id == i, "parent"] >= 0) {
      rest_list <- append(rest_list, which(i == exam_nodes$id, arr.ind = TRUE))
    }
  }
  rest_list <- rest_list[-1]
  return (append(res_list, rest_list))
}


# Executes conditional threading of a flatten_pd.
#
# @param flatten_pd A flatten parsed data data.frame.
#
cd_one_flatten_pd <- function(parsed_dataset, flatten_pd)
{
  pd <- parsed_dataset
  fpd <- flatten_pd
  # Procedure to include nodes from Functions into exam nodes
  fun_ids <- fpd$id[fpd$token == "FUNCTION"]      
  fun_prnt_ids <- fpd$parent[fpd$id %in% fun_ids] 
  
  fun_exam_nodes <- NULL
  for(i in fun_prnt_ids){
    fun_exam_nodes <- rbind(fun_exam_nodes, get_children(fpd, i, FALSE)[get_children(fpd, i, FALSE)$token == "expr", ])
  }
  
  # Procedure to include nodes from Loops into exam nodes
  loop_ids <- fpd[fpd$token %in% loops, "id"]
  loop_parent_ids <- fpd[fpd$id %in% loop_ids, "parent"]
  loop_exam_nodes <- NULL
  for(i in loop_parent_ids) {
    loop_exam_nodes <- rbind(loop_exam_nodes, get_children(fpd, i, FALSE)[get_children(fpd, i, FALSE)$token == "expr", ])
  }
  
  exam_nodes <- get_roots(pd)
  exam_nodes <- rbind(exam_nodes, fun_exam_nodes)
  exam_nodes <- rbind(exam_nodes, fun_exam_nodes, loop_exam_nodes)
  
  # Procedure to include nodes from nested IF loops
  stray_nodes <- fpd[fpd$token == "IF","parent"]
  stray_exam_nodes <- NULL
  for(i in stray_nodes) {
    stray_exam_nodes <- rbind(stray_exam_nodes, fpd[fpd$id == i, ])
  }
  for(i in stray_exam_nodes$id) {
    if(!(i %in% exam_nodes$id)) {
      exam_nodes <- rbind(exam_nodes, fpd[fpd$id == i, ])
    }
  }
  
  #Removing duplicate nodes from exam_nodes 
  e_nodes <- NULL
  for(i in unique(exam_nodes$id)) {
    e_nodes <- rbind(e_nodes, unique(exam_nodes[exam_nodes$id == i, ]))
  }
  
  exam_nodes <- e_nodes
  exam_nodes_copy <- exam_nodes #This seems stupid, but will be pretty useful later on
  
  #Examination of the exam_nodes begin
  #First we will handle all the statements that have to be merged.
  to_change_node <- NULL
  to_remove_node <- NULL
  merge_to <- NULL
  merge_from <- NULL
  
  for(itr in seq_len(length(exam_nodes$id))) {
    i <- exam_nodes[itr, "id"]
    if(check_if(fpd, i)) {
      if(check_if_next(fpd, i, exam_nodes)) {
        node1 <- first_if_expr(fpd, i)
        node2 <- consecutive_if_expr(fpd, i, exam_nodes)
        if(!has_func_calls(fpd, node1, node2)) {
          if(check_duplicate_expr(fpd, node1, node2)) {
            merge_to <- append(merge_to, itr)
            merge_from <- append(merge_from, node_removal_fun(itr, exam_nodes = exam_nodes, fpd = fpd)[1])
            to_change_node <- append(to_change_node, itr)
            to_remove_node <- append(to_remove_node, node_removal_fun(itr, exam_nodes = exam_nodes, fpd = fpd))
  } } } } }
  
  deletion_nodes <- vector(mode = "numeric")
  
  for(i in to_remove_node) {
    deletion_nodes <- append(deletion_nodes, exam_nodes[i, "id"])
  }
  
  for(i in to_change_node) {
    deletion_nodes <- append(deletion_nodes, exam_nodes[i, "id"])
  }
  
  # The following sequence is used to change the text of exam_nodes text where merging has to take place
  to_expr <- NULL
  from_expr <- NULL
  
  for(i in seq_len(length(merge_from))) {
    to_expr[i] <- get_if_block_expr(fpd, exam_nodes, merge_to[i])
    if(grepl("{\n", to_expr[i], fixed = TRUE)) {
      to_expr[i] <- gsub("{\n", "", to_expr[i], fixed = TRUE)
      to_expr[i] <- gsub("\n}", "", to_expr[i], fixed = TRUE)
    } 
    else if(grepl("{", to_expr[i], fixed = TRUE)) {
      to_expr[i] <- gsub("{", "", to_expr[i], fixed = TRUE)
      to_expr[i] <- gsub("}", "", to_expr[i], fixed = TRUE)
    }
    
  from_expr[i] <- get_if_block_expr(fpd, exam_nodes, merge_from[i])
    if(grepl("{\n", from_expr[i], fixed = TRUE)) {
      from_expr[i] <- gsub("{\n", "", from_expr[i], fixed = TRUE)
      from_expr[i] <- gsub("\n}", "", from_expr[i], fixed = TRUE)
    } 
    else if(grepl("{", from_expr[i], fixed = TRUE)) {
      from_expr[i] <- gsub("{", "", from_expr[i], fixed = TRUE)
      from_expr[i] <- gsub("}", "", from_expr[i], fixed = TRUE)
    }
  }
  
  for(i in seq_len(length(to_change_node))) {
    if_cond <- fpd[fpd$id == first_if_expr(fpd, exam_nodes[to_change_node[i], "id"]), "text"]
    string1 <- paste("if", "(", if_cond, ") ", "{\n", sep = "")
    string2 <- paste(to_expr[i], "\n", from_expr[i], "\n}", sep = " ")
    exam_nodes[to_change_node[i], "text"] <- paste0(string1, string2)
  }
  
  if(length(to_remove_node) > 0) {
    exam_nodes <- exam_nodes[-(to_remove_node), ]
  }
    
  #Here conversion of marked IF statements to ELSE takes place
  to_change_node <- NULL
  to_remove_node <- NULL
  convert_to_else <- NULL
  
  for(itr in seq_len(length(exam_nodes$id))) {
    i <- exam_nodes[itr, "id"]
    if(check_if(fpd, i)) {
      if(check_if_next(fpd, i, exam_nodes)) {
        node1 <- first_if_expr(fpd, i)
        node2 <- consecutive_if_expr(fpd, i, exam_nodes)
        if(!has_func_calls(fpd, node1, node2)) {
          if(check_negation(fpd, node1, node2) |
             check_not_equal(fpd, node1, node2) |
             check_comparsion_logic_ge(fpd, node1, node2) |
             check_comparsion_logic_le(fpd, node1, node2)) {
            convert_to_else <- append(convert_to_else, node_removal_fun(itr, exam_nodes = exam_nodes, fpd = fpd)[1])
            to_change_node <- append(to_change_node, itr)
            to_remove_node <- append(to_remove_node, node_removal_fun(itr, exam_nodes = exam_nodes, fpd = fpd))
          } } } } }
  
  for(i in to_remove_node) {
    deletion_nodes <- append(deletion_nodes, exam_nodes[i, "id"])
  }
  
  for(i in to_change_node) {
    deletion_nodes <- append(deletion_nodes, exam_nodes[i, "id"])
  }
  
  else_expr <- NULL
  for(i in seq_len(length(convert_to_else))) {
    else_expr[i] <- get_modified_if_expr(fpd, exam_nodes, convert_to_else[i])
    if(grepl("{\n", else_expr[i], fixed = TRUE)) {
      else_expr[i] <- gsub("{\n", "", else_expr[i], fixed = TRUE)
      else_expr[i] <- gsub("\n}", "", else_expr[i], fixed = TRUE)
    } 
    else if(grepl("{", else_expr[i], fixed = TRUE)) {
      else_expr[i] <- gsub("{", "", else_expr[i], fixed = TRUE)
      else_expr[i] <- gsub("}", "", else_expr[i], fixed = TRUE)
    }
  }
  
  for(i in seq_len(length(to_change_node))) {
    string1 <- exam_nodes[to_change_node[i], "text"]
    exam_nodes[to_change_node[i], "text"] <- paste0(string1, paste("else", "{\n", else_expr[i], " \n}", sep = " "))
  }
  
  
  if(length(to_remove_node) > 0) {
    exam_nodes <- exam_nodes[-(to_remove_node), ]
  }
  
  exam_nodes <- exam_nodes[order(exam_nodes$pos_id), ]
  
  #Segregating the nodes that are not to be edited
  not_to_edit <- c()
  
  for(i in exam_nodes_copy$id) {
    if(!(i %in% deletion_nodes)) {
      not_to_edit <- rbind(not_to_edit, exam_nodes_copy[exam_nodes_copy$id == i, ])
    }
  }
  
  #Tidying up the not_to_edit list
  for(i in seq_len(nrow(not_to_edit))) {
    not_to_edit <- rbind(not_to_edit, get_children(pd, not_to_edit[i, "id"]))
  }
  
  not_to_edit <- not_to_edit[order(not_to_edit$pos_id), ]
  
  #Removing entries with duplicate IDs.
  not_to_edit_final <- NULL
  for(i in unique(not_to_edit$id)) {
    not_to_edit_final <- rbind(not_to_edit_final, unique(not_to_edit[not_to_edit$id == i, ]))
  }
  
  #Creating a list that consists of all the nodes to be changed
  final_exam_nodes <- c()
  for(i in to_change_node) {
    final_node_id <- exam_nodes_copy[i, "id"]
    final_exam_nodes <- rbind(final_exam_nodes, exam_nodes[exam_nodes$id == final_node_id, ])
  }
  
  # Creating new properties for the new_fpd
  new_fpd <- NULL
  if(length(final_exam_nodes$id) > 0)
    for(itr in seq_len(nrow(final_exam_nodes)))
    {
      act_fpd <- final_exam_nodes[itr, ]
      new_act_fpd <- parse_text(act_fpd$text)
      
      #Setting new ids for the newly edited and parsed codes
      new_act_fpd$id <- paste0(act_fpd$id, "_", new_act_fpd$id)
      
      #Keeping old parents for new fpd
      new_act_fpd$parent[new_act_fpd$parent != 0] <- paste0(act_fpd$id, "_", new_act_fpd$parent[new_act_fpd$parent != 0])
      new_act_fpd$parent[new_act_fpd$parent == 0] <- act_fpd$parent
      
      #Calling a pre-wriiten rco::function....
      new_act_fpd$pos_id <- create_new_pos_id(act_fpd, nrow(new_act_fpd), act_fpd$id)
      
      #Fixing the next_spaces section of new_fpd
      new_act_fpd$next_spaces[nrow(new_act_fpd)] <- act_fpd$next_spaces
      
      #Fixing the next_lines section of new_fpd
      new_act_fpd$next_lines[nrow(new_act_fpd)] <- act_fpd$next_lines
      
      #Fixing the prev_spaces section of new_fpd
      new_act_fpd$prev_spaces[which(new_act_fpd$terminal)[[1]]] <- act_fpd$prev_spaces
      
      #Merging the new_fpd and the act_fpd(obtained upon iteration)
      new_fpd <- rbind(new_fpd, new_act_fpd)
      
      #Ordering the new_fpd according to the pos_id
      new_fpd <- new_fpd[order(new_fpd$pos_id), ]
    }
  
  resultant_fpd <- rbind(not_to_edit_final, new_fpd)
  resultant_fpd <- resultant_fpd[order(resultant_fpd$pos_id), ]
  
  test_fpd <- flatten_leaves(resultant_fpd)
  
  # Removing the nodes that were in deletion_nodes
  inspection_ids <- c()
  remove_indices <- c()
  for(i in deletion_nodes) {
    inspection_ids <- get_children(fpd, i)$id
    for(j in inspection_ids) {
      if(length(which(test_fpd$id == j, arr.ind = T)) == 1)
        remove_indices <- append(remove_indices, which(test_fpd$id == j, arr.ind = T))
    }
  }
  
  if(length(remove_indices) > 0){
    test_fpd <- test_fpd[-remove_indices, ]
  }
  
  # Inserting appropriate values in the new_fpd
  comments_ids <- NULL
  curr_new_line_ids <- NULL
  next_line_ids <- NULL
  correction_nodes <- NULL
  correction_ids <- NULL
  
  correction_nodes <- test_fpd[test_fpd$parent == 0 & is.na(test_fpd$next_spaces) & is.na(test_fpd$next_lines) & is.na(test_fpd$prev_spaces), ]
  
  for(i in correction_nodes$id){
    correction_ids <- append(correction_ids, which(test_fpd$id == i, arr.ind = TRUE))
  }
  
  test_fpd[which(is.na(test_fpd$next_lines)), "next_lines"] <- 0
  test_fpd[which(is.na(test_fpd$next_spaces)), "next_spaces"] <- 0
  test_fpd[which(is.na(test_fpd$prev_spaces)), "prev_spaces"] <- 0
  
  comments_ids <- which(test_fpd$parent < 0)
  correction_ids <- append(correction_ids, comments_ids)
  
  for(i in correction_ids){
    if((i-1) > 0){
      test_fpd[i-1, "next_lines"] <- 1
    }else{
      next
    }
  }
  
  test_fpd <- test_fpd[!(test_fpd$parent == 0 & test_fpd$terminal == FALSE), ]
  
  return (test_fpd)
}
