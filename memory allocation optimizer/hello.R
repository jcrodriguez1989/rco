text1 <- paste(
  "v <- w <- x <- NULL",
  "for(i in 1:10) {",
  "  v[i] <- i*i",
  "}",
  "fun <- function(n) {",
  "  num <- NULL",
  "  for(i in c(2010,2011)) {",
  "    num[i] <- i",
  "  }",
  "  sum(num)",
  "}",
  sep = "\n"
)

text2 <- paste(
  "x <- NULL",
  "v <- NULL",
  "z <- NULL",
  "for(i in seq_len(5)) {",
  "  v[i] = i*i",
  "  i -> x[i]",
  "  v[i+5] <- 2*i",
  "  z[i] <- v[i]",
  "}",
  sep = "\n"
)

text3 <- paste(
"break_it <- function() {",
"  i <<- 1",
"}",
"x <- c()",
"for(i in 1:10) {",
"  break_it()",
"  x[i] <- i^2",
"  print(i)",
"}",
sep = "\n"
)

text4 <- paste(
  "v <- NULL",
  "if(5 == 5) {",
  "  if(3 == 3) {",
  "    equal <- NULL",
  "    for(i in 1:10) {",
  "      equal[i] <- i",
  "    }",
  "  }",
  "}",
  "really <- function() {",
  "  v <- c()",
  "  for(i in 1:10) {",
  "    v[i] <- i",
  "  }",
  "}",
  "really()",
  "for(i in 1:10) {",
  "  v <- NULL",
  "  for(j in 1:5) {",
  "    v[j] <- i",
  "  }",
  "}",
  sep = "\n"
)

pd <- parse_text(text1)
fpd <- flatten_leaves(pd)

fpd <- eq_assign_to_expr(fpd)
fpd

exam_nodes <- fpd[fpd$token == "expr", ]

## custom_exam_nodes
null_parents <- fpd[fpd$token == "NULL_CONST", "parent"]
null_parents_alt <- fpd[fpd$token == "expr" & fpd$text == "c()", "parent"]
null_parents <- c(null_parents, null_parents_alt)
custom_exam_nodes <- NULL
for(i in null_parents) {
  custom_exam_nodes <- rbind(custom_exam_nodes, fpd[fpd$id == i, ])
}
for(i in exam_nodes$id) {
  if(nrow(get_children(fpd, i, FALSE)) > 0 & 
     get_children(fpd, i, FALSE)[1, "token"] == "FOR") {
    custom_exam_nodes <- rbind(custom_exam_nodes, fpd[fpd$id == i, ])
  }
}
custom_exam_nodes <- custom_exam_nodes[order(custom_exam_nodes$pos_id), ]


restrict_body_to_one_loop <- function(body_fpd) {
  list_of_fors <- body_fpd[body_fpd$token == "FOR", "pos_id"]
  if(length(list_of_fors) > 1) {
    body_fpd <- body_fpd[body_fpd$pos_id < list_of_fors[[2]], ]
  }
  body_fpd
}


restrict_condition_to_one_loop <- function(cond_fpd) {
  list_of_conds <- cond_fpd[cond_fpd$token == "forcond", "pos_id"]
  if(length(list_of_conds) > 1) {
    cond_fpd <- cond_fpd[cond_fpd$pos_id < list_of_conds[[2]], ]
  }
  cond_fpd
}


extract_vector_name <- function(node_id) {
  assignment_pd <- get_children(fpd, node_id)
  assignment_index <- which(get_children(fpd, node_id)$token %in% assigns)
  assignment_type <- assignment_pd[assignment_index, "token"]
  if(assignment_type == "EQ_ASSIGN" || assignment_type == "LEFT_ASSIGN") {
    vector_name <- assignment_pd[(assignment_index - 1), "text"]
  } else {
    vector_name <- assignment_pd[(assignment_index + 1), "text"]
  }
  vector_name
}


check_forcond_num_declaration <- function(cond_fpd, index_name, in_pos) {
  forcond_number_flag <- FALSE
  if("':'" %in% cond_fpd[cond_fpd$pos_id > in_pos, "token"]) {
    colon_pos <- which(cond_fpd$token == "':'", arr.ind = TRUE)
    colon_pos_id <- cond_fpd[colon_pos, "pos_id"] 
    if("NUM_CONST" %in% cond_fpd[cond_fpd$pos_id > colon_pos_id, "token"]) {
      forcond_number_flag <- TRUE
      cond_fpd <- cond_fpd[cond_fpd$pos_id > colon_pos_id, ]
      memory_alloc_number <- as.integer(cond_fpd[cond_fpd$token == "NUM_CONST", "text"])
    }
  }
  else if("SYMBOL_FUNCTION_CALL" %in% cond_fpd[cond_fpd$pos_id > in_pos & 
                                              cond_fpd$text == "seq_len", "token"]) {
    seqLen_pos_id <- cond_fpd[cond_fpd$pos_id > in_pos &
                               cond_fpd$token == "SYMBOL_FUNCTION_CALL" &
                               cond_fpd$text == "seq_len", "pos_id"]
    if("NUM_CONST" %in% cond_fpd[cond_fpd$pos_id > seqLen_pos_id, "token"]) {
      forcond_number_flag <- TRUE
      cond_fpd <- cond_fpd[cond_fpd$pos_id > seqLen_pos_id, ]
      memory_alloc_number <- as.integer(cond_fpd[cond_fpd$token == "NUM_CONST", "text"])
    }
  }
  else if("SYMBOL_FUNCTION_CALL" %in% cond_fpd[cond_fpd$pos_id > in_pos & 
                                              cond_fpd$text == "c", "token"]) {
    c_pos_id <- cond_fpd[cond_fpd$pos_id > in_pos &
                          cond_fpd$token == "SYMBOL_FUNCTION_CALL" &
                          cond_fpd$text == "c", "pos_id"]
    if("NUM_CONST" %in% cond_fpd[cond_fpd$pos_id > c_pos_id, "token"]) {
      forcond_number_flag <- TRUE
      cond_fpd <- cond_fpd[cond_fpd$pos_id > c_pos_id, ]
      memory_alloc_number <- 0
      for(i in cond_fpd$token) {
        if(i == "NUM_CONST") {
          memory_alloc_number = memory_alloc_number + 1
        }
      }
    }
  }
  
  forcond_number_flag == TRUE ? return (memory_alloc_number) : return (FALSE)
}


check_fun_call <- function(body_fpd) {
  return ("SYMBOL_FUNCTION_CALL" %in% body_fpd$token)
}


check_index_assignment <- function(body_fpd, index_name) {
  assign_operator_list <- list()
  assign_operator_list <- body_fpd[body_fpd$token %in% assigns, ]
  check_index_assignment_list <- failed_list <- NULL
  for(i in seq_len(length(assign_operator_list$id))) {
    check_pos_id <- assign_operator_list[i, "pos_id"]
    check_assign_op <- assign_operator_list[i, "token"]
    check_parent_id <- assign_operator_list[i, "parent"]
    
    open_bracket_flag <- close_bracket_flag <- FALSE
    name_of_vector <- vector(mode = "character", length = 1)
    ##If = or <- is used, then we will search the expr before the assignment operator
    if(check_assign_op == "EQ_ASSIGN" | check_assign_op == "LEFT_ASSIGN") {
      assigned_vector_node_id <- body_fpd[body_fpd$token == "expr" & 
                                         body_fpd$parent == check_parent_id &
                                         body_fpd$pos_id < check_pos_id, "id"]
      
      if(length(assigned_vector_node_id) == 0) {
        return (check_index_assignment_list)
      }
      
      parsed_assigned_expr <- get_children(body_fpd, assigned_vector_node_id)
      
      index_name_idx <- which(parsed_assigned_expr$text == index_name &
                                parsed_assigned_expr$token == "SYMBOL", arr.ind = TRUE)
      
      open_bracket_flag <- (parsed_assigned_expr[(index_name_idx - 1), "text"] == "[")
      close_bracket_flag <- (parsed_assigned_expr[(index_name_idx + 1), "text"] == "]")
      
      open_bracket_index <- which(parsed_assigned_expr$token == "'['", arr.ind = TRUE)
      name_of_vector <- parsed_assigned_expr[(open_bracket_index-1), "text"]
    
    } else {
      assigned_vector_node_id <- body_fpd[body_fpd$token == "expr" & 
                                            body_fpd$parent == check_parent_id &
                                            body_fpd$pos_id > check_pos_id, "id"]
      
      if(length(assigned_vector_node_id) == 0) {
        return (check_index_assignment_list)
      }
      
      parsed_assigned_expr <- get_children(body_fpd, assigned_vector_node_id)
      
      index_name_idx <- which(parsed_assigned_expr$text == index_name &
                                parsed_assigned_expr$token == "SYMBOL", arr.ind = TRUE)
      
      open_bracket_flag <- (parsed_assigned_expr[(index_name_idx - 1), "text"] == "[")
      close_bracket_flag <- (parsed_assigned_expr[(index_name_idx + 1), "text"] == "]")
    
      open_bracket_index <- which(parsed_assigned_expr$token == "'['", arr.ind = TRUE)
      name_of_vector <- parsed_assigned_expr[(open_bracket_index-1), "text"]
    }
    
    if(open_bracket_flag & close_bracket_flag) {
      check_index_assignment_list <- c(check_index_assignment_list, name_of_vector)
    } else {
      check_index_assignment_list <- c(check_index_assignment_list, name_of_vector)
      failed_list <- c(failed_list, name_of_vector)
    }
  }
  return (unique(check_index_assignment_list[!(check_index_assignment_list %in% failed_list)]))
}

# for_statement_ids <- fpd[fpd$token == "FOR", "parent"]

## Here we analyse all the nodes of custom_exam_nodes that consists of assignemnts and FOR loops
vector_initialization_list <- NULL
initializations_to_change <- NULL
vectors_memory_list <- NULL
for(i in seq_len(length(custom_exam_nodes$id))) {
  if(get_children(fpd, custom_exam_nodes[i, "id"], F)[1, "token"] == "FOR") {
    i <- custom_exam_nodes[i, "id"]
    sub_fpd <- get_children(fpd, i)
    
    cond_fpd <- get_children(sub_fpd, sub_fpd[sub_fpd$token=="forcond", "id"])
    cond_fpd <- restrict_condition_to_one_loop(cond_fpd)
    
    body_fpd <- sub_fpd[!(sub_fpd$id %in% cond_fpd$id), ]
    body_fpd <- restrict_body_to_one_loop(body_fpd)
    
    in_pos <- cond_fpd[cond_fpd$token == "IN", "pos_id"]
    index_name <- cond_fpd[cond_fpd$pos_id < in_pos &
                             cond_fpd$token == "SYMBOL", "text"]
    
    ## Checking condition 1: No function calls inside the loop, to counter the possibility of use of `<<-` in that particular function
    fun_call_flag <- check_fun_call(body_fpd)
    
    ## Checking condition 2: A number being explicitly mentioned in the for condition, and isolating that number if condition is true
    forcond_number_flag <- check_forcond_num_declaration(cond_fpd, index_name, in_pos)
    if(typeof(forcond_number_flag) != "logical") {
      memory_alloc_num <- forcond_number_flag
    }
    
    ## Checking condition 3: The index should be mentioned specifically of the vector to which something is being assigned
    passing_vectors <- check_index_assignment(body_fpd, index_name)
    index_assignment_flag <- (length(passing_vectors) == 0)
    
    # print(fun_call_flag)
    # print(forcond_number_flag)
    # print(index_assignment_flag)
    # print(passing_vectors)
    # 
    
    if(fun_call_flag | typeof(forcond_number_flag) == "logical" | index_assignment_flag) {
      next
    } else {
      initializations_to_change <- c(initializations_to_change, passing_vectors)
      initializations_to_change <- unique(initializations_to_change)
      for(vec in passing_vectors) {
        if(vec %in% names(vectors_memory_list)) {
          if(vectors_memory_list[[vec]] < memory_alloc_num) {
            vectors_memory_list[[vec]] <- memory_alloc_num
          } else {
            next
          }
        } else {
          vectors_memory_list[[vec]] <- memory_alloc_num
        }
      }
    }
  } else{
    # Here we associate all the vectors (of the same name too) with the IDs with which they were initialized.
    initialized_vector_name <- extract_vector_name(custom_exam_nodes[i, "id"])
    if(initialized_vector_name %in% names(vector_initialization_list)) {
      vector_initialization_list[[initialized_vector_name]] <- c(vector_initialization_list[[initialized_vector_name]], 
                                                                 custom_exam_nodes[i, id])
    } else {
      vector_initialization_list[[initialized_vector_name]] <- custom_exam_nodes[i, "id"]
    }
  }
}

# check_assignee <- NULL
# assign_pos_ids <- fpd[fpd$token %in% assigns, "pos_id"]
# for(position in assign_pos_ids) {
#   assigned_token <- fpd[fpd$pos_id == (position+1), "token"]
#   assigned_text <- fpd[fpd$pos_id == (position+1), "text"]
#   if(assigned_token == "NULL_CONST" | assigned_text == "c()") {
#     check_assignee <- c(check_assignee, fpd[fpd$pos_id == (position-2), "text"])
#   }
# }



