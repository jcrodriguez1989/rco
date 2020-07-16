text1 <- paste(
  "v <- w <- x <- NULL",
  "for(i in 1:10) {",
  "  v[i] <- i*i",
  "}",
  "fun <- function(n) {",
  "  num <- NULL",
  "  for(i in c(2010,2011,2012,2013,2014,2015)) {",
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

pd <- parse_text(text4)
fpd <- flatten_leaves(pd)

fpd <- eq_assign_to_expr(fpd)
fpd

exam_nodes <- fpd[fpd$token == "expr", ]

check_forcond_num_declaration <- function(sub_fpd, index_name, in_pos) {
  forcond_number_flag <- FALSE
  if("':'" %in% sub_fpd[sub_fpd$pos_id > in_pos, "token"]) {
    colon_pos <- which(sub_fpd$token == "':'", arr.ind = TRUE)
    colon_pos_id <- sub_fpd[colon_pos, "pos_id"] 
    if("NUM_CONST" %in% sub_fpd[sub_fpd$pos_id > colon_pos_id, "token"]) {
      forcond_number_flag <- TRUE
      sub_fpd <- sub_fpd[sub_fpd$pos_id > colon_pos_id, ]
      memory_alloc_number <- as.integer(sub_fpd[sub_fpd$token == "NUM_CONST", "text"])
    }
  }
  else if("SYMBOL_FUNCTION_CALL" %in% sub_fpd[sub_fpd$pos_id > in_pos & 
                                              sub_fpd$text == "seq_len", "token"]) {
    seqLen_pos_id <- sub_fpd[sub_fpd$pos_id > in_pos &
                               sub_fpd$token == "SYMBOL_FUNCTION_CALL" &
                               sub_fpd$text == "seq_len", "pos_id"]
    if("NUM_CONST" %in% sub_fpd[sub_fpd$pos_id > seqLen_pos_id, "token"]) {
      forcond_number_flag <- TRUE
      sub_fpd <- sub_fpd[sub_fpd$pos_id > seqLen_pos_id, ]
      memory_alloc_number <- as.integer(sub_fpd[sub_fpd$token == "NUM_CONST", "text"])
    }
  }
  else if("SYMBOL_FUNCTION_CALL" %in% sub_fpd[sub_fpd$pos_id > in_pos & 
                                              sub_fpd$text == "c", "token"]) {
    c_pos_id <- sub_fpd[sub_fpd$pos_id > in_pos &
                          sub_fpd$token == "SYMBOL_FUNCTION_CALL" &
                          sub_fpd$text == "c", "pos_id"]
    if("NUM_CONST" %in% sub_fpd[sub_fpd$pos_id > c_pos_id, "token"]) {
      forcond_number_flag <- TRUE
      sub_fpd <- sub_fpd[sub_fpd$pos_id > c_pos_id, ]
      memory_alloc_number <- 0
      for(i in sub_fpd$token) {
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



for_statement_ids <- fpd[fpd$token == "FOR", "parent"]

for(i in for_statement_ids) {
  sub_fpd <- get_children(fpd, i)
  cond_fpd <- get_children(sub_fpd, sub_fpd[sub_fpd$token=="forcond", "id"])
  body_fpd <- sub_fpd[!(sub_fpd$id %in% cond_fpd$id), ]
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
  
  print(fun_call_flag)
  print(forcond_number_flag)
  print(index_assignment_flag)
  # 
  if(fun_call_flag | typeof(forcond_number_flag) == "logical" | index_assignment_flag) {
    next
  } else {
    ## Here we have to make changes in the x <- NULL statements.
    ## Find and delete the x <- NULL statements and they will be referred to as not_to_edit
    ## Then write x <- vector(length = 5) and call it final_exam_nodes and later merge it.
    
    
  }
}

check_assignee <- NULL
assign_pos_ids <- fpd[fpd$token %in% assigns, "pos_id"]
for(position in assign_pos_ids) {
  assigned_token <- fpd[fpd$pos_id == (position+1), "token"]
  assigned_text <- fpd[fpd$pos_id == (position+1), "text"]
  if(assigned_token == "NULL_CONST" | assigned_text == "c()") {
    check_assignee <- c(check_assignee, fpd[fpd$pos_id == (position-2), "text"])
  }
}

check_assignee

v <- NULL
for(i in 1:5) {
  v[i] <- i
}

for(i in 1:3) {
  v[i] <- i*i
}



## custom_exam_nodes
null_parents <- fpd[fpd$token == "NULL_CONST", "parent"]
custom_exam_nodes <- NULL
for(i in null_parents) {
  custom_exam_nodes <- rbind(custom_exam_nodes, fpd[fpd$id == i, ])
}
for(i in exam_nodes$id) {
  if(get_children(fpd, i, FALSE)[1, "token"] == "FOR") {
    custom_exam_nodes <- rbind(custom_exam_nodes, fpd[fpd$id == i, ])
  }
}
custom_exam_nodes <- custom_exam_nodes[order(custom_exam_nodes$pos_id), ]


