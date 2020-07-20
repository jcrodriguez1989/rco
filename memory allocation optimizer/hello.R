#############################################################################################################################################################################################
#### Example texts ####
#############################################################################################################################################################################################

text1 <- paste(
  "v <- NULL",
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

#############################################################################################################################################################################################
#### Preparing pd, fpd, exam_nodes and custom_exam_nodes ####
#############################################################################################################################################################################################

pd <- parse_text(text4)

fpd <- flatten_leaves(pd)
fpd <- eq_assign_to_expr(fpd) ## eq_assign_to_expr helps to convert the `EQ_ASSIGN` token to `expr`

## taking a different approach from simply calling get_children(fpd) and including all `expr`.
exam_nodes <- fpd[fpd$token == "expr", ]

## custom_exam_nodes
## custom_exam_nodes will contain only loops and the assignment nodes from the entire exam_nodes
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

## Check pd, fpd, exam_nodes and custom_exam_nodes
pd
fpd
exam_nodes
custom_exam_nodes

###########################################################################################################################################################################################
#### Auxillary functions ####
###########################################################################################################################################################################################

## If an `expr` from the exam_nodes list is a nested `FOR` loop, then this function restricts the search area of the resulting
## data.frame to the *body* contents of one(starting) loop.
restrict_body_to_one_loop <- function(body_fpd) {
  list_of_fors <- body_fpd[body_fpd$token == "FOR", "pos_id"]
  if(length(list_of_fors) > 1) {
    body_fpd <- body_fpd[body_fpd$pos_id < list_of_fors[[2]], ]
  }
  body_fpd
}

## If an `expr` from the exam_nodes list is a nested `FOR` loop, then this function restricts the search area of the resulting
## data.frame to the *condition* contents of one(starting) loop.
restrict_condition_to_one_loop <- function(cond_fpd) {
  list_of_conds <- cond_fpd[cond_fpd$token == "forcond", "pos_id"]
  if(length(list_of_conds) > 1) {
    cond_fpd <- cond_fpd[cond_fpd$pos_id < list_of_conds[[2]], ]
  }
  cond_fpd
}

## Given the `id` of the `expr` from custom_exam_nodes, find the name of the vector in that `expr`
## The given `expr` will always be of the form  *vec <- NULL* or *vec <- c()* 
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

## This function checks whether a number is explicitly mentioned in the condition of the `FOR` loop
## If true, the number is returned else FALSE is returned
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

## This function checks for presence of functions inside the body of `loops`.
## Presence of functions inside the body is undesirable because functions can introduce unexpected changes to the iterator 
check_fun_call <- function(body_fpd) {
  return ("SYMBOL_FUNCTION_CALL" %in% body_fpd$token)
}

## This function checks whether the index is explicitly being mentioned at the time of assignment inside the loop
## Returns list of vectors that have the index, if none are present empty list is returned
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

###########################################################################################################################################################################################
#### End of auxillary functions, analysis of custom_exam_nodes begin ####
###########################################################################################################################################################################################

## Here we analyse all the nodes of custom_exam_nodes that consists of assignemnts and FOR loops
vector_initialization_list <- list()
initializations_to_change <- NULL
vectors_memory_list <- list()

## if condition handles the loops from custom_exam_nodes and the else part handles the vector assignment from custom_exam_nodes  
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
      vector_initialization_list[[initialized_vector_name]] <- c(vector_initialization_list[[initialized_vector_name]], custom_exam_nodes[i, "id"])
    } else {
      vector_initialization_list[[initialized_vector_name]] <- custom_exam_nodes[i, "id"]
    }
  }
}

####################################################################################################################################################################################################################
## Now the id of the nodes that have to be edited is stored in vector_initialization_list, and when we remove those nodes from exam_nodes we have the not_to_edit fpd
####################################################################################################################################################################################################################

edit_nodes_list <- NULL
for(i in seq_len(length(vector_initialization_list))) {
  if(names(vector_initialization_list)[i] %in% names(vectors_memory_list)) {
    edit_nodes_list <- c(edit_nodes_list, vector_initialization_list[[i]])
  }
}

not_to_edit <- exam_nodes[!(exam_nodes$id %in% edit_nodes_list), ]
not_to_edit_copy <- not_to_edit

for(i in seq_len(length(not_to_edit_copy$id))) {
  not_to_edit <- rbind(not_to_edit, get_children(fpd, not_to_edit_copy[i, "id"]))
}

not_to_edit <- not_to_edit[order(not_to_edit$pos_id), ]

not_to_edit <- unique(not_to_edit)

#####################################################################################################################################################################################################################
#### Creating the final_exam_nodes list from the `vector_initialization_list` and `vectors_memory_list` ####
#####################################################################################################################################################################################################################

final_exam_nodes <- exam_nodes[exam_nodes$id %in% edit_nodes_list, ]

not_to_edit <- not_to_edit[order(not_to_edit$pos_id), ]
not_to_edit <- unique(not_to_edit)

## final_exam_nodes_copy <- final_exam_nodes
## Now we iterate over all the entries of final_exam_nodes and change their text
for(i in seq_len(length(final_exam_nodes$id))) {
  name_of_vector <- extract_vector_name(final_exam_nodes[i, "id"])
  final_exam_nodes[i, "text"] <- sprintf("%s <- vector(length = %d)", name_of_vector, vectors_memory_list[[name_of_vector]])
}


## sprintf(".subset2(%s, %s)", data_frame[i], column_name[i])
###################################################################################################################################################################################################################
#### Merging the final_exam_nodes and the not_to_edit and subsequent cleaning operations ####
###################################################################################################################################################################################################################

final_exam_nodes_ids <- final_exam_nodes$id

new_fpd <- NULL
for(itr in seq_len(length(final_exam_nodes$id))) {
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

###################################################################################################################################################################################################################
#### Final Steps of molding the not_to_edit and final_exam_nodes into the same fpd ####
###################################################################################################################################################################################################################

resultant_fpd <- rbind(not_to_edit, new_fpd)
resultant_fpd <- resultant_fpd[order(resultant_fpd$pos_id), ]

test_fpd <- flatten_leaves(resultant_fpd)

comments_ids <- curr_new_line_ids <- next_line_ids <- correction_nodes <- correction_ids <- NULL

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
deletion_nodes <- NULL
for(i in final_exam_nodes_ids){
  if(i %in% test_fpd$id){
    deletion_nodes <- rbind(deletion_nodes, get_children(test_fpd, i))
  }
}

next_line_ids <- as.double(test_fpd[test_fpd$id %in% deletion_nodes$id & test_fpd$next_lines == 1, "pos_id"])

test_fpd <- test_fpd[!(test_fpd$id %in% deletion_nodes$id), ]

for(i in next_line_ids){
  curr_new_line_ids <- append(curr_new_line_ids, 
                              max(which(test_fpd$pos_id < i)))  
}

for (i in curr_new_line_ids) {
  test_fpd[i, ]$next_lines <- 1
}

cat(deparse_data(test_fpd))






############################################################################################################################################################################################
#### Discarded Code Snippets, May prove useful later on ####
############################################################################################################################################################################################
# check_assignee <- NULL
# assign_pos_ids <- fpd[fpd$token %in% assigns, "pos_id"]
# for(position in assign_pos_ids) {
#   assigned_token <- fpd[fpd$pos_id == (position+1), "token"]
#   assigned_text <- fpd[fpd$pos_id == (position+1), "text"]
#   if(assigned_token == "NULL_CONST" | assigned_text == "c()") {
#     check_assignee <- c(check_assignee, fpd[fpd$pos_id == (position-2), "text"])
#   }
# }

## Check whether the loops containing the vectors from the *vectors_memory_list* have a function call inside them
## If yes, remove that node from the *final_exam_nodes* and add it to *not_to_edit*
# for(i in seq_len(length(final_exam_nodes$id))) {
#   check_vector_id <- final_exam_nodes[i, "id"]
#   check_vector_name <- extract_vector_name(check_vector_id)
#   if(!(check_vector_name %in% names(vectors_memory_list))) {
#     not_to_edit <- rbind(not_to_edit, get_children(fpd, final_exam_nodes[i, "id"]))
#     final_exam_nodes <- final_exam_nodes[-i, ]
#   }
# }




