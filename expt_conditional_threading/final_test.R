text <- paste(list(
  "num <- sample(1:100, 1)",
  "even_sum_a <- 0",
  "even_sum_b <- 0",
  "#Hello, I am a comment.",
  "odd_sum_a <- 0",
  "odd_sum_b <- 0",
  "if(num %% 2) {",
  "odd_sum_a <- odd_sum_a + num",
  "}",
  "if(num %% 2) {",
  "odd_sum_b <- odd_sum_b + num",
  "}",
  "if(!(num %% 2)) {",
  "even_sum_a <- even_sum_a + num",
  "}",
  "if(!(num %% 2)) {",
  "even_sum_b <- even_sum_b + num",
  "}",
  sep = "\n"
))

text2 <- paste(list(
  "num <- sample(1:100, 1)",
  "even_sum_a <- 0",
  "even_sum_b <- 0",
  "odd_sum_a <- 0",
  "odd_sum_b <- 0",
  "if(num %% 2) {",
  "odd_sum_a <- odd_sum_a + num",
  "}",
  "if(num %% 2) {",
  "odd_sum_b <- odd_sum_b + num",
  "}",
  "if(!(num %% 2)) {",
  "even_sum_a <- even_sum_a + num",
  "}",
  sep = "\n"
))

pd <- parse_text(text)
fpd <- flatten_leaves(pd)

## Taking care of expressions from functions
fun_ids <- fpd$id[fpd$token == "FUNCTION"]      
fun_prnt_ids <- fpd$parent[fpd$id %in% fun_ids] 

fun_exam_nodes <- NULL
for(i in fun_prnt_ids){
  fun_exam_nodes <- rbind(fun_exam_nodes, get_children(fpd, i, FALSE)[get_children(fpd, i, FALSE)$token == "expr", ])
}

exam_nodes <- get_roots(pd)
exam_nodes <- rbind(exam_nodes, fun_exam_nodes)

exam_nodes_alt <- get_roots(fpd)
exam_nodes_alt <- rbind(exam_nodes_alt, fun_exam_nodes)

## This function checks for IF Statement in the given node id of the code snippet

check_if <- function(fpd, node_id) {
  if("IF" %in% get_children(fpd, node_id)$token) {
    return (TRUE)
  } else {
    return (FALSE)
  }
}

## This function checks for IF Statement immediately after the given IF statement
## And returns the pos_id of the next IF statement

check_if_next <- function(fpd, node_id) {
  if(check_if(fpd, node_id)) {
    row_num <- which(exam_nodes$id == node_id, arr.ind = TRUE)
    if(row_num != nrow(exam_nodes)) {
      next_node_id <- exam_nodes[row_num+1, "id"]
      return (check_if(fpd, next_node_id))
    } else {
      return (FALSE)
    }
  } else {
    return (FALSE)
  }
}

## If it is confirmed that there exists two consecutive IF statements in our code, we use this function to 
## get the "id" of the expression inside the first IF statement.

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


## This function checks whether another IF statement is present immediately after the first discovered if statement. 
## If the IF statement is presents, it returns the expression inside of that if statement (2nd one).

consecutive_if_expr <- function(fpd, node_id) {
  row_num <- which(exam_nodes$id == node_id, arr.ind = TRUE)
  start_id <- exam_nodes[row_num+1, "id"]
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

## This function will help us to compare the expressions obtained from inside the two IF statements and it will determine
## whether the expression of the second IF statement is an exact negation of the expr of the first IF statement or not.

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

## This function checks whether the two given IF statements have the same expression

check_duplicate_expr <- function(fpd, node1, node2) {
  first_expr <- fpd[fpd$id == node1, "text"]
  second_expr <- fpd[fpd$id == node2, "text"]
  
  first_expr <- gsub(" ", "", first_expr, fixed = TRUE)
  second_expr <- gsub(" ", "", second_expr, fixed = TRUE)
  
  return (first_expr == second_expr)
}

## This function will help us to compare the expressions obtained from inside the two IF statements and it will determine
## whether the expression of the second IF statement is the only other logical possibilty of first IF statement or not.

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


## This function will help us to compare the expressions obtained from inside the two IF statements and it will determine
## whether the expression of the second IF statement is the only other logical possibilty of first IF statement or not.

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

## This function checks whether there functions used in the expressions of your IF Statements

has_func_calls <- function(fpd, node1, node2) {
  return ("SYMBOL_FUNCTION_CALL" %in% get_children(fpd, node1)$token | 
            "SYMBOL_FUNCTION_CALL" %in% get_children(fpd, node2))  
}

## This function returns the id of the `IF` statement from the expression

get_if_block_expr <- function(fpd, node_id) {
  expr_id <- exam_nodes[which(exam_nodes$id %in% get_ancestors(fpd, node_id), arr.ind = T), "id"][1]
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

get_modified_if_expr <- function(exam_nodes, index) {
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

get_og_id <- function(fpd, node_id) {
  return ( expr_id <- exam_nodes[which(exam_nodes$id %in% get_ancestors(fpd, node_id), arr.ind = T), "id"][1])
}

to_change_node <- NULL
to_remove_node <- NULL
merge_to <- NULL
merge_from <- NULL

for(itr in seq_len(length(exam_nodes$id))) {
  i <- exam_nodes[itr, "id"]
  if(check_if(fpd, i)) {
    if(check_if_next(fpd, i)) {
      node1 <- first_if_expr(fpd, i)
      node2 <- consecutive_if_expr(fpd, i)
      if(!has_func_calls(fpd, node1, node2)) {
        if(check_duplicate_expr(fpd, node1, node2)) {
          merge_to <- append(merge_to, node1)
          merge_from <- append(merge_from, node2)
          to_change_node <- append(to_change_node, itr)
          to_remove_node <- append(to_remove_node, itr+1)
} } } } }

exam_nodes

to_expr <- NULL
from_expr <- NULL

for(i in seq_len(length(merge_from))) {
  to_expr[i] <- get_if_block_expr(fpd, merge_to[i])
  if(grepl("{\n", to_expr[i], fixed = TRUE)) {
    to_expr[i] <- gsub("{\n", "", to_expr[i], fixed = TRUE)
    to_expr[i] <- gsub("\n}", "", to_expr[i], fixed = TRUE)
  } 
  else if(grepl("{", to_expr[i], fixed = TRUE)) {
    to_expr[i] <- gsub("{", "", to_expr[i], fixed = TRUE)
    to_expr[i] <- gsub("}", "", to_expr[i], fixed = TRUE)
  }
  
  from_expr[i] <- get_if_block_expr(fpd, merge_from[i])
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

exam_nodes <- exam_nodes[-(to_remove_node), ]

to_change_node <- NULL
to_remove_node <- NULL
convert_to_else <- NULL

for(itr in seq_len(length(exam_nodes$id))) {
  i <- exam_nodes[itr, "id"]
  if(check_if(fpd, i)) {
    if(check_if_next(fpd, i)) {
      node1 <- first_if_expr(fpd, i)
      node2 <- consecutive_if_expr(fpd, i)
      if(!has_func_calls(fpd, node1, node2)) {
        if(check_negation(fpd, node1, node2)) {
          convert_to_else <- append(convert_to_else, itr+1)
          to_change_node <- append(to_change_node, itr)
          to_remove_node <- append(to_remove_node, itr+1)
} } } } }

else_expr <- NULL
for(i in seq_len(length(convert_to_else))) {
  else_expr[i] <- get_modified_if_expr(exam_nodes, convert_to_else[i])
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

exam_nodes <- exam_nodes[-(to_remove_node), ]
exam_nodes

new_fpd <- NULL
for(itr in seq_len(nrow(exam_nodes)))
{
  act_fpd <- exam_nodes[itr, ]
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

test_fpd <- flatten_leaves(new_fpd)

###############################################################################################################################################################################################################################################
## Operation Clean Up :p
#############################################################################################################################################################################################################################################
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
deletion_nodes <- NULL
for(i in exam_nodes$id){
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






































