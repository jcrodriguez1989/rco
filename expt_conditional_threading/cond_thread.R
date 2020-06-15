text1 <- paste(list(
  "num <- sample(1:100, 1)",
  "even_sum <- 0",
  "odd_sum <- 0",
  "if(num %% 2) {",
  "odd_sum <- odd_sum + num",
  "}",
  "if(!(num %% 2)) {",
  "even_sum <- even_sum + num",
  "}",
  "if(num > 5) {",
  "print(num %% 5)",
  "}",
  "if(num > 5) {",
  "print(num)",
  "}",
  sep = "\n"
))

text2 <- paste(list(
  "num <- sample(1:100, 1)",
  "even_sum <- 0",
  "odd_sum <- 0",
  "count <- 0",
  "if(even_sum + odd_sum < 10) {",
  "if(num %% 2)",
  "  odd_sum <- odd_sum + num",
  "if(!(num %% 2))",
  "  even_sum <- even_sum + num",
  "count <- count + 1",
  "}",
  sep = "\n"
))

text3 <- paste(list(
  "num_vec <- sample(1:5, 10, replace = T)",
  "ones <- 0",
  "others <- 0",
  "for(i in num_vec) {",
  "if(i == 1) {",
  "  ones <- ones + 1",
  "}",
  "if(i != 1) {",
  "  others <- others + 1",
  "}}",
  sep = "\n"
))

text4 <- paste(list(
  "is_even <- function(a) {",
  "if(a %% 2) {",
  "  return (FALSE)",
  "}",
  "if(!(a %% 2)) {",
  "  return (TRUE)",
  "}",
  "is_even(150)",
  sep = "\n"
))

text5 <- paste(list(
  "num <- sample(1:100, 1)",
  "even_sum_a <- 0",
  "even_sum_b <- 0",
  "odd_sum_a <- 0",
  "odd_sum_b <- 0",
  "if(num %% 2) {",
  "odd_sum_a <- odd_sum_a + num",
  "}",
  "if(num %% 2) ",
  "  odd_sum_b <- odd_sum_b + num",
  "if(!(num %% 2)) {",
  "even_sum_a <- even_sum_a + num",
  "}",
  "if(!(num %% 2)) {even_sum_b <- even_sum_b + num}",
  sep = "\n"
))

text6 <- paste(list(
  "num <- sample(1:100, 1)",
  "even_sum <- 0",
  "odd_sum <- 0",
  "if(num %% 2)",
  "  odd_sum <- odd_sum + num",
  "else {",
  "  even_sum <- even_sum + num",
  "}",
  sep = "\n"
))

pd <- parse_text(text6)
## pd
fpd <- flatten_leaves(pd)
## fpd

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


## exam_nodes
## exam_nodes_alt

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

get_og_id <- function(fpd, node_id) {
  return ( expr_id <- exam_nodes[which(exam_nodes$id %in% get_ancestors(fpd, node_id), arr.ind = T), "id"][1])
}

##########################################################################################################################################################################
# exam_nodes
# check_if(fpd, 149)
# check_if_next(fpd, 149)
# first_if_expr(fpd, 149)
# consecutive_if_expr(fpd, 149)
# check_comparsion_logic_le(fpd, 124, 160)
# check_comparsion_logic_le(fpd, 160, 124)

###########################################################################################################################################################################

merge_from <- NULL
merge_to <- NULL
convert_to_else <- NULL
not_to_edit <- NULL
final_exam_nodes <- NULL
method_used <- NULL

for(itr in seq_len(length(exam_nodes$id))) {
  i <- exam_nodes[itr, "id"]
  if(check_if(fpd, i)) {
    if(check_if_next(fpd, i)) {
      node1 <- first_if_expr(fpd, i)
      node2 <- consecutive_if_expr(fpd, i)
      if(!has_func_calls(fpd, node1, node2)) {
        if(check_negation(fpd, node1, node2)) {
          convert_to_else <- append(convert_to_else, node2)
          method_used <- append(method_used, 2)
          final_exam_nodes <- rbind(final_exam_nodes, exam_nodes[itr + 1, ])
        } 
        else if(check_comparsion_logic_ge(fpd, node1, node2)) {
          convert_to_else <- append(convert_to_else, node2)
          method_used <- append(method_used, 2)
          final_exam_nodes <- rbind(final_exam_nodes, exam_nodes[itr + 1, ])
        } 
        else if(check_comparsion_logic_le(fpd, node1, node2)) {
          convert_to_else <- append(convert_to_else, node2)
          method_used <- append(method_used, 2)
          final_exam_nodes <- rbind(final_exam_nodes, exam_nodes[itr + 1, ])
        } 
        else if(check_duplicate_expr(fpd, node1, node2)) {
          merge_to <- append(merge_to, node1)
          merge_from <- append(merge_from, node2)
          final_exam_nodes <- rbind(final_exam_nodes, exam_nodes[exam_nodes$id == i, ])
          method_used <- append(method_used, 1)
        } 
        else {
          not_to_edit <- rbind(not_to_edit, exam_nodes[exam_nodes$id == i, ])
        }
      }
      else {
        not_to_edit <- rbind(not_to_edit, exam_nodes[exam_nodes$id == i, ])
      }
    }
  }
  else {
    not_to_edit <- rbind(not_to_edit, exam_nodes[exam_nodes$id == i, ])
  }
}

########################################################################################################################################################################

 merge_from
 merge_to
 convert_to_else
 not_to_edit

########################################################################################################################################################################

## These loops are used to form the content from inside the IF statements that have to be merged.

to_expr <- NULL
from_expr <- NULL

for(i in seq_len(length(merge_from))) {
  to_expr[i] <- get_if_block_expr(fpd, merge_to[i])
  to_expr[i + length(merge_from)] <- get_og_id(fpd, merge_to[i])
  if(grepl("{\n", to_expr[i], fixed = TRUE)) {
    to_expr[i] <- gsub("{\n", "", to_expr[i], fixed = TRUE)
    to_expr[i] <- gsub("\n}", "", to_expr[i], fixed = TRUE)
  } 
  else if(grepl("{", to_expr[i], fixed = TRUE)) {
    to_expr[i] <- gsub("{", "", to_expr[i], fixed = TRUE)
    to_expr[i] <- gsub("}", "", to_expr[i], fixed = TRUE)
  }
    
  from_expr[i] <- get_if_block_expr(fpd, merge_from[i])
  from_expr[i + length(merge_from)] <- get_og_id(fpd, merge_from[i])
  if(grepl("{\n", from_expr[i], fixed = TRUE)) {
    from_expr[i] <- gsub("{\n", "", from_expr[i], fixed = TRUE)
    from_expr[i] <- gsub("\n}", "", from_expr[i], fixed = TRUE)
  } 
  else if(grepl("{", from_expr[i], fixed = TRUE)) {
    from_expr[i] <- gsub("{", "", from_expr[i], fixed = TRUE)
    from_expr[i] <- gsub("}", "", from_expr[i], fixed = TRUE)
  }
}

else_expr <- NULL
for(i in seq_len(length(convert_to_else))) {
  else_expr[i] <- get_if_block_expr(fpd, convert_to_else[i])
  if(grepl("{\n", else_expr[i], fixed = TRUE)) {
    else_expr[i] <- gsub("{\n", "", else_expr[i], fixed = TRUE)
    else_expr[i] <- gsub("\n}", "", else_expr[i], fixed = TRUE)
  } 
  else if(grepl("{", else_expr[i], fixed = TRUE)) {
    else_expr[i] <- gsub("{", "", else_expr[i], fixed = TRUE)
    else_expr[i] <- gsub("}", "", else_expr[i], fixed = TRUE)
  }
}

########################################################################################################################################################################

for(i in seq_len(length(merge_from))) {
  print(to_expr[i])
  print(from_expr[i])
}

for(i in seq_len(length(convert_to_else))) {
  print(else_expr[i])
}

########################################################################################################################################################################

can_convert_to_else <- function(itr) {
  flag <- TRUE
  for(i in seq_len(length(final_exam_nodes$id))){
    if(method_used[i] == 2) {
      node_id <- final_exam_nodes[i, "id"]
      for(node in seq_len(length(final_exam_nodes$id))) {
        if(final_exam_nodes[node, "id"] == node_id & method_used[node] == 1) {
          flag <- FALSE
        }
      }
    } else {
      flag <- FALSE
    }
    return (flag)
  }
}

## This loop will take care of merging apart from the case where else statements have to be merged. Have a look at text5 as an example 
##(There the last two `IF` statements have to merged.)

## The `check_new_else_nodes` contains the indices from the `final_exam_nodes` that have to be converted to `else`

# merge_to = 1, convert_to_else = 2
j <- 1
k <- 1
check_new_else_nodes <- NULL 
merged_else <- FALSE
for(i in seq_len(length(final_exam_nodes$id))) {
  if(method_used[i] == 1) {
    node_id <- final_exam_nodes[i, "id"]
    if_cond <- fpd[fpd$id == first_if_expr(fpd, node_id), "text"]
    string1 <- paste("if", "(", if_cond, ") ", "{\n", sep = "")
    string2 <- paste(to_expr[j], "\n", from_expr[j], "\n}", sep = " ")
    final_exam_nodes[i, "text"] <- paste(string1, string2)
    j <- j + 1
  }  
  else if(can_convert_to_else(i)) {
    final_exam_nodes[i, "text"] <- paste("`", "else", "{\n", else_expr[k], " \n}", "`", sep = " ")
    k <- k + 1
  }
  else {
    check_new_else_nodes <- append(check_new_else_nodes, i)
    merged_else <- TRUE
  }
}


if(merged_else) {
  ## This loop is used to convert to ELSE statement after all the merging(of IF statements) has been done.
  ## using the vector called `check_new_else_nodes`.
  for(i in check_new_else_nodes) {
    new_else_id <- final_exam_nodes[i, "id"]
    id_index <- which(to_expr == new_else_id, arr.ind = T)
    id_index <- id_index - length(merge_to)
    final_exam_nodes[i, "text"] <- paste("`", "else", "{\n", to_expr[id_index], "\n", from_expr[id_index], "\n}", "`", sep = " ")
  }
  
  ## This loop will remove the "extra" entry of merged IF statements that should essentially be ab ELSE statement.
  ## And, we know it was converted to ELSE in the last loop.
  for(i in check_new_else_nodes) {
    to_remove_id <- final_exam_nodes[i, "id"]
    for(itr in seq_len(length(final_exam_nodes$id))) {
      if(to_remove_id == final_exam_nodes[itr, "id"] & method_used[itr] == 1)
        final_exam_nodes <- final_exam_nodes[-itr, ]
    }
  }
}

#############################################################################################################################################################################
final_exam_nodes #This is the final version
##########################################################################################################################################################################

## Here, we are trying to convert the exam_nodes back to a parsed_code form starting from not_to_edit
for(i in seq_len(nrow(not_to_edit))) {
  not_to_edit <- rbind(not_to_edit, get_children(pd, not_to_edit[i, "id"]))
}

## Arranging the not_to_edit by the `pos_id` parameter.
not_to_edit <- not_to_edit[order(not_to_edit$pos_id), ]

not_to_edit_final <- NULL
for(i in unique(not_to_edit$id)) {
  not_to_edit_final <- rbind(not_to_edit_final, unique(not_to_edit[not_to_edit$id == i, ]))
}

final_exam_nodes_ids <- final_exam_nodes$id

new_fpd <- NULL
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
























