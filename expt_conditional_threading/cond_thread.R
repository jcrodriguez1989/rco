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
  "if(rnorm(1) > 5) {",
  "print(num %% 5)",
  "}",
  "if(rnorm(1) > 5) {",
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
  "if(num %% 2)",
  "  even_sum <- even_sum + num",
  sep = "\n"
))

pd <- parse_text(text5)
## pd
fpd <- flatten_leaves(pd)
## fpd


exam_nodes <- get_roots(pd)
exam_nodes_alt <- get_roots(fpd)

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

for(i in exam_nodes$id) {
  if(check_if(fpd, i)) {
    if(check_if_next(fpd, i)) {
      node1 <- first_if_expr(fpd, i)
      node2 <- consecutive_if_expr(fpd, i)
      if(!has_func_calls(fpd, node1, node2)) {
        if(check_negation(fpd, node1, node2)) {
          convert_to_else <- append(convert_to_else, node2)
          final_exam_nodes <- rbind(final_exam_nodes, exam_nodes[exam_nodes$id == i, ])
        } 
        else if(check_comparsion_logic_ge(fpd, node1, node2)) {
          convert_to_else <- append(convert_to_else, node2)
          final_exam_nodes <- rbind(final_exam_nodes, exam_nodes[exam_nodes$id == i, ])
        } 
        else if(check_comparsion_logic_le(fpd, node1, node2)) {
          convert_to_else <- append(convert_to_else, node2)
          final_exam_nodes <- rbind(final_exam_nodes, exam_nodes[exam_nodes$id == i, ])
        } 
        else if(check_duplicate_expr(fpd, node1, node2)) {
          merge_to <- append(merge_to, node1)
          merge_from <- append(merge_from, node2)
          final_exam_nodes <- rbind(final_exam_nodes, exam_nodes[exam_nodes$id == i, ])
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

# merge_from
# merge_to
# convert_to_else
# not_to_edit

########################################################################################################################################################################

to_expr <- NULL
from_expr <- NULL

for(i in 1:length(merge_from)) {
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

for(i in 1:length(merge_from)) {
  print(to_expr[i])
  print(from_expr[i])
}

final_exam_nodes

