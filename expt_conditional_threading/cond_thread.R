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
  "if(num <= 5) {",
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
  "if(num %% 2)",
  "  odd_sum <- odd_sum + num",
  "if(!(num %% 2))",
  "  even_sum <- even_sum + num",
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

pd <- parse_text(text1)
## pd
fpd <- flatten_leaves(pd)
## fpd


exam_nodes <- get_roots(pd)
exam_nodes_alt <- get_roots(fpd)

## exam_nodes
## exam_nodes_alt

## This function checks for IF Statements in the given code snippet
## If present it returns the (pos_id + 1) of the our IF statement. 

check_if <- function(fpd, node_id) {
  if("IF" %in% get_children(fpd, node_id)$token) {
    last_row <- nrow(get_children(fpd, node_id))
    last_pos <- get_children(fpd, node_id)[last_row, "pos_id"]
    return (last_pos + 1)
  } else {
    return (FALSE)
  }
}

## This function checks whether another IF statement is present immediately after the first discovered if statement. 
## If the IF statement is presents, it returns the expression inside of that if statement (2nd one).

check_consecutive_if <- function(fpd, node_id) {
    start_pos <- check_if(fpd, node_id)
    start_id <- fpd[fpd$pos_id == start_pos, "id"]
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

## This function will help us to compare the expressions obtained from inside the two IF statements and it will determine
## whether the expression of the second IF statement is an exact negation of the expr of the first IF statement or not.

check_negation <- function(fpd, node_id) {
  first_expr <- fpd[fpd$id == first_if_expr(fpd, node_id), "text"]
  second_expr <- fpd[fpd$id == check_consecutive_if(fpd, node_id), "text"]
  check_expr1 <- gsub(" ", "", paste("!", first_expr, sep = ""), fixed = TRUE)
  check_expr2 <- gsub(" ", "", paste("!","(", first_expr, ")", sep = ""), fixed = TRUE)
  second_expr_no_whitespace <- gsub(" ", "", second_expr, fixed = TRUE)
  if(check_expr1 == second_expr_no_whitespace) {
    return (check_consecutive_if(fpd, node_id))
  } 
  else if(check_expr2 == second_expr_no_whitespace) {
    return (check_consecutive_if(fpd, node_id))
  } 
  else 
    return (FALSE)
}

## This function will help us to compare the expressions obtained from inside the two IF statements and it will determine
## whether the expression of the second IF statement is the only other logical possibilty of first IF statement or not.

check_comparsion_logic_ge <- function(fpd, node_id) {
  first_expr <- fpd[fpd$id == first_if_expr(fpd, node_id), "text"]
  second_expr <- fpd[fpd$id == check_consecutive_if(fpd, node_id), "text"]
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

check_comparsion_logic_le <- function(fpd, node_id) {
  first_expr <- fpd[fpd$id == first_if_expr(fpd, node_id), "text"]
  second_expr <- fpd[fpd$id == check_consecutive_if(fpd, node_id), "text"]
  if(length(grep("<=", first_expr)) > 0) {
    first_expr <- gsub(" ", "", first_expr, fixed = T)
    first_expr <- gsub("<=", ">", first_expr)
    second_expr <- gsub(" ", "", second_expr, fixed = TRUE)
    if(first_expr == second_expr) {
      return (check_consecutive_if(fpd, node_id))
    } else {
      return (FALSE)
    }
  } 
  else if(length(grep("<=", second_expr)) > 0) {
    second_expr <- gsub(" ", "", second_expr, fixed = TRUE)
    first_expr <- gsub(" ", "", first_expr, fixed = TRUE)
    second_expr <- gsub("<=", ">", second_expr)
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


# exam_nodes
# check_if(fpd, 74)
# check_consecutive_if(fpd, 74)
# fpd[fpd$id == "97", "text"]
# fpd[fpd$id == first_if_expr(fpd, 74), "text"]






