#' Optimizer: Efficient Column Extraction.
#'
#' Performs optimisations for column extraction throughout the code.
#' Carefully examine the results after running this function!
#'
#' @param texts A list of character vectors with the code to optimize.
#' 
#' @examples
#' code <- paste(
#'  "mtcars[ , 11]",
#'  "#This is a comment!!",
#'  "mtcars [[11]]",
#'  "mtcars$carb", 
#'  "yo <- 1",
#'  "yo",
#'  "mtcars[[c(11)]]",
#'  ".subset2(mtcars, 11)",
#'   sep = "\n"
#' )
#' cat(opt_column_extractor(list(code))$codes[[1]])
#' @export
#'
opt_column_extractor <- function(texts) {
  res <- list()
  res$codes <- lapply(texts, ce_one_file)
  return (res)
}

# Executes efficient column selection/extraction on one text of code.
#
# @param text A character vector with code to optimize.
#
ce_one_file <- function(text)# Executes searching operations to find dollar sign.
{
  fpd <- parse_text(texts)
  fpd <- flatten_leaves(fpd)
  res_fpd <- ce_one_fpd(fpd)
  return (deparse_data(res_fpd))
}

#
# @param fpd A flatten parsed data data.frame.
# @param node_id Id of the node to be examined
#
has_dollar_sign <- function(fpd, node_id)
{
  test_dollar_df <- fpd[fpd$parent == node_id, ]
  dollar <- match("'$'", test_dollar_df$token)
  dollar_flag <- (!(is.na(dollar)))
  return (dollar_flag)
}


# Executes searching operations related to dollar sign to find dataset.
#
# @param fpd A flatten parsed data data.frame.
# @param node_id Id of the node to be examined
#
dollar_dataset <- function(fpd, node_id)
{
  dollar_loc <- fpd[fpd$parent == node_id & fpd$token == "'$'", ]
  replace_df_dollar <- fpd[fpd$pos_id < dollar_loc$pos_id & fpd$parent == node_id & fpd$token == "SYMBOL", ]$text
  return (replace_df_dollar)
}


# Executes searching operations related to dollar sign to find column number.
#
# @param fpd A flatten parsed data data.frame.
# @param node_id Id of the node to be examined
#
dollar_colnum <- function(fpd, node_id)
{
  dollar_loc_colnum <- fpd[fpd$parent == node_id & fpd$token == "'$'", ]
  replace_colname_dollar <- fpd[fpd$pos_id > dollar_loc_colnum$pos_id & fpd$parent == node_id & fpd$token == "SYMBOL", ]$text
  return (which(colnames(mtcars) == replace_colname_dollar[1]))
}

# Executes searching operations to find double square brackets(LBB).
#
# @param fpd A flatten parsed data data.frame.
# @param node_id Id of the node to be examined
#
has_square_brackets <- function(fpd, node_id)
{
  test_square <- fpd[fpd$parent == node_id, ]
  square <- match("LBB", test_square$token)
  square_flag <- (!(is.na(square)))
  return (square_flag)
}

# Executes searching operations related to double square brackets(LBB) to find dataset.
#
# @param fpd A flatten parsed data data.frame.
# @param node_id Id of the node to be examined
#
square_dataset <- function(fpd, node_id)
{
  square_loc <- fpd[fpd$parent == node_id & fpd$token == "LBB", ]
  replace_df_square <- fpd[fpd$pos_id < square_loc$pos_id & fpd$parent == node_id & fpd$token == "SYMBOL", ]$text
  return (replace_df_square)
}

# Executes searching operations related to double square brackets(LBB) to find column number.
#
# @param fpd A flatten parsed data data.frame.
# @param node_id Id of the node to be examined
#
square_colnum <- function(fpd, node_id)
{
  square_loc_colnum <- fpd[fpd$parent == node_id & fpd$token == "LBB", ]
  replace_colnum_square1 <- fpd[fpd$pos_id > square_loc_colnum$pos_id & fpd$parent == node_id & fpd$token == "expr", ]$text
  replace_colnum_square2 <- fpd[fpd$parent == node_id & fpd$pos_id > square_loc_colnum$pos_id & fpd$token == "NUM_CONST", ]$text
  if(length(replace_colnum_square1) == 0)
    return (replace_colnum_square2)
  return (replace_colnum_square1)
}

# Executes searching operations to find single square brackets.
#
# @param fpd A flatten parsed data data.frame.
# @param node_id Id of the node to be examined
#
has_single_bracket <- function(fpd, node_id)
{
  test_single_bracket <- fpd[fpd$parent == node_id, ]
  single_bracket <- match("'['", test_single_bracket$token)
  single_bracket_flag <- (!(is.na(single_bracket)))
  return (single_bracket_flag)
}


# Executes searching operations related to single square bracket to find dataset.
#
# @param fpd A flatten parsed data data.frame.
# @param node_id Id of the node to be examined
#
single_dataset <- function(fpd, node_id)
{
  single_loc <- fpd[fpd$parent == node_id & fpd$token == "'['", ]
  replace_df_single <- fpd[fpd$pos_id < single_loc$pos_id & fpd$parent == node_id & fpd$token == "SYMBOL", ]$text
  return (replace_df_single)
}


# Executes searching operations related to single square brackets to find column number.
#
# @param fpd A flatten parsed data data.frame.
# @param node_id Id of the node to be examined
#
single_colnum <- function(fpd, node_id)
{
  single_loc_colnum <- fpd[fpd$parent == node_id & fpd$token == "'['", ]
  replace_colnum_dollar <- fpd[fpd$pos_id > single_loc_colnum$pos_id & fpd$parent == node_id & fpd$token == "NUM_CONST", ]$text
  return (replace_colnum_dollar)
}


# Executes efficient column selection/extraction of a fpd.
#
# @param fpd A flatten parsed data data.frame.
#
ce_one_fpd <- function(fpd)
{
  comments_df <- fpd[fpd$parent < 0, ]
  fpd <- fpd[fpd$parent >= 0, ]
  exam_nodes <- get_roots(fpd)
  replace_text <- NULL
  i <- 1
  final_exam_nodes <- NULL
  data_frame <- NULL
  column_name <- NULL
  not_to_edit <- NULL
  for(i in seq_len(nrow(exam_nodes)))
  {
    test_id <- exam_nodes[i, ]$id
    if(has_dollar_sign(fpd, test_id) == TRUE)
    {
      data_frame <- append(data_frame, dollar_dataset(fpd, test_id))
      column_name <- append(column_name, dollar_colnum(fpd, test_id))
      final_exam_nodes <- rbind(final_exam_nodes, exam_nodes[i, ])
    }
    else if(has_square_brackets(fpd, test_id) == TRUE)
    {
      data_frame <- append(data_frame, square_dataset(fpd, test_id))
      column_name <- append(column_name, square_colnum(fpd, test_id))
      final_exam_nodes <- rbind(final_exam_nodes, exam_nodes[i, ])
    }
    else if(has_single_bracket(fpd, test_id) == TRUE)
    {
      data_frame <- append(data_frame, single_dataset(fpd, test_id))
      column_name <- append(column_name, single_colnum(fpd, test_id))
      final_exam_nodes <- rbind(final_exam_nodes, exam_nodes[i, ])
    }
    else
    {
      not_to_edit <- rbind(not_to_edit, exam_nodes[i, ])
    }
  }
  
  
  j <- 1
  for(j in seq_len(nrow(final_exam_nodes))) #Replacing column extraction methods with .subset2
  {
    final_exam_nodes[j, ]$text <- sprintf(".subset2(%s, %s)", data_frame[j], column_name[j])
  }
  
  #Curating the fpd that was not to be edited.
  not_to_edit <- rbind(not_to_edit, comments_df)
  k <- 1
  for(k in seq_len(nrow(not_to_edit)))
  {
    not_to_edit <- rbind(not_to_edit, fpd[fpd$parent == not_to_edit[k, ]$id, ])
  }
  not_to_edit <- not_to_edit[order(not_to_edit$pos_id), ]
  
  
  #Transforming the final_exam_nodes array so it may become compatible with not_to_edit
  itr <- 1
  new_fpd <- NULL
  
  for(itr in seq_len(nrow(final_exam_nodes)))
  {
    act_fpd <- final_exam_nodes[itr, ]
    new_act_fpd <- flatten_leaves(parse_text(act_fpd$text))
    
    #Backing up the original new_act_fpd
    new_act_fpd_duplicate <- new_act_fpd
    
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
  
  #Merging the new_fpd(derived from final_exam_nodes) with the not_to_edit data.frame into resultant_fpd
  
  resultant_fpd <- rbind(not_to_edit, new_fpd[order(new_fpd$pos_id), ])
  resultant_fpd <- resultant_fpd[order(resultant_fpd$pos_id), ]
  resultant_fpd <- flatten_leaves(resultant_fpd)
  
  #Identifying the lines where the new line character has to be introduced.
  next_arr <- NULL
  next_arr <- resultant_fpd[!resultant_fpd$terminal & resultant_fpd$parent == 0, ]
  next_arr <- rbind(next_arr, resultant_fpd[resultant_fpd$parent < 0, ])
  next_arr <- next_arr[order(next_arr$pos_id), ]
  
  #Results obtained in "optimized_text"
  target_id <- NULL
  resultant_fpd[is.na(resultant_fpd)] <- 0 #Replacing the NAs with zeroes.
  l <- 1
  for(l in seq_len(nrow(next_arr)-1))
  {
    target_id <- append(target_id, max(resultant_fpd[resultant_fpd$pos_id >= next_arr[l, ]$pos_id & resultant_fpd$pos_id < next_arr[l+1, ]$pos_id, ]$pos_id))
  }

  m <- 1
  for(m in seq_len(length(target_id)))
  {
    resultant_fpd[which(resultant_fpd$pos_id == target_id[m]), ]$next_lines <- 1
  }
  
  return (resultant_fpd)
}
