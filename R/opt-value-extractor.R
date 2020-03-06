#' Optimizer: Efficient Value Extraction.
#'
#' Performs optimisations for value extraction throughout the code.
#' Carefully examine the results after running this function!
#'
#' @param code A list of character vectors with the code to optimize.
#' 
#' @examples
#' code <- paste(
#' "mtcars[32, 11]",
#' "#This is a comment!!",
#' "mtcars [[11]][32]",
#' "mtcars$carb[32]",
#' "yo <- 1",
#' "yo",
#' "mtcars[[c(11,32)]]",
#' ".subset2(mtcars, 11)[32]",
#'  sep = "\n"
#'  )
#' cat(opt_value_extractor(list(code))$codes[[1]])
#' @export
opt_value_extractor <- function(code) {
  res <- list()
  res$codes <- lapply(code, ve_one_file)
  return (res)
}

# Executes efficient value selection/extraction on one text of code.
#
# @param code A character vector with code to optimize.
#
ve_one_file <- function(code)
{
  flatten_pd <- parse_text(code)
  flatten_pd <- flatten_leaves(flatten_pd)
  result_flatten_pd <- ve_one_flatten_pd(flatten_pd)
  return (deparse_data(result_flatten_pd))
}


# Executes searching operations to find dollar sign.
# @param flatten_pd A flatten parsed data data.frame.
# @param nodal_id Id of the node to be examined
#
Has_dollar_sign <- function(flatten_pd, nodal_id)
{
  father <- nodal_id
  mother <- flatten_pd[flatten_pd$parent == nodal_id &is.na(flatten_pd$next_lines) & is.na(flatten_pd$next_spaces) & is.na(flatten_pd$prev_spaces), ]$id
  Test_dollar_df <- flatten_pd[flatten_pd$parent == father | flatten_pd$parent == mother, ]
  Dollar <- match("'$'", Test_dollar_df$token)
  Dollar_flag <- (!(is.na(Dollar)))
  return (Dollar_flag)
}

# Executes searching operations related to dollar sign to find dataset.
#
# @param flatten_pd A flatten parsed data data.frame.
# @param nodal_id Id of the node to be examined
#
Dollar_dataset <- function(flatten_pd, nodal_id)
{
  father <- nodal_id
  mother <- flatten_pd[flatten_pd$parent == nodal_id &is.na(flatten_pd$next_lines) & is.na(flatten_pd$next_spaces) & is.na(flatten_pd$prev_spaces), ]$id
  Dollar_loc <- flatten_pd[(flatten_pd$parent == father | flatten_pd$parent == mother) & flatten_pd$token == "'$'", ]
  Replace_df_dollar <- flatten_pd[flatten_pd$pos_id < Dollar_loc$pos_id & (flatten_pd$parent == father | flatten_pd$parent == mother) & flatten_pd$token == "SYMBOL", ]$text
  return (Replace_df_dollar)
}

# Executes searching operations related to dollar sign to find column number.
#
# @param flatten_pd A flatten parsed data data.frame.
# @param nodal_id Id of the node to be examined
#
Dollar_colnum <- function(flatten_pd, nodal_id)
{
  father <- nodal_id
  mother <- flatten_pd[flatten_pd$parent == nodal_id &is.na(flatten_pd$next_lines) & is.na(flatten_pd$next_spaces) & is.na(flatten_pd$prev_spaces), ]$id
  Dollar_loc_colnum <- flatten_pd[(flatten_pd$parent == father | flatten_pd$parent == mother) & flatten_pd$token == "'$'", ]
  Replace_colname_dollar <- flatten_pd[flatten_pd$pos_id > Dollar_loc_colnum$pos_id & (flatten_pd$parent == father | flatten_pd$parent == mother) & flatten_pd$token == "SYMBOL", ]$text
  return (which(colnames(mtcars) == Replace_colname_dollar[1]))
}


# Executes searching operations related to dollar sign to find row number.
#
# @param flatten_pd A flatten parsed data data.frame.
# @param nodal_id Id of the node to be examined
#
Dollar_rownum <- function(flatten_pd, nodal_id)
{
  father <- nodal_id
  mother <- flatten_pd[flatten_pd$parent == nodal_id &is.na(flatten_pd$next_lines) & is.na(flatten_pd$next_spaces) & is.na(flatten_pd$prev_spaces), ]$id
  Replace_rowname_dollar <- flatten_pd[(flatten_pd$parent == father | flatten_pd$parent == mother) & flatten_pd$token == "NUM_CONST", ]$text
  return (as.numeric(Replace_rowname_dollar))
}

# Executes searching operations related to double square brackets(LBB) to find dataset.
#
# @param flatten_pd A flatten parsed data data.frame.
# @param nodal_id Id of the node to be examined
#
Has_square_brackets <- function(flatten_pd, nodal_id)
{
  father <- nodal_id
  mother <- flatten_pd[flatten_pd$parent == nodal_id &is.na(flatten_pd$next_lines) & is.na(flatten_pd$next_spaces) & is.na(flatten_pd$prev_spaces), ]$id
  Test_square <- flatten_pd[flatten_pd$parent == father | flatten_pd$parent == mother, ]
  Square <- match("LBB", Test_square$token)
  Square_flag <- (!(is.na(Square)))
  return (Square_flag)
}

# Executes searching operations related to double square brackets(LBB) to find dataset.
#
# @param flatten_pd A flatten parsed data data.frame.
# @param nodal_id Id of the node to be examined
#
Square_dataset <- function(flatten_pd, nodal_id)
{
  father <- nodal_id
  mother <- flatten_pd[flatten_pd$parent == nodal_id &is.na(flatten_pd$next_lines) & is.na(flatten_pd$next_spaces) & is.na(flatten_pd$prev_spaces), ]$id
  Square_loc <- flatten_pd[(flatten_pd$parent == father | flatten_pd$parent == mother) & flatten_pd$token == "LBB", ]
  Replace_df_square <- flatten_pd[flatten_pd$pos_id < Square_loc$pos_id & (flatten_pd$parent == father | flatten_pd$parent == mother) & flatten_pd$token == "SYMBOL", ]$text
  return (Replace_df_square)
}

# Executes searching operations related to double square brackets(LBB) to find column number.
#
# @param flatten_pd A flatten parsed data data.frame.
# @param nodal_id Id of the node to be examined
#
Square_colnum <- function(flatten_pd, nodal_id)
{
  father <- nodal_id
  mother <- flatten_pd[flatten_pd$parent == nodal_id &is.na(flatten_pd$next_lines) & is.na(flatten_pd$next_spaces) & is.na(flatten_pd$prev_spaces), ]$id
  Square_loc_colnum <- flatten_pd[(flatten_pd$parent == father | flatten_pd$parent == mother) & flatten_pd$token == "LBB", ]
  Replace_colnum_square <- flatten_pd[flatten_pd$pos_id > Square_loc_colnum$pos_id & (flatten_pd$parent == father | flatten_pd$parent == mother) & flatten_pd$token == "NUM_CONST", ]$text
  return (Replace_colnum_square[1])
}

# Executes searching operations related to double square brackets(LBB) to find row number.
#
# @param flatten_pd A flatten parsed data data.frame.
# @param nodal_id Id of the node to be examined
#
Square_rownum <- function(flatten_pd, nodal_id)
{
  father <- nodal_id
  mother <- flatten_pd[flatten_pd$parent == nodal_id &is.na(flatten_pd$next_lines) & is.na(flatten_pd$next_spaces) & is.na(flatten_pd$prev_spaces), ]$id
  Square_loc_colnum <- flatten_pd[(flatten_pd$parent == father | flatten_pd$parent == mother) & flatten_pd$token == "LBB", ]
  Replace_colnum_square <- flatten_pd[flatten_pd$pos_id > Square_loc_colnum$pos_id & (flatten_pd$parent == father | flatten_pd$parent == mother) & flatten_pd$token == "NUM_CONST", ]$text
  return (Replace_colnum_square[2])
}

# Executes searching operations to find single square brackets.
#
# @param flatten_pd A flatten parsed data data.frame.
# @param nodal_id Id of the node to be examined
#
Has_single_bracket <- function(flatten_pd, nodal_id)
{
  father <- nodal_id
  mother <- flatten_pd[flatten_pd$parent == nodal_id &is.na(flatten_pd$next_lines) & is.na(flatten_pd$next_spaces) & is.na(flatten_pd$prev_spaces), ]$id
  Test_single_bracket <- flatten_pd[flatten_pd$parent == father, ]
  Single_bracket <- match("'['", Test_single_bracket$token)
  Single_bracket_flag <- (!(is.na(Single_bracket)))
  Single_bracket_flag1 <- TRUE
  if(length(mother) > 0L)
    Single_bracket_flag1 <- FALSE
  return (Single_bracket_flag & Single_bracket_flag1)
}

# Executes searching operations related to single square bracket to find dataset.
#
# @param flatten_pd A flatten parsed data data.frame.
# @param nodal_id Id of the node to be examined
#
Single_dataset <- function(flatten_pd, nodal_id)
{
  Single_loc <- flatten_pd[flatten_pd$parent == nodal_id & flatten_pd$token == "'['", ]
  Replace_df_single <- flatten_pd[flatten_pd$pos_id < Single_loc$pos_id & flatten_pd$parent == nodal_id & flatten_pd$token == "SYMBOL", ]$text
  return (Replace_df_single)
}

# Executes searching operations related to single square brackets to find column number.
#
# @param flatten_pd A flatten parsed data data.frame.
# @param nodal_id Id of the node to be examined
#
Single_colnum <- function(flatten_pd, nodal_id)
{
  Single_loc_colnum <- flatten_pd[flatten_pd$parent == nodal_id & flatten_pd$token == "'['", ]
  Replace_colnum_dollar <- flatten_pd[flatten_pd$pos_id > Single_loc_colnum$pos_id & flatten_pd$parent == nodal_id & flatten_pd$token == "NUM_CONST", ]$text
  return (Replace_colnum_dollar[2])
}

# Executes searching operations related to single square brackets to find row number.
#
# @param flatten_pd A flatten parsed data data.frame.
# @param nodal_id Id of the node to be examined
#
Single_rownum <- function(flatten_pd, nodal_id)
{
  Single_loc_colnum <- flatten_pd[flatten_pd$parent == nodal_id & flatten_pd$token == "'['", ]
  Replace_colnum_dollar <- flatten_pd[flatten_pd$pos_id > Single_loc_colnum$pos_id & flatten_pd$parent == nodal_id & flatten_pd$token == "NUM_CONST", ]$text
  return (Replace_colnum_dollar[1])
}

# Executes efficient value selection/extraction of a flatten_pd.
#
# @param flatten_pd A flatten parsed data data.frame.
#
ve_one_flatten_pd <- function(flatten_pd)
{
  Comment_df <- flatten_pd[flatten_pd$parent < 0, ] 
  flatten_pd <- flatten_pd[flatten_pd$parent >= 0, ] 
  examination_nodes <- get_roots(flatten_pd)
  i <- 1
  final_examination_nodes <- NULL
  Data_frame <- NULL
  Name_Column <- NULL
  Name_Row <- NULL
  no_edit_pd <- NULL
  for(i in seq_len(nrow(examination_nodes)))
  {
    Test_id <- examination_nodes[i, ]$id
    if(Has_dollar_sign(flatten_pd, Test_id) == TRUE)
    {
      Data_frame <- append(Data_frame, Dollar_dataset(flatten_pd, Test_id))
      Name_Column <- append(Name_Column, Dollar_colnum(flatten_pd, Test_id))
      Name_Row <- append(Name_Row, Dollar_rownum(flatten_pd, Test_id))
      final_examination_nodes <- rbind(final_examination_nodes, examination_nodes[i, ])
    }
    else if(Has_square_brackets(flatten_pd, Test_id) == TRUE)
    {
      Data_frame <- append(Data_frame, Square_dataset(flatten_pd, Test_id))
      Name_Column <- append(Name_Column, Square_colnum(flatten_pd, Test_id))
      Name_Row <- append(Name_Row, Square_rownum(flatten_pd, Test_id))
      final_examination_nodes <- rbind(final_examination_nodes, examination_nodes[i, ])
    }
    else if(Has_single_bracket(flatten_pd, Test_id) == TRUE)
    {
      Data_frame <- append(Data_frame, Single_dataset(flatten_pd, Test_id))
      Name_Column <- append(Name_Column, Single_colnum(flatten_pd, Test_id))
      Name_Row <- append(Name_Row, Single_rownum(flatten_pd, Test_id))
      final_examination_nodes <- rbind(final_examination_nodes, examination_nodes[i, ])
    }
    else
    {
      no_edit_pd <- rbind(no_edit_pd, examination_nodes[i, ])
    }
  }
  
  j <- 1
  for(j in seq_len(nrow(final_examination_nodes)))
  {
    final_examination_nodes[j, ]$text <- sprintf(".subset2(%s, %s)[%s]", Data_frame[j], Name_Column[j], Name_Row[j])
  }
  
  no_edit_pd <- rbind(no_edit_pd, Comment_df)
  k <- 1
  for(k in seq_len(nrow(no_edit_pd)))
  {
    father <- no_edit_pd[k, ]$id
    mother <- flatten_pd[flatten_pd$parent == no_edit_pd[k, ]$id &is.na(flatten_pd$next_lines) & is.na(flatten_pd$next_spaces) & is.na(flatten_pd$prev_spaces), ]$id
    if(length(mother) > 0)
      no_edit_pd <- rbind(no_edit_pd, flatten_pd[(flatten_pd$parent == father | flatten_pd$parent == mother), ])
    no_edit_pd <- rbind(no_edit_pd, flatten_pd[flatten_pd$parent == father, ])
  }
  
  no_edit_pd <- no_edit_pd[order(no_edit_pd$pos_id), ]
  
  itr <- 1
  new_flatten_pd <- NULL
  
  for(itr in seq_len(nrow(final_examination_nodes)))
  {
    act_flatten_pd <- final_examination_nodes[itr, ]
    new_act_flatten_pd <- flatten_leaves(parse_text(act_flatten_pd$text))
    
    #Backing up the original new_act_flatten_pd
    new_act_flatten_pd_duplicate <- new_act_flatten_pd
    
    #Setting new ids for the newly edited and parsed codes
    new_act_flatten_pd$id <- paste0(act_flatten_pd$id, "_", new_act_flatten_pd$id)
    
    #Keeping old parents for new flatten_pd
    new_act_flatten_pd$parent[new_act_flatten_pd$parent != 0] <- paste0(act_flatten_pd$id, "_", new_act_flatten_pd$parent[new_act_flatten_pd$parent != 0])
    new_act_flatten_pd$parent[new_act_flatten_pd$parent == 0] <- act_flatten_pd$parent
    
    #Calling a pre-wriiten rco::function....
    new_act_flatten_pd$pos_id <- create_new_pos_id(act_flatten_pd, nrow(new_act_flatten_pd), act_flatten_pd$id)
    
    #Fixing the next_spaces section of new_flatten_pd
    new_act_flatten_pd$next_spaces[nrow(new_act_flatten_pd)] <- act_flatten_pd$next_spaces
    
    #Fixing the next_lines section of new_flatten_pd
    new_act_flatten_pd$next_lines[nrow(new_act_flatten_pd)] <- act_flatten_pd$next_lines
    
    #Fixing the prev_spaces section of new_flatten_pd
    new_act_flatten_pd$prev_spaces[which(new_act_flatten_pd$terminal)[[1]]] <- act_flatten_pd$prev_spaces
    
    #Merging the new_flatten_pd and the act_flatten_pd(obtained upon iteration)
    new_flatten_pd <- rbind(new_flatten_pd, new_act_flatten_pd)
    
    #Ordering the new_flatten_pd according to the pos_id
    new_flatten_pd <- new_flatten_pd[order(new_flatten_pd$pos_id), ]
  }
  
  resultant_flatten_pd <- rbind(no_edit_pd, new_flatten_pd[order(new_flatten_pd$pos_id), ])
  resultant_flatten_pd <- resultant_flatten_pd[order(resultant_flatten_pd$pos_id), ]
  
  #Identifying the lines where the new line character has to be introduced.
  next_lines_corpus <- NULL
  next_lines_corpus <- resultant_flatten_pd[resultant_flatten_pd$parent == 0, ]
  next_lines_corpus <- rbind(next_lines_corpus, resultant_flatten_pd[resultant_flatten_pd$parent < 0, ])
  next_lines_corpus <- next_lines_corpus[order(next_lines_corpus$pos_id), ]
  
  targeted_id <- NULL
  resultant_flatten_pd[is.na(resultant_flatten_pd)] <- 0 #Replacing the NAs with zeroes.
  l <- 1
  for(l in seq_len(nrow(next_lines_corpus)-1))
  {
    targeted_id <- append(targeted_id, max(resultant_flatten_pd[resultant_flatten_pd$pos_id >= next_lines_corpus[l, ]$pos_id & resultant_flatten_pd$pos_id < next_lines_corpus[l+1, ]$pos_id, ]$pos_id))
  }
  
  m <- 1
  for(m in seq_len(length(targeted_id)))
  {
    resultant_flatten_pd[which(resultant_flatten_pd$pos_id == targeted_id[m]), ]$next_lines <- 1
  }
  
  resultant_flatten_pd <-resultant_flatten_pd[!duplicated(resultant_flatten_pd), ]
  
  return (resultant_flatten_pd)
}
