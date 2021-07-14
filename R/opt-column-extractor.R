#' Optimizer: Efficient Column Extraction.
#'
#' Performs optimisations for column extraction throughout the code.
#' Carefully examine the results after running this function!
#'
#' @param texts A list of character vectors with the code to optimize.
#' 
#' @examples
#' code <- paste (
#'   "points <- data.frame (x = rnorm (100), y = rnorm (100))",
#'   "points[ , 2]",
#'   "#This is a comment!!",
#'   "poimts [[2]]",
#'   "points$y", 
#'   "yo <- 1",
#'   "yo",
#'   "points[[c (2)]]",
#'   ".subset2 (mtcars, 2)",
#'    sep = "\n"
#' )
#' cat (opt_column_extractor (list (code))$codes[[1]])
#' @export
#'
opt_column_extractor <- function (texts) {
  res <- list ()
  res$codes <- lapply (texts, ce_one_file)
  res
}

#  Executes efficient column selection/extraction on one text of code.
#
#  @param texts A character vector with code to optimize.
#
ce_one_file <- function (texts){
  pd <- parse_text (texts)
  res_fpd <- ce_one_fpd (pd)
  deparse_data (res_fpd)
}

#  Executes searching operations to find dollar sign.
# 
#  @param fpd A flatten parsed data data.frame.
#  @param node_id Id of the node to be examined
#
has_dollar_sign <- function (fpd, node_id) {
  "'$'" %in% fpd[fpd$parent == node_id, ]$token
}


#  Executes searching operations related to dollar sign to find dataset.
#
#  @param fpd A flatten parsed data data.frame.
#  @param node_id Id of the node to be examined
#
dollar_dataset <- function (fpd, node_id) {
  open_id <- fpd[fpd$parent == node_id & fpd$token == "'$'", "pos_id"]
  fpd[fpd$parent == node_id & fpd$token == "SYMBOL" & 
        fpd$pos_id < open_id, "text"]
}


#  Executes searching operations related to dollar sign to find column number.
#
#  @param fpd A flatten parsed data data.frame.
#  @param node_id Id of the node to be examined
#
dollar_colname <- function (fpd, node_id) {
  open_id <- fpd[fpd$parent == node_id & fpd$token == "'$'", "pos_id"]
  fpd[fpd$parent == node_id & fpd$token == "SYMBOL" & 
        fpd$pos_id > open_id, ]$text
}

#  Executes searching operations to find double square brackets (LBB).
#
#  @param fpd A flatten parsed data data.frame.
#  @param node_id Id of the node to be examined
#
has_square_brackets <- function (fpd, node_id) {
  "LBB" %in% fpd[fpd$parent == node_id, ]$token
}

#  Executes searching operations related to double square brackets (LBB) to find dataset.
#
#  @param fpd A flatten parsed data data.frame.
#  @param node_id Id of the node to be examined
#
square_dataset <- function (fpd, node_id) {
  node_children <- fpd[fpd$parent == node_id, ]
  open_id <- node_children[node_children$token == "LBB", "pos_id"]
  node_children[node_children$token == "SYMBOL" & 
                   (node_children$pos_id < open_id), "text"]
}

#  Executes searching operations related to double square brackets (LBB) to find column number.
#
#  @param fpd A flatten parsed data data.frame.
#  @param node_id Id of the node to be examined
#
square_colnum <- function (fpd, node_id) {
  node_children <- fpd[fpd$parent == node_id, ]
  open_id <- node_children[node_children$token == "LBB", "pos_id"]
  close_id <- max(node_children[node_children$token == "']'", "pos_id"])  
  node_children[node_children$pos_id > open_id & node_children$pos_id < close_id
                &  (node_children$token == "NUM_CONST" | node_children$token == "expr"| 
                     node_children$token == "SYMBOL"), "text"]
}

#  Executes searching operations to find single square brackets.
#
#  @param fpd A flatten parsed data data.frame.
#  @param node_id Id of the node to be examined
#
has_single_bracket <- function (fpd, node_id) {
  "'['" %in% fpd[fpd$parent == node_id, ]$token
}


#  Executes searching operations related to single square bracket to find dataset.
#
#  @param fpd A flatten parsed data data.frame.
#  @param node_id Id of the node to be examined
#
single_dataset <- function (fpd, node_id) { 
  node_children <- fpd[fpd$parent == node_id, ]
  open_id <- node_children[node_children$token == "'['", "pos_id"]
  node_children[node_children$token == "SYMBOL" & node_children$pos_id < open_id, "text"]
}


#  Executes searching operations related to single square brackets to find column number.
#
#  @param fpd A flatten parsed data data.frame.
#  @param node_id Id of the node to be examined
#
single_colnum <- function (fpd, node_id) {  
  node_children <- fpd[fpd$parent == node_id, ]
  open_id <- node_children[node_children$token == "'['", "pos_id"]
  close_id <- node_children[node_children$token == "']'", "pos_id"]
  node_children[node_children$pos_id > open_id & node_children$pos_id < close_id
                &  (node_children$token == "NUM_CONST" | node_children$token == "expr"|
                     node_children$token == "SYMBOL"), "text"]
}


#  Executes efficient column selection/extraction of a fpd.
#
#  @param fpd A flatten parsed data data.frame.
#
ce_one_fpd <- function  (pd) {
  fpd <- flatten_leaves (pd)
  fun_exam_nodes <- NULL
  
  fun_ids <- fpd$id[fpd$token == "FUNCTION"]      
  fun_prnt_ids <- fpd$parent[fpd$id %in% fun_ids]
  for (i in fun_prnt_ids){
    fun_exam_nodes <- rbind (fun_exam_nodes, get_children (fpd, i, FALSE)[get_children (fpd, i, FALSE)$token == "expr", ])
  }
  exam_nodes <- get_roots (pd)
  exam_nodes <- rbind (exam_nodes, fun_exam_nodes)
  
  final_exam_nodes <- NULL
  not_to_edit <- NULL
  column_name <- c  (NA); length  (column_name) <- length (exam_nodes$id)
  data_frame <- c (NA); length (data_frame) <- length (exam_nodes$id)
  method_used <- c (NA); length  (method_used) <- length (exam_nodes$id)
  
  for  (i in seq_len (nrow (exam_nodes))) {
    test_id <- exam_nodes[i, ]$id
    if  (has_dollar_sign (fpd, test_id)) {
      data_frame <- append (data_frame, dollar_dataset (fpd, test_id))
      column_name <- append (column_name, dollar_colname (fpd, test_id))
      final_exam_nodes <- rbind (final_exam_nodes, exam_nodes[i, ])
      method_used <- append (method_used, "$")
    } else if  (has_square_brackets (fpd, test_id) ) {
      data_frame <- append (data_frame, square_dataset (fpd, test_id))
      column_name <- append (column_name, square_colnum (fpd, test_id))
      final_exam_nodes <- rbind (final_exam_nodes, exam_nodes[i, ])
      method_used <- append (method_used, "LBB")  
    } else if  (has_single_bracket (fpd, test_id) ) {
      data_frame <- append (data_frame, single_dataset (fpd, test_id))
      column_name <- append (column_name, single_colnum (fpd, test_id))
      final_exam_nodes <- rbind (final_exam_nodes, exam_nodes[i, ])
      method_used <- append (method_used, "[")
    } else {
      not_to_edit <- rbind (not_to_edit, exam_nodes[i, ])
    }
  }
  
  column_name <- column_name[!is.na (column_name)]
  data_frame <- data_frame[!is.na (data_frame)]
  method_used <- method_used[!is.na (method_used)]
  
  for  (i in seq_len (nrow (final_exam_nodes))) {
    if  (! (method_used[i] == "$")) {
      #Replacing column extraction methods with .subset2
      final_exam_nodes[i, "text"] <- sprintf (".subset2 (%s, %s)", data_frame[i], column_name[i])
    }else {
      #What we are trying to do here: .subset2 (mtcars, which (colnames (mtcars) == "mpg"))
      final_exam_nodes[i, "text"] <- sprintf (".subset2 (%s, which (colnames (%s) == \"%s\"))", data_frame[i], data_frame[i], column_name[i]) 
    }
  }
  
  for (i in seq_len (nrow (not_to_edit))) {
    not_to_edit <- rbind (not_to_edit, get_children (pd, not_to_edit[i, "id"]))
  }
  
  not_to_edit <- not_to_edit[order (not_to_edit$pos_id), ]
  #Removing entries with duplicate IDs.
  not_to_edit_final <- NULL
  for (i in unique (not_to_edit$id)) {
    not_to_edit_final <- rbind (not_to_edit_final, unique (not_to_edit[not_to_edit$id == i, ]))
  }
  #These ids will be used for correction of new_lines after the substitution
  final_exam_nodes_ids <- final_exam_nodes$id
  
  new_fpd <- NULL
  for (itr in seq_len (nrow (final_exam_nodes)))
  {
    act_fpd <- final_exam_nodes[itr, ]
    new_act_fpd <- parse_text (act_fpd$text)
    
    #Setting new ids for the newly edited and parsed codes
    new_act_fpd$id <- paste0 (act_fpd$id, "_", new_act_fpd$id)
    
    #Keeping old parents for new fpd
    new_act_fpd$parent[new_act_fpd$parent != 0] <- paste0 (act_fpd$id, "_", 
                                                           new_act_fpd$parent[new_act_fpd$parent != 0])
    new_act_fpd$parent[new_act_fpd$parent == 0] <- act_fpd$parent
    
    #Calling a pre-wriiten rco::function....
    new_act_fpd$pos_id <- create_new_pos_id (act_fpd, nrow (new_act_fpd), act_fpd$id)
    
    #Fixing the next_spaces section of new_fpd
    new_act_fpd$next_spaces[nrow (new_act_fpd)] <- act_fpd$next_spaces
    
    #Fixing the next_lines section of new_fpd
    new_act_fpd$next_lines[nrow (new_act_fpd)] <- act_fpd$next_lines
    
    #Fixing the prev_spaces section of new_fpd
    new_act_fpd$prev_spaces[which (new_act_fpd$terminal)[[1]]] <- act_fpd$prev_spaces
    
    #Merging the new_fpd and the act_fpd (obtained upon iteration)
    new_fpd <- rbind (new_fpd, new_act_fpd)
    
    #Ordering the new_fpd according to the pos_id
    new_fpd <- new_fpd[order (new_fpd$pos_id), ]
  }
  
  resultant_fpd <- rbind (not_to_edit_final, new_fpd)
  resultant_fpd <- resultant_fpd[order (resultant_fpd$pos_id), ]
  
  test_fpd <- NULL
  test_fpd <- flatten_leaves (resultant_fpd)
  
  comments_ids <- NULL
  curr_new_line_ids <- NULL
  next_line_ids <- NULL
  correction_nodes <- NULL
  correction_ids <- NULL
  correction_nodes <- test_fpd[test_fpd$parent == 0 & is.na (test_fpd$next_spaces)
                               & is.na (test_fpd$next_lines) & is.na (test_fpd$prev_spaces), ]
  
  for (i in correction_nodes$id){
    correction_ids <- append (correction_ids, which (test_fpd$id == i, arr.ind = TRUE))
  }
  
  test_fpd[which (is.na (test_fpd$next_lines)), "next_lines"] <- 0
  test_fpd[which (is.na (test_fpd$next_spaces)), "next_spaces"] <- 0
  test_fpd[which (is.na (test_fpd$prev_spaces)), "prev_spaces"] <- 0
  
  comments_ids <- which (test_fpd$parent < 0)
  correction_ids <- append (correction_ids, comments_ids)
  for (i in correction_ids){
    if ( (i-1) > 0){
      test_fpd[i-1, "next_lines"] <- 1
    }else{
      next
    }
  }
  test_fpd <- test_fpd[! (test_fpd$parent == 0 & test_fpd$terminal == FALSE), ]
  deletion_nodes <- NULL
  for (i in final_exam_nodes_ids){
    if (i %in% test_fpd$id){
      deletion_nodes <- rbind (deletion_nodes, get_children (test_fpd, i))
    }
  }
  next_line_ids <- as.double (test_fpd[test_fpd$id %in% deletion_nodes$id
                                      & test_fpd$next_lines == 1, "pos_id"])
  test_fpd <- test_fpd[! (test_fpd$id %in% deletion_nodes$id), ]
  for (i in next_line_ids){
    curr_new_line_ids <- append (curr_new_line_ids, 
                                max (which (test_fpd$pos_id < i)))  
  }
  for (i in curr_new_line_ids) {
    test_fpd[i, ]$next_lines <- 1
  }
  
  test_fpd
}



