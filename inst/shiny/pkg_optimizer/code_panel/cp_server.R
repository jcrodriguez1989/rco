fill_code_panel <- function(input, output, session, opt_pkg_list) {
  updateSelectInput(session, "file_sel", choices = opt_pkg_list$opt_files)
}

fill_sel_file <- function(input, output, session, opt_pkg_list) {
  output$code_diff <- diffr::renderDiffr(diffr::diffr(
    before = "Original:",
    file1 = paste0(opt_pkg_list$pkg_dir, "/R/", input$file_sel),
    after = "Optimized:",
    file2 = paste0(opt_pkg_list$opt_pkg_dir, "/R/", input$file_sel)
  ))
}
