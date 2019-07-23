fill_code_panel <- function(input, output, session, opt_pkg_list) {
  new_choices <- list(
    "Optimized:" = opt_pkg_list$opt_files,
    "Not optimized:" = setdiff(
      dir(paste0(opt_pkg_list$pkg_dir, "/R")),
      opt_pkg_list$opt_files
    )
  )
  updateSelectInput(session, "file_sel", choices = new_choices)
}

fill_sel_file <- function(input, output, session, opt_pkg_list) {
  output$code_diff <- diffr::renderDiffr(diffr::diffr(
    before = "Original:",
    file1 = paste0(opt_pkg_list$pkg_dir, "/R/", input$file_sel),
    after = "Optimized:",
    file2 = paste0(opt_pkg_list$opt_pkg_dir, "/R/", input$file_sel)
  ))
}
