library("shiny")

source("package_selector/server.R", local = TRUE)

shinyServer(function(input, output, session) {
  observeEvent(input$optimize, {
    pkg_dir <- download_pkg(input$pkg)
    if (pkg_dir == "") {
      showNotification("Could not download package", type = "error")
      return(NULL)
    }

    pkg_dir_opt <- paste0(pkg_dir, "_opt")
    dir.create(pkg_dir_opt)
    invisible(
      file.copy(dir(pkg_dir, full.names = TRUE), pkg_dir_opt, recursive = TRUE)
    )

    opt_files <- dir(paste0(pkg_dir_opt, "/R"), full.names = TRUE)

    sel_optimizers <- rco::all_optimizers[input$opt_list]
    opt_res <- rco::optimize_files(opt_files, overwrite = TRUE,
                                   optimizers = sel_optimizers)


    browser()
  })
})
