library("shiny")

source("package_selector/ps_server.R", local = TRUE)
source("code_panel/cp_server.R", local = TRUE)

shinyServer(function(input, output, session) {
  pkg <- reactiveVal(list())

  ############ Package loader panel

  observeEvent(input$optimize, {
    showModal(modalDialog(
      "Meanwhile, enjoy this random gif:", br(), br(),
      HTML(.random_gif()),
      title = "Optimization in progress ...",
      size = "m",
      footer = NULL,
      easyClose = FALSE
    ))
    progress <- Progress$new(session, min=1, max=4)
    on.exit(progress$close())
    progress$set(message = "Downloading package", value = 2,
                 detail = "This may take a while...")

    pkg_dir <- download_pkg(input$pkg)
    if (inherits(pkg_dir, "try-error")) {
      showNotification("Could not download package", type = "error")
      removeModal()
      return()
    }

    opt_pkg_dir <- paste0(pkg_dir, "_opt")
    dir.create(opt_pkg_dir)
    invisible(
      file.copy(dir(pkg_dir, full.names = TRUE), opt_pkg_dir, recursive = TRUE)
    )
    opt_files <- dir(paste0(opt_pkg_dir, "/R"), full.names = TRUE)

    sel_optimizers <- rco::all_optimizers[input$opt_list]
    progress$set(message = "Optimizing package", value = 3,
                 detail = "This may take a while...")
    opt_res <- rco::optimize_files(opt_files,
      overwrite = TRUE,
      optimizers = sel_optimizers
    )

    updateTabsetPanel(session, "main_tabset", "Code")
    opt_files <- opt_files[opt_res]
    opt_files <- gsub(".*/", "", opt_files)

    pkg(list(
      pkg_dir = pkg_dir, opt_pkg_dir = opt_pkg_dir,
      opt_files = opt_files
    ))
    removeModal()
  })

  ############ Optimized code panel

  observeEvent(pkg(), fill_code_panel(input, output, session, pkg()))
  observeEvent(input$file_sel, fill_sel_file(input, output, session, pkg()))
})
