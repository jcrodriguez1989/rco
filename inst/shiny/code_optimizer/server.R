library("shiny")

shinyServer(function(input, output, session) {
  observeEvent(input$opt_btn, {
    current_opt_names <- input$opt_list
    if (length(current_opt_names) == 0) {
      showNotification("Select at least one optimizer", type = "error")
      return()
    }
    # get the first optimizer
    current_opt <- .optimizers[current_opt_names[[1]]]
    showModal(modalDialog(
      "Meanwhile, enjoy this random gif:", br(), br(),
      HTML(.random_gif()),
      title = "Optimization in progress ...",
      size = "m",
      footer = NULL,
      easyClose = FALSE))
    opt_code <- try ({
      optimize_text(input$input_code, optimizers = current_opt)
    })
    removeModal()
    if (inherits(opt_code, "try-error")) {
      showNotification(
        paste0(
          "Error while trying to optimize: ",
          opt_code
        ),
        type = "error"
      )
      return()
    }
    if (input$input_code == opt_code) {
      showNotification(
        paste0(
          current_opt_names[[1]],
          " optimizer could not optimize code"
        ),
        type = "message"
      )
    }

    # update with the optimized code
    updateTextAreaInput(session, "opt_code", value = opt_code)
    # put last the used optimizer
    current_opt_names <- c(current_opt_names[-1], current_opt_names[1])
    updateSelectInput(session, "opt_list", selected = current_opt_names)
  })

  observeEvent(input$accept_changes_btn, {
    opt_code <- input$opt_code
    updateTextAreaInput(session, "input_code", value = opt_code)
    updateTextAreaInput(session, "opt_code", value = "")
  })

  observeEvent(input$dismiss_changes_btn, {
    updateTextAreaInput(session, "opt_code", value = "")
  })

  observeEvent(input$load_ex_code_btn, {
    example_code <- paste(
      "x <- 28",
      "y <- 7 - x / 4",
      "res <- y * (28 / x + 2)",
      "if (res > 0) {",
      '  print("Greater than zero!!")',
      "} else if (res == 0) {",
      '  print("Equal to zero!!")',
      "} else {",
      '  print("Less than zero!!")',
      "}",
      "",
      sep = "\n"
    )
    updateTextAreaInput(session, "input_code", value = example_code)
  })
})
