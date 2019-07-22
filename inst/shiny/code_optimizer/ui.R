library("shiny")

shinyUI(fluidPage(
  theme = shinytheme("simplex"),
  fluidPage(
    titlePanel("rco - The R Code Optimizer"),
    wellPanel(
      fluidRow(
        selectInput("opt_list",
          label = "Otimizers", multiple = TRUE,
          choices = names(.optimizers),
          selected = names(.optimizers)
        )
      ),
      fluidRow(
        column(
          6,
          actionButton("opt_btn", label = "Optimize"),
          actionButton("load_ex_code_btn", label = "Load Example")
        ),
        column(
          6,
          "Changes:",
          actionButton("accept_changes_btn", label = "Accept"),
          actionButton("dismiss_changes_btn", label = "Dismiss")
        )
      ),
      fluidRow(
        column(6, textAreaInput("input_code",
          label = "",
          placeholder = "Input Code",
          height = "100%",
          rows = 20
        )),
        column(6, textAreaInput("opt_code",
          label = "",
          placeholder = "Optimized Code",
          height = "100%",
          rows = 20
        ))
      )
    )
  )
))
