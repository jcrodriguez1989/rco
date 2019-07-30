library("shiny")

shinyUI(fluidPage(
  theme = shinythemes::shinytheme("journal"),
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
        textAreaInput("input_code",
          label = "",
          placeholder = "Input Code",
          height = "100%",
          rows = 12
        )
      ),
      fluidRow(
        column(5, NULL),
        column(
          4,
          actionButton("load_ex_code_btn", label = "Load Example"),
          actionButton("opt_btn", label = "Optimize")
        )
      ),
      fluidRow(diffr::diffrOutput("opt_code"))
    )
  )
))
