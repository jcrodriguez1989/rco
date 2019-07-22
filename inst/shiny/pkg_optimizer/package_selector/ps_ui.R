package_selector <- function() {
  wellPanel(
    fluidRow(textInput("pkg", "Package:", "jcrodriguez1989/rco",
      placeholder = "CRAN package name, or github repository"
    )),
    fluidRow(selectInput("opt_list",
      label = "Otimizers", multiple = TRUE,
      choices = names(rco::all_optimizers),
      selected = names(rco::all_optimizers)
    )),
    fluidRow(
      column(5, br()),
      column(2, actionButton("optimize", "Optimize")),
      column(5, br())
    )
  )
}
