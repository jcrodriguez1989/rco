code_panel <- function() {
  wellPanel(
    fluidRow(
      column(2, selectInput("file_sel", "Optimized files:", list(),
        size = 15,
        multiple = FALSE, selectize = FALSE
      )),
      column(10, diffr::diffrOutput("code_diff"))
    )
  )
}
