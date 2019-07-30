library("shiny")

source("global.R", local = TRUE)
source("package_selector/ps_ui.R", local = TRUE)
source("code_panel/cp_ui.R", local = TRUE)

shinyUI(fluidPage(
  theme = shinythemes::shinytheme("journal"),
  fluidPage(
    titlePanel("rco - The R Code Optimizer"),
    tabsetPanel(id = "main_tabset",
      tabPanel("Package", package_selector()),
      tabPanel("Code", code_panel())
    )
  )
))
