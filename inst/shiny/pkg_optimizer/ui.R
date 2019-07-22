library("shiny")

source("global.R", local = TRUE)
source("package_selector/ui.R", local = TRUE)

shinyUI(fluidPage(
  theme = shinythemes::shinytheme("simplex"),
  fluidPage(
    titlePanel("rco - The R Code Optimizer"),
    tabsetPanel(
      tabPanel("Package", package_selector())
    )
  )
))
