library(shiny)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("ACF & White Noise"),
    tabPanel("Transformations"),
    tabPanel("Seasonal Adjustment")
  )
)

server <- function(input, output, session) {

}

shinyApp(ui, server)
