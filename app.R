library(shiny)

ui <- fluidPage(
  sliderInput("n", "Observações", min = 10, max = 500, value = 100),
  plotOutput("grafico")
)

server <- function(input, output) {
  output$grafico <- renderPlot({
    hist(rnorm(input$n))
  })
}

shinyApp(ui, server)
