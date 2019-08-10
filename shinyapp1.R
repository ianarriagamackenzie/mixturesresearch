library(shiny)

ui = fluidPage(
  sliderInput(inputId = 'num',
  label = 'Choose a number',
  value = 10, min = 1, max = 1000),
  
  plotOutput('hist')
)

server = function(input, output) {
  output$hist = renderPlot({
    title = '100 randint'
    hist(rnorm(input$num), main = title)
  })
}

shinyApp(ui = ui, server = server)