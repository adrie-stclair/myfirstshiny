library(shiny)
library(ggplot2)

ui <- fluidPage(
  sliderInput(
  "howmany", 
  "How many?",
  min = 0,
  max = 100, 
  step = 5,
  value = 15,
  animate = TRUE
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)
