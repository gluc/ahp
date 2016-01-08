library(shiny)
library(shinyAce)

ui = shinyUI(grVizOutput("diagram"))

server = function(input, output){
  output$diagram <- renderGrViz({
    #browser()
    graph <- create_random_graph(5, 6)
    grViz(graph$dot_code)
  })
  
}

shinyApp(ui = ui, server = server)