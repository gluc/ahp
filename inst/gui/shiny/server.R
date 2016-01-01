library(shiny)


shinyServer(function(input, output, session) {
  
  observeEvent(input$examples, {
    #print(input$examples)
    ahpFile <- system.file("extdata", input$examples, package="ahp")
    fileContent <- readChar(ahpFile, file.info(ahpFile)$size)
    updateAceEditor(session, "ace", value = fileContent)
  })
  
  observeEvent(input$calculate, {
    modelString <- input$ace
    print(modelString)
    ahpTree <- LoadString(modelString)
    Calculate(ahpTree)
    #print(GetDataFrame(ahpTree))
    decisionMakers <- ahp:::GetDecisionMakers(ahpTree)
    if(length(decisionMakers) > 1) {
      output$decisionMaker <- renderUI({
        radioButtons("decisionMaker", "Decision Maker: ", choices = c("Total", decisionMakers))
      })
    }
    output$table <- renderFormattable(ahp::ShowTable(ahpTree))
  })
  
  observeEvent(input$decisionMaker, {
    modelString <- input$ace
    print(modelString)
    ahpTree <- LoadString(modelString)
    Calculate(ahpTree)
    #print(GetDataFrame(ahpTree))
    output$table <- renderFormattable(ahp::ShowTable(ahpTree, input$decisionMaker))
  })
  
})