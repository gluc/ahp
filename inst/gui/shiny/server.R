library(shiny)

ahpTree <- NULL

shinyServer(function(input, output, session) {
  
  observeEvent(input$examples, {
    #print(input$examples)
    ahpFile <- system.file("extdata", input$examples, package="ahp")
    fileContent <- readChar(ahpFile, file.info(ahpFile)$size)
    updateAceEditor(session, "ace", value = fileContent)
  })
  
  observeEvent(input$navbar, {
    if (input$navbar == "analysis") {
      modelString <- input$ace
      print(modelString)
      ahpTree <<- LoadString(modelString)
      Calculate(ahpTree)
      #print(GetDataFrame(ahpTree))
      decisionMakers <- ahp:::GetDecisionMakers(ahpTree)
      if(length(decisionMakers) > 1) {
        output$decisionMaker <- renderUI({
          radioButtons("decisionMaker", "Decision Maker: ", choices = c("Total", decisionMakers))
        })
      } else {
        output$decisionMaker <- renderUI("")
      }
      output$table <- renderFormattable(ahp::ShowTable(ahpTree))
    }
  })
  
  observeEvent(input$decisionMaker, {
    #browser()
    output$table <- renderFormattable(ahp::ShowTable(ahpTree, input$decisionMaker))
  })
  
  output$downloadFile <- downloadHandler(
    
    filename = function() {
      modelString <- input$ace
      myAhpTree <- LoadString(modelString)
      nme <- paste0(gsub('[^a-zA-Z]', '', myAhpTree$name), ".ahp")
      return (nme)
    },
    content = function(file) {
      writeChar(input$ace, file)
    }
  )
  
  observeEvent(input$uploadFile, {
    fileContent <- readChar(input$uploadFile$datapath, file.info(input$uploadFile$datapath)$size)
    updateAceEditor(session, "ace", value = fileContent)
  })
  
  output$uploadFileOutput <- renderUI({
    input$uploadFile
    fileInput('uploadFile', NULL, width="80%")
  })
  
})