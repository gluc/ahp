library(shiny)

ahpTree <- NULL


DoCalculation <- function(input) {
  modelString <- input$ace
  print(modelString)
  ahpTree <<- LoadString(modelString)
  Calculate(ahpTree, GetMethod(input))
  return (ahpTree)
}

GetMethod <- function(input) {
  switch (input$ahpmethod,
          `Eigenvalues` = PrioritiesFromPairwiseMatrixEigenvalues,
          `Mean of Normalized Values` = PrioritiesFromPairwiseMatrixMeanNormalization,
          `Geometric Mean` = PrioritiesFromPairwiseMatrixGeometricMean
  )
}


shinyServer(function(input, output, session) {
  
  observeEvent(input$examples, {
    #print(input$examples)
    ahpFile <- system.file("extdata", input$examples, package="ahp")
    fileContent <- readChar(ahpFile, file.info(ahpFile)$size)
    updateAceEditor(session, "ace", value = fileContent)
  })
  
  observeEvent(input$navbar, {
    if (input$navbar == "analysis") {
      ahpTree <- DoCalculation(input)
      #print(GetDataFrame(ahpTree))
      decisionMakers <- ahp:::GetDecisionMakers(ahpTree)
      if(length(decisionMakers) > 1) {
        output$decisionMaker <- renderUI({
          radioButtons("decisionMaker", "Decision Maker: ", choices = c("Total", decisionMakers), selected = ifelse(is.null(input$decisionMaker), yes = "Total", no = input$decisionMaker))
        })
      } else {
        output$decisionMaker <- renderUI("")
      }
      output$table <- renderFormattable(ShowTable(ahpTree, decisionMaker =  ifelse(is.null(input$decisionMaker), yes = "Total", no = input$decisionMaker)))
    } else if(input$navbar == "AHP File Format") {
      output$fileFormat <- renderUI(fluidRow(column(6, includeMarkdown(system.file("doc", "file-format.Rmd", package="ahp")))))
    }
  })
  
  observeEvent(input$decisionMaker, {
    #browser()
    output$table <- renderFormattable(ShowTable(ahpTree, input$decisionMaker))
  })
  
  observeEvent(input$ahpmethod, {
    #browser()
    try(
      ahpTree <- DoCalculation(input)
    )
    output$table <- renderFormattable(ShowTable(ahpTree, ifelse(is.null(input$decisionMaker), yes = "Total", no = input$decisionMaker)))
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



