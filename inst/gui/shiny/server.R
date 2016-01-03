library(shiny)

ahpTree <- NULL



#########################
## Helpers



DoCalculation <- function(input) {
  modelString <- input$ace
  #print(modelString)
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


## Helpers
#########################


shinyServer(function(input, output, session) {
  
  
  #####################
  ## Event Handlers
  
  # Navbar
  observeEvent(input$navbar, {
    print(paste0("event: navbar ", input$navbar))
    if (input$navbar == "analysis") {
      ahpTree <- DoCalculation(input)
      #print(GetDataFrame(ahpTree))
      decisionMakers <- ahp:::GetDecisionMakers(ahpTree)
      if(length(decisionMakers) > 1) {
        output$decisionMaker <- renderUI({
          radioButtons("decisionMaker", 
                       "Decision Maker: ", 
                       choices = c("Total", decisionMakers), 
                       selected = ifelse(is.null(input$decisionMaker), yes = "Total", no = input$decisionMaker))
        })
      } else {
        output$decisionMaker <- renderUI("")
      }
      output$table <- renderFormattable(ShowTable(ahpTree, 
                                                  decisionMaker =  ifelse(is.null(input$decisionMaker), 
                                                                          yes = "Total", 
                                                                          no = input$decisionMaker)))
    } else {
      ahpTree <- NULL
    }
    
    
    if(input$navbar == "AHP File Format") {
      output$fileFormat <- renderUI(fluidRow(column(6, 
                                                    includeMarkdown(system.file("doc", 
                                                                                "file-format.Rmd", 
                                                                                package="ahp")
                                                                    )
                                                    )
                                             )
                                    )
    }
  })
  
  # Show Upload
  observeEvent(input$showUpload ,{
    print("event: showUpload")
    output$uploadFileOutput <- renderUI({
      #input$uploadFile
      fluidRow(
        column(
          4,
          selectInput("examples", 
                      "Load package example: ", 
                      choices = c("", "car.ahp", "vacation.ahp"), 
                      selected = "")
        ),
        column(
          8,
          HTML('<label class="control-label" for="examples">Load file from disk: </label>'),
          br(),
          fileInput('uploadFile', NULL, width="80%")
        )
      )
      
    })
  })
  
  # Upload File
  observeEvent(input$uploadFile, {
    fileContent <- readChar(input$uploadFile$datapath, file.info(input$uploadFile$datapath)$size)
    updateAceEditor(session, "ace", value = fileContent)
    output$uploadFileOutput <- renderUI("")
  })
  
  
  # Examples
  observeEvent(input$examples, {
    print("event: examples")
    if (nchar(input$examples) > 0) {
      ahpFile <- system.file("extdata", input$examples, package="ahp")
      fileContent <- readChar(ahpFile, file.info(ahpFile)$size)
      updateAceEditor(session, "ace", value = fileContent)
      output$uploadFileOutput <- renderUI("")
    }
    
  })
  

  # Decision Maker
  observeEvent(input$decisionMaker, {
    print("event: decisionMaker")
    output$table <- renderFormattable(ShowTable(ahpTree, input$decisionMaker))
  })
  
  # Ahp Method
  observeEvent(input$ahpmethod, {
    print("event: ahpmethod")
    #recalculate if method changed
    if(!is.null(ahpTree)) {
      try(
        ahpTree <- DoCalculation(input)
      )
      output$table <- renderFormattable(ShowTable(ahpTree, 
                                                  ifelse(is.null(input$decisionMaker), 
                                                         yes = "Total", 
                                                         no = input$decisionMaker)
                                                  )
                                        )
    }
  })
  

  
  ## Event Handlers
  #############################
  
  
  
  
  output$downloadFile <- downloadHandler(
    
    
    filename = function() {
      nme <- 'model.ahp'
      try(
        {
          #try to derive name from model spec
          modelString <- input$ace
          myAhpTree <- LoadString(modelString)
          nme <- paste0(gsub('[^a-zA-Z]', '', myAhpTree$name), ".ahp")
        },
        silent = TRUE
      )
      return (nme)
    },
    content = function(file) {
      writeChar(input$ace, file)
    }
  )
  

  
  
})



