library(shiny)
library(shinyAce)
library(formattable)
library(shinyjs)
library(DiagrammeR)
library(ahp)



#########################
## Helpers


DoCalculation <- function(input) {
  
  tryCatch({
    modelString <- input$ace
    #print(modelString)
    ahpTree <- LoadString(modelString)
    Calculate(ahpTree, GetMethod(input))
    return (ahpTree)
  },
  error = function(e) {
    return (NULL)
  })
  
}


GetTable <- function(input, ahpTree) {
  
  tryCatch({
    if (!is.null(ahpTree)) {
      dm <- ifelse(
        is.null(input$decisionMaker),                                                                             
        yes = "Total", 
        no = input$decisionMaker
      )
      
      co <- TryAsNumeric(input$cutoff, 0)
      le <- TryAsNumeric(input$level, 0)
      renderFormattable(
        AnalyzeTable(
          ahpTree, 
          decisionMaker = dm,
          variable = GetVariable(input),
          sort = GetSort(input),
          pruneFun = function(x, dm) PruneByCutoff(x, dm, co) && PruneLevels(x, dm, le)

        )
      )
    } else {
      print("not printing table")
      renderUI("")
    }
  },
  error = function(e) {
    warning(as.character(e))
    renderUI(as.character(e))
  })
}

GetMethod <- function(input) {
  if (is.null(input$ahpmethod)) return (PrioritiesFromPairwiseMatrixEigenvalues)
  switch (
    input$ahpmethod,
    `Eigenvalues` = PrioritiesFromPairwiseMatrixEigenvalues,
    `Mean of Normalized Values` = PrioritiesFromPairwiseMatrixMeanNormalization,
    `Geometric Mean` = PrioritiesFromPairwiseMatrixGeometricMean
  )
}


GetVariable <- function(input) {
  if (is.null(input$variable)) return ("weightContribution")
  switch (
    input$variable,
    `Total Contribution` = "weightContribution",
    `Score` = "score",
    `Priority` = "priority"
  )
}

GetSort <- function(input) {
  if (is.null(input$sort)) return ("priority")
  switch (
    input$sort,
    `Priority` = "priority",
    `Total Priority` = "totalPriority",
    `Original` = "orig"
  )
}

TryAsNumeric <- function(input, defaultVal) {
  if (is.numeric(input)) return (input)
  
  res <- tryCatch(
    as.numeric(input),
    warning = function(e) defaultVal,
    error = function(e) defaultVal
    
  )
  if (!is.na(defaultVal) && is.na(res)) res <- defaultVal
  return (res)
}

## Helpers
#########################


shinyServer(function(input, output, session) {
  
  #####################
  ## Event Handlers
  
  # Navbar
  observeEvent(input$navbar, {
    print(paste0("event: navbar ", input$navbar))
    
    # Visualize
    if(input$navbar == "visualizePanel") {
      ahpTree <- DoCalculation(input)
      output$visualizeTree <- renderGrViz(grViz(generate_dot(GetGraph(ahpTree))))
    }
    
    
    if(input$navbar == "AHP File Format") {
      #cannot find images
      #output$fileFormat <- renderUI(includeMarkdown(rmarkdown::render(system.file("doc", "file-format.Rmd", package="ahp"))))
      
      #does not produce toc
      #output$fileFormat <- renderUI(includeMarkdown(system.file("doc", "file-format.Rmd", package="ahp")))
      
      #only works nicely if theme is the same for shiny app and for vignette
      output$fileFormat <- renderUI(includeHTML(system.file("doc", "file-format.html", package="ahp")))
    }
  })
  
  # Show Upload
  observeEvent(input$showUpload ,{
    print("event: showUpload")
    sampleFiles <- list.files(system.file("extdata", package="ahp"), full.names = TRUE)
    sampleFiles <- basename(sampleFiles[!file.info(sampleFiles)$isdir])
    output$uploadFileOutput <- renderUI({
      #input$uploadFile
      fluidRow(
        column(
          4,
          selectInput("examples", 
                      "Load package example: ", 
                      choices = c("", sampleFiles), 
                      selected = ""
                      )
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
  
  # Calculate AHP
  observe({
    
    input$variable
    input$decisionMaker
    input$ahpmethod
    input$sort
    #input$ace
    input$cutoff
    input$level
    input$navbar
    
    if (input$navbar == "analysis") {
      print("Calculate tree")
      tryCatch({
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
          show(id = "decisionMaker", anim = TRUE)
          sel <- ifelse(length(input$sort) > 0, input$sort, "Total Priority")
          #browser()
          updateRadioButtons(session, "sort", choices = c("Total Priority", "Priority", "Original"), selected = sel)
        } else {
          #browser()
          sel <- ifelse(length(input$sort) == 0 || input$sort == "Total Priority", "Priority", input$sort)
          updateRadioButtons(session, "sort", choices = c("Priority", "Original"), selected = sel)
          #Cannot just remove, because otherwise the value sticks!
          updateRadioButtons(session, "decisionMaker", selected = "Total")
          hide(id = "decisionMaker", anim = TRUE)
        }
        #browser()
        output$table <- GetTable(input, ahpTree)
        
      },
      error = function(e) {
        output$table <- renderFormattable(formattable(TRUE, yes = as.character(e)))
        
      })
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



