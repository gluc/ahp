library(shiny)
library(shinyAce)
library(formattable)
library(shinyjs)
library(DiagrammeR)



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
    
    # Analysis
    if (input$navbar == "analysis") {
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
          
          output$sort <- renderUI({
            radioButtons(
              inputId = "sort",
              label = "Sort: ",
              choices = c("Priority", "Total Priority", "Original"),
              selected = "Total Priority"
            )
          })
          
        } else {
          output$sort <- renderUI({
            radioButtons(
              inputId = "sort",
              label = "Sort: ",
              choices = c("Priority", "Original"),
              selected = "Priority"
            )
          })
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
    
    if(input$navbar == "visualizePanel") {
      ahpTree <- DoCalculation(input)
      #output$visualizeTree <- renderDiagrammeR(GetGraph(ahpTree) )
      output$visualizeTree <- renderGrViz(grViz(GetGraph(ahpTree)$dot_code) )
    }
    
    
    if(input$navbar == "AHP File Format") {
      #output$fileFormat <- renderUI(includeMarkdown(rmarkdown::render(system.file("doc", "file-format.Rmd", package="ahp"))))
      #output$fileFormat <- renderUI(includeMarkdown(system.file("doc", "file-format.Rmd", package="ahp")))
      output$fileFormat <- renderUI(includeHTML(system.file("doc", "file-format.html", package="ahp")))
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
                      choices = c("", list.files(system.file("extdata", package="ahp"), full.names = FALSE)), 
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
  
  
  
  # Sort Order
  observe({
    input$variable
    input$decisionMaker
    input$sort
    ahpTree <- DoCalculation(input)
    output$table <- GetTable(input, ahpTree)
  })
  
  # Ahp Method
  observeEvent(input$ahpmethod, {
    print("event: ahpmethod")

    ahpTree <- DoCalculation(input)
    output$table <- GetTable(input, ahpTree)
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



