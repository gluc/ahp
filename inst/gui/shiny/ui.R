library(shiny)
library(shinyAce)
library(formattable)

# Define UI for application that draws a histogram
shinyUI(
  
  navbarPage(
    theme = "bootstrap.css",
    "AHP",
    tabPanel(
      "Model", 
      
      
      
      mainPanel(
        fluidPage(
          fluidRow(
            
            column(
              4,
              HTML('<label class="control-label" for="examples">Load file: </label>'),
              br(),
              actionButton("showUpload", "Upload", icon = icon("upload"))
              #done on server so the progress bar disappears after upload
              #uiOutput("uploadFileOutput")
            ),
            column(
              4,
              HTML('<label class="control-label" for="examples">Save file: </label>'),
              br(),
              downloadButton('downloadFile', 'Download')
            )
          ),
          br(),
          uiOutput("uploadFileOutput"),
          br(),
        
          
          aceEditor("ace", mode = "yaml", theme = "clouds", value = "define ahp model here")
        )
      )
    ),
    
    tabPanel(
      "Analysis", 
      mainPanel(
        fluidPage(
          fluidRow(
            column(
              6, 
              radioButtons(
                inputId = "ahpmethod", 
                label = "AHP Priority Calculation Method: ", 
                choices = c("Eigenvalues", "Mean of Normalized Values", "Geometric Mean"),
                selected = "Eigenvalues"
              )
            ),
            column(6, uiOutput("decisionMaker"))
          ),
          formattableOutput("table")
        )
      ),
      value = "analysis"
    ),
    navbarMenu(
      "More",
      tabPanel(
        "About",
        fluidRow(column(6, includeMarkdown("about.md")))
      ),
      tabPanel(
        "Help",
        fluidRow(column(6, includeMarkdown("help.md")))
      ),
      tabPanel(
        "AHP File Format",
        uiOutput("fileFormat")
      )
    ),
    id = "navbar"
  )
 
  
)