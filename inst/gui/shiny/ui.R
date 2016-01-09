library(shiny)
library(shinythemes)
library(shinyAce)
library(formattable)
library(shinyjs)
library(DiagrammeR)

# Define UI for application that draws a histogram
shinyUI(
  
  
  
  navbarPage(
    useShinyjs(),
    theme = shinytheme("flatly"),
    "AHP",
    tabPanel(
      "Model", 
      
      
      
      mainPanel(
        fluidPage(
          fluidRow(
            
            column(
              4,
              actionButton("showUpload", "Load", icon = icon("upload"))
              #done on server so the progress bar disappears after upload
              #uiOutput("uploadFileOutput")
            ),
            column(
              4,
              downloadButton('downloadFile', 'Save')
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
      "Visualize", 
      grVizOutput("visualizeTree"),
      value = "visualizePanel"
    ),
    
    tabPanel(
      "Analyze", 
      mainPanel(
        fluidPage(
          fluidRow(
            column(
              3, 
              radioButtons(
                inputId = "ahpmethod", 
                label = "AHP Priority Calculation Method: ", 
                choices = c("Eigenvalues", "Mean of Normalized Values", "Geometric Mean"),
                selected = "Eigenvalues"
              )
            ),
            column(3, uiOutput("sort")),
            column(
              3, 
              radioButtons(
                inputId = "variable", 
                label = "Variable: ", 
                choices = c("Total Contribution", "Priority", "Score"),
                selected = "Total Contribution"
              )
            ),
            column(3, uiOutput("decisionMaker"))
          ),
          fluidRow(
            column(
              3,
              textInput(inputId = "cutoff", label = "Filter rows with weight contribution smaller than: ", value = "0")
            ),
            column(
              3,
              textInput(inputId = "level", label = "Filter n levels: ", value = "0")
            )
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
        includeMarkdown("about.md")
      ),
      tabPanel(
        "Help",
        includeMarkdown("help.md")
      ),
      tabPanel(
        "AHP File Format",
        #done on server because the file format is a vignette
        #that is not directly included with the shiny app
        uiOutput("fileFormat")
      )
    ),
    id = "navbar"
  )
 
  
)