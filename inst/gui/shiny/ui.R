library(shiny)
library(shinythemes)
library(shinyAce)
library(formattable)
library(shinyjs)
library(DiagrammeR)


shinyUI(
  
  navbarPage(
    "AHP",
    tabPanel(
      "Model", 
      
      
        fluidPage(
          useShinyjs(),
          fluidRow(
            column(2, actionButton("showUpload", "Load", icon = icon("upload"))),
            column(2, downloadButton('downloadFile', 'Save'))
          ),
          fluidRow(uiOutput("uploadFileOutput")),
          br(),
          fluidRow(aceEditor("ace", mode = "yaml", theme = "clouds", value = "define ahp model here"))
        )
      
    ),
    
    tabPanel(
      "Visualize", 
      grVizOutput("visualizeTree"),
      value = "visualizePanel"
    ),
    
    tabPanel(
      "Analyze", 
     
        sidebarLayout(
          sidebarPanel(
            radioButtons(
              inputId = "ahpmethod", 
              label = "AHP Priority Calculation Method: ", 
              choices = c("Eigenvalues", "Mean of Normalized Values", "Geometric Mean"),
              selected = "Eigenvalues"
            ),
           
            radioButtons(
              inputId = "sort", 
              label = "Sort Order: ", 
              choices = c("Total Priority", "Priority", "Original"),
              selected = "Total Priority"
            ),
           
            radioButtons(
              inputId = "variable", 
              label = "Variable: ", 
              choices = c("Total Contribution", "Priority", "Score"),
              selected = "Total Contribution"
            ),
         
            uiOutput("decisionMaker"),
            textInput(inputId = "cutoff", label = "Filter by weight contribution: ", value = "0"),
            textInput(inputId = "level", label = "Filter n levels: ", value = "0")
          ),
          mainPanel(formattableOutput("table"))
        
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
    position = "fixed-top",
    theme = shinytheme("flatly"),
    tags$style(type="text/css", "body {padding-top: 70px;}"),
    id = "navbar"
    
  )
 
  
)