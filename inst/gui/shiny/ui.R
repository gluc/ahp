library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  
  navbarPage(
    "AHP",
    tabPanel(
      "Model", 
      
      
      
      mainPanel(
        fluidPage(
          fluidRow(
            column(
              4,
              selectInput("examples", "Examples: ", choices = c("car.ahp", "vacation.ahp"), selected = "car.ahp")
            ),
            column(
              4,
              uiOutput("uploadFileOutput")
            ),
            column(
              4,
              HTML('<label class="control-label" for="examples">Save file to disk: </label>'),
              br(),
              downloadButton('downloadFile', 'Download')
            )
          ),
        
          
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
        fluidRow(column(6, "Help"))
        )
    ),
    id = "navbar"
  )
 
  
)