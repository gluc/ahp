library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  
  navbarPage(
    "AHP",
    tabPanel(
      "Model", 
      sidebarLayout(
        sidebarPanel(
          
          selectInput("examples", "Examples: ", choices = c("car.ahp", "vacation.ahp"), selected = "car.ahp"),
          fileInput(
            inputId = 'uploadFile', 
            label = 'Load file: ',
            multiple = FALSE,
            accept = c('ahp')
          ),
          downloadButton('downloadFile', 'Download'),
          br(),
          actionButton('calculate', label = "Calculate", icon = icon("calculator"))
        ),
        mainPanel(
          aceEditor("ace", mode = "yaml", theme = "clouds", value = "define ahp model here")
        )
      )
    ),
    tabPanel(
      "Analysis", 
      mainPanel(
        uiOutput("decisionMaker"),
        formattableOutput("table")
      )
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
    )
  )
 
  
)