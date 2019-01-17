#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(Benchmarking)
library(lpSolveAPI)
library(ucminf)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Final Exam in Shiny"),
  fileInput(inputId = "file", "Choose a csv file with headers as x and y",
            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv" )
            ),
  selectInput("method", "Choose a method:",
              choices = c("crs", "vrs", "drs", "irs", "fdh", "fdh+")),
  submitButton("Update View"),

  
  
  mainPanel(
    tableOutput("contents"),
  
  
  
  
  tabsetPanel(
   tabPanel("Graph Viewer", plotOutput("selected_graph")),
  tabPanel("Peers Viewer", tableOutput("selected_peers")),
  tabPanel("Lambda Viewer", tableOutput("selected_lambda"))
  
),

  h4('Arguments:'),
  h4('x- Inputs or resources used by each decision making unit.'),
  
  h4('y- Outputs or products of each decision making unit. Must have same number of rows as x'),
  
  h4('rts- Returns to scale for the application, production technology, or industry studied'),
  
  h4('vrs- Variable returns to scale, convexity and free disposability'),
  
  h4('drs- Decreasing returns to scale, convexity, down-scaling and free disposability'),
  
  h4('crs- Constant returns to scale, convexity and free disposability'),
  
  h4('irs-  Increasing returns to scale, (up-scaling, but not down-scaling), convexity and free disposability
     '),
  h4('fdh- Free disposability hull, no convexity assumption'),
  h4('frh: A combination of free disposability and restricted or local constant return to scale')

)
)


server <- function(input, output, session) {
  file_df <- reactive({
     in_file <- input$file

    if (is.null(in_file))
      return(NULL)

    read.csv(in_file$datapath, header = T)
  })
  
 

  output$contents <- renderTable({
    if (is.null(file_df()))
      return(NULL)

    file_df()
  })
  
 
    
  output$selected_graph <- renderPlot({
   
    if (is.null(file_df()))
      return(NULL)
    
    df <- file_df()

  dea.plot.frontier(df$x, df$y, RTS = input$method)
  })

  
 
  
  output$selected_peers <- renderTable({

    if (is.null(file_df()))
      return(NULL)
    
    df <- file_df()
    
    peers(dea(df$x, df$y, RTS = input$method))

  })
  
 
  output$selected_lambda <- renderTable({
  
    if (is.null(file_df()))
      return(NULL)
    
    df <- file_df()
    
    lambda(dea(df$x, df$y, RTS = input$method))
    
  })
  
  }
# Run the application
shinyApp(ui = ui, server = server)