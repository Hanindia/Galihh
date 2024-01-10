library(shiny)
library(ggplot2)
library(dplyr)
library(rsconnect)

# Define UI components
ui <- fluidPage(
  titlePanel("Ad Placement CTR Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File"),
      actionButton("reset", "Reset"),
      helpText("Upload a CSV file containing CTR data.")
    ),
    
    mainPanel(
      plotOutput("placementPlot"),
      verbatimTextOutput("summary")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  data <- reactive({
    req(input$file)
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    
    df <- read.csv(inFile$datapath)
    return(df)
  })
  
  output$placementPlot <- renderPlot({
    # Create boxplot here based on the data with colored boxes
    ggplot(data(), aes(x = placement, y = CTR, fill = placement)) +
      geom_boxplot(color = "black") +
      scale_fill_manual(values = c("red", "blue", "green")) +
      labs(fill = "Placement") # Menambahkan label untuk legenda warna
  })
  
  output$summary <- renderPrint({
    # Perform statistical analysis here and output the results
    if (!is.null(data())) {
      result <- aov(CTR ~ placement, data = data())
      summary_result <- summary(result)
      p_value <- summary_result[[1]]$"Pr(>F)"[1]
      
      cat("Analysis of Variance Table:\n")
      print(summary_result)
      
      cat("\n\n")
      cat("P-value:", p_value, "\n")
      cat("Statistical Significance: ", ifelse(p_value < 0.05, "Yes", "No"), "\n")
    } else {
      cat("Upload a CSV file to see the analysis.")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
