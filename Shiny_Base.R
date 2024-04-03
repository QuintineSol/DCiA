# Load necessary libraries
library(shiny)
library(shinydashboard)
library(DT)          # For data tables
library(readxl)      # For reading Excel files
library(shinyAce)    # For the Ace code editor
library(httr)        # For HTTP requests
library(jsonlite)    # For JSON processing
library(igraph)      # For network analysis (not explicitly used in provided snippet but may be needed)

# Setting the Hugging Face API key (ensure this is securely managed in production)
Sys.setenv(HUGGINGFACE_API_KEY = "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")

# Define the User Interface
ui <- dashboardPage(
  dashboardHeader(title = "Apollo"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction"),
      menuItem("Data Upload", tabName = "data_upload"),
      menuItem("Community Detection", tabName = "community_detection")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "introduction",
              fluidRow(
                column(width = 12,
                       wellPanel(
                         h1("Welcome!", align = "center"),
                         h3("Introduction:", align = "center"),
                         p("Welcome to our interdisciplinary collaboration tutorial! This tutorial aims to provide researchers in the fields of digital design, marketing, and music with a user-friendly and non-technical introduction to understanding network data. By exploring the connections and dynamics within your research networks, we hope to empower you to enhance interdisciplinary collaboration and innovation within your respective domains.", style = "text-align: justify; padding: 20px;")
                       )
                )
              )
      ),
      tabItem(tabName = "data_upload",
              fluidRow(
                column(width = 12,
                       h3("Data Upload Page", align = "center"),
                       fileInput('file1', 'Choose CSV/Excel File', accept = c('.csv', '.xlsx', '.xls')),
                       DT::dataTableOutput("dataTable")  # Renders the uploaded data table
                )
              )
      ),
      tabItem(tabName = "community_detection",
              fluidPage(
                h3("Community Detection", align = "center"),
                selectInput("algorithm", "Choose a Community Detection Algorithm:",
                            choices = c("Fast Greedy", "Louvain", "Girvan-Newman", "Walktrap"),
                            selected = "Louvain"),
                actionButton("runAnalysis", "Run Analysis"),
                h4("Modularity Score"),
                verbatimTextOutput("modularityOutput"),
                h4("Community Memberships"),
                DT::dataTableOutput("membershipOutput"),
                h4("Hugging Face Explanation"),
                textOutput("hfExplanation")
              )
      )
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  # Reactive value to store uploaded dataset
  dataset <- reactiveVal(NULL)
  
  # Observe file upload and update `dataset`
  observeEvent(input$file1, {
    inFile <- input$file1
    if (grepl("\\.csv$", inFile$name)) {
      dataset(read.csv(inFile$datapath))  # Load CSV file
    } else if (grepl("\\.(xlsx|xls)$", inFile$name)) {
      dataset(read_excel(inFile$datapath))  # Load Excel file
    }
  })
  
  # Render uploaded data table
  output$dataTable <- DT::renderDataTable({
    req(dataset())  # Ensure dataset is not NULL
    dataset()  # Return the dataset for rendering
  })
  
  analysisResult <- eventReactive(input$runAnalysis, {
    req(dataset())
    g <- graph_from_data_frame(dataset(), directed = FALSE)
    result <- switch(input$algorithm,
                     "Fast Greedy" = cluster_fast_greedy(g),
                     "Louvain" = cluster_louvain(g),
                     "Girvan-Newman" = cluster_edge_betweenness(g),
                     "Walktrap" = cluster_walktrap(g))
    
    modularity_val <- modularity(result)
    memberships <- membership(result)
    
    list(modularity = modularity_val, memberships = memberships)
  })
  
  output$modularityOutput <- renderPrint({
    req(analysisResult())
    analysisResult()$modularity
  })
  
  output$membershipOutput <- DT::renderDataTable({
    req(analysisResult())
    memberships <- analysisResult()$memberships
    data.frame(Node = names(memberships), Community = memberships)
  })
  
  explanationOutput <- eventReactive(input$runAnalysis, {
    req(analysisResult())
    modularity_val <- analysisResult()$modularity
    communities <- toString(unique(analysisResult()$memberships))
    
    prompt <- sprintf("Explain the significance of a modularity score of %s and community memberships as follows: %s, in terms of network analysis and its potential impact.", modularity_val, communities)
    
    
    # Set up API request
    api_url <- "https://api-inference.huggingface.co/models/google/flan-t5-xxl"
    api_key <- Sys.getenv("HUGGINGFACE_API_KEY")
    headers <- add_headers(`Authorization` = paste("Bearer", api_key), `Content-Type` = "application/json")
    body <- toJSON(list(inputs = prompt, parameters = list(max_new_tokens = 512)), auto_unbox = TRUE)
    
    response <- POST(url = api_url, body = body, config = headers)
    content <- content(response, "parsed")
    
    if (response$status_code == 200) {
      return(content$text) 
    } else {
      return(sprintf("Failed to retrieve explanation. Status code: %s", response$status_code))
    }
  })
  
  output$hfExplanation <- renderText({
    explanationOutput()
  })
}

# Run the Shiny application
shinyApp(ui = ui, server = server)
