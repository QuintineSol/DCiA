# Load necessary libraries
library(shiny)
library(shinydashboard)
library(DT)          # For data tables
library(readxl)      # For reading Excel files
library(shinyAce)    # For the Ace code editor
library(httr)        # For HTTP requests
library(jsonlite)    # For JSON processing
library(igraph)      # For network analysis
library(intergraph)  # For converting network data

# Setting the Hugging Face API key (ensure this is securely managed in production)
Sys.setenv(HUGGINGFACE_API_KEY = "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")

# Define the User Interface
ui <- dashboardPage(
  dashboardHeader(title = "Apollo"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction"),
      menuItem("Data Upload", tabName = "data_upload"),
      menuItem("Code Execution", tabName = "code_execution"),
      menuItem("CUG Test", tabName = "cug_test")
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
      tabItem(tabName = "code_execution",
              fluidRow(
                column(width = 12,
                       h3("Code Execution Page", align = "center"),
                       shinyAce::aceEditor("code", mode = "r", theme = "github", value = "data <- dataset\ni_data <- snafun::to_igraph(data)\nsnafun::g_summary(i_data)"),
                       actionButton("runCode", "Run Code"),  # Button to execute the code in the Ace editor
                       h4("Code Output"),
                       verbatimTextOutput("codeOutput"),  # Area to display the output of executed code
                       h4("Hugging Face Explanation"),
                       textOutput("hfExplanation")  # Area to display the explanation from Hugging Face API
                )
              )
      ),
      tabItem(tabName = "cug_test",
              fluidPage(
                column(
                  width = 12,
                  h3("CUG Test", align = "center"),
                  p("This page allows you to conduct a CUG (Conditional Uniform Graph) test on your network data. In particular, we will focus on evaluating the betweenness centralization of edges in your network. This helps to assess how much influence certain edges have in connecting different parts of the network."),
                  p("The Conditional Uniform Graph (CUG) test is a statistical method used to measure the degree of centralization of edges in a network. Centralization refers to the extent to which a network's connectivity is concentrated around a few edges or nodes. In the context of the CUG test, we specifically examine betweenness centrality, which quantifies the importance of individual edges in facilitating communication between other nodes in the network."),
                  p("By conducting the CUG test, you can gain insights into the structural properties of your network and identify key edges that play a significant role in connecting different components. This information is valuable for understanding the flow of information, identifying potential bottlenecks, and optimizing network efficiency."),
                  actionButton("runCUG", "Run CUG Test"),
                  fluidRow(
                    column(width = 6,
                           plotOutput("networkPlot", width = "100%", height = "400px")
                    ),
                    column(width = 6,
                           plotOutput("betweennessPlot", width = "100%", height = "400px")
                    )
                  ),
                  h4("CUG Test Results"),
                  verbatimTextOutput("cugTestOutput"),
                  h4("Hugging Face Explanation"),
                  textOutput("hfExplanationCUG")
                )
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
  
  # Reactive expression for CUG test
  cugTest <- eventReactive(input$runCUG, {
    req(dataset())
    n <- intergraph::asNetwork(graph_from_data_frame(dataset(), directed = FALSE))
    # Perform CUG test here
    cug_test_result <- sna::cug.test(
      n,  
      FUN = sna::centralization,
      FUN.arg = list(FUN = sna::betweenness), 
      mode = "graph",
      cmode = "edges",
      reps = 10,
      ignore.eval = TRUE
    )
    return(cug_test_result)
  })
  
  # Render CUG test results
  output$cugTestOutput <- renderPrint({
    req(input$runCUG)
    print(cugTest())
  })
  
  # Render the explanation text obtained from the Hugging Face API
  output$hfExplanationCUG <- renderText({
    explanationOutput()  # Use the existing reactive expression for Hugging Face explanation
  })
  
  # Generate network plot before CUG test
  output$networkPlot <- renderPlot({
    req(dataset())
    n <- intergraph::asNetwork(graph_from_data_frame(dataset(), directed = FALSE))
    plot(n, edge.width = cugTest()$betweenness * 10, edge.color = "blue")
  })
  
  # Generate betweenness centrality plot after CUG test
  output$betweennessPlot <- renderPlot({
    req(input$runCUG)
    req(dataset())
    barplot(cugTest()$betweenness, names.arg = 1:length(cugTest()$betweenness), main = "Betweenness Centrality")
  })
  
  # Reactive expression to handle API call for generating explanations
  explanationOutput <- eventReactive(input$runCode, {
    req(input$code)  # Ensure code is entered before proceeding
    codeToRun <- input$code
    
    tryCatch({
      evalEnv <- new.env()  # New environment for code evaluation
      evalEnv$dataset <- dataset()  # Make dataset available in the environment
      # Capture and collapse the output of the executed code
      result <- capture.output(eval(parse(text = codeToRun), envir = evalEnv))
      codeOutputStr <- paste(result, collapse = "\n")
      
      # Construct the prompt for the API
      prompt <- paste0("Output:\n", codeOutputStr, 
                       "\n\nPlease provide an elaborate explanation for output and definition about network analysis and their significance in plain text elaborate.")
      
      # Set up API request
      api_url <- "https://api-inference.huggingface.co/models/google/flan-t5-xxl"
      api_key <- Sys.getenv("HUGGINGFACE_API_KEY")
      headers <- add_headers(`Authorization` = paste("Bearer", api_key), `Content-Type` = "application/json")
      body <- toJSON(list(inputs = prompt, parameters = list(max_new_tokens = 512)), auto_unbox = TRUE)
      
      # Execute the API request
      response <- POST(url = api_url, headers, body = body)
      content <- content(response, "parsed")
      
      # Process the API response
      if (response$status_code == 200) {
        # Assuming content structure is correct, return the first item's text
        if (!is.null(content[[1]])) {
          return(as.character(content[[1]]))
        } else {
          return("Explanation found but unable to parse.")
        }
      } else {
        return(sprintf("Failed to retrieve explanation. Status code: %s, Response: %s", response$status_code, rawToChar(response$content)))
      }
    }, error = function(e) {
      return(sprintf("Error: %s", e$message))
    })
  })
  
  # Render the output of the executed code
  output$codeOutput <- renderPrint({
    req(input$runCode)  # Wait for the 'Run Code' button to be pressed
    tryCatch({
      evalEnv <- new.env()
      evalEnv$dataset <- dataset()
      result <- eval(parse(text = input$code), envir = evalEnv)
      print(result)
    }, error = function(e) {
      cat(sprintf("Error in code execution: %s", e$message))
    })
  })
}

# Run the Shiny application
shinyApp(ui = ui, server = server)
