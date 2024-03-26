# Load necessary libraries
library(shiny)
library(DT)          # For data tables
library(readxl)      # For reading Excel files
library(shinyAce)    # For the Ace code editor
library(httr)        # For HTTP requests
library(jsonlite)    # For JSON processing
library(igraph)      # For network analysis (not explicitly used in provided snippet but may be needed)

# Setting the Hugging Face API key (ensure this is securely managed in production)
Sys.setenv(HUGGINGFACE_API_KEY = "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")

# Define the User Interface
ui <- navbarPage(
  "Interactive Data Analysis App", 
  id = "navbar",
  
  # Introduction tab
  tabPanel("Introduction",
           fluidPage(
             wellPanel(
               h1("Welcome!", align = "center"),
               h3("Introduction:", align = "center"),
               p("Welcome to our interdisciplinary collaboration tutorial! This tutorial aims to provide researchers in the fields of digital design, marketing, and music with a user-friendly and non-technical introduction to understanding network data. By exploring the connections and dynamics within your research networks, we hope to empower you to enhance interdisciplinary collaboration and innovation within your respective domains.", style = "text-align: justify; padding: 20px;")
             )
           )),
  
  # Data upload tab
  tabPanel("Data Upload",
           fluidPage(
             h3("Data Upload Page", align = "center"),
             fileInput('file1', 'Choose CSV/Excel File', accept = c('.csv', '.xlsx', '.xls')),
             DT::dataTableOutput("dataTable")  # Renders the uploaded data table
           )),
  
  # Code execution tab
  tabPanel("Code Execution",
           fluidPage(
             h3("Code Execution Page", align = "center"),
             shinyAce::aceEditor("code", mode = "r", theme = "github", value = "data <- dataset\ni_data <- snafun::to_igraph(data)\nsnafun::g_summary(i_data)"),
             actionButton("runCode", "Run Code"),  # Button to execute the code in the Ace editor
             h4("Code Output"),
             verbatimTextOutput("codeOutput"),  # Area to display the output of executed code
             h4("Hugging Face Explanation"),
             textOutput("hfExplanation")  # Area to display the explanation from Hugging Face API
           ))
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
  
  # Render the explanation text obtained from the Hugging Face API
  output$hfExplanation <- renderText({
    explanationOutput()  # Use the reactive expression to get the explanation text
  })
}

# Run the Shiny application
shinyApp(ui = ui, server = server)






