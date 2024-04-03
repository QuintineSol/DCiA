library(shiny)
library(DT)
library(readxl)
library(httr)
library(jsonlite)
library(igraph)

# Securely manage Hugging Face API Key
Sys.setenv(HUGGINGFACE_API_KEY = "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")

# Define the User Interface
ui <- navbarPage(
  "Interactive Data Analysis App",
  id = "navbar",
  
  tabPanel("Introduction",
           fluidPage(
             wellPanel(
               h1("Welcome!", align = "center"),
               p("This application facilitates interdisciplinary collaboration by allowing researchers to upload network data, apply community detection algorithms, and receive insights into the structure and dynamics of their networks.", style = "text-align: justify; padding: 20px;")
             )
           )),
  
  tabPanel("Data Upload",
           fluidPage(
             h3("Data Upload Page", align = "center"),
             fileInput('file1', 'Choose CSV/Excel File', accept = c('.csv', '.xlsx', '.xls')),
             DT::dataTableOutput("dataTable")
           )),
  
  tabPanel("Community Detection",
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
           ))
)

# Define Server Logic
server <- function(input, output, session) {
  dataset <- reactiveVal(NULL)
  
  observeEvent(input$file1, {
    inFile <- input$file1
    if (grepl("\\.csv$", inFile$name)) {
      dataset(read.csv(inFile$datapath))
    } else if (grepl("\\.(xlsx|xls)$", inFile$name)) {
      dataset(read_excel(inFile$datapath))
    }
  })
  
  output$dataTable <- DT::renderDataTable({
    req(dataset())
    dataset()
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

shinyApp(ui = ui, server = server)
