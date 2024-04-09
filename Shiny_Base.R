# Load necessary libraries
library(shiny)
library(shinydashboard)
library(DT)          # For data tables
library(readxl)      # For reading Excel files
library(shinyAce)    # For the Ace code editor
library(httr)        # For HTTP requests
library(jsonlite)    # For JSON processing
library(igraph)      # For network analysis (not explicitly used in provided snippet but may be needed)
library(visNetwork)

# Setting the Hugging Face API key (ensure this is securely managed in production)
Sys.setenv(HUGGINGFACE_API_KEY = "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")

# Define the User Interface
ui <- dashboardPage(
  dashboardHeader(title = "Apollo"),
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                menuItem("Introduction", tabName = "introduction"),
                menuItem("Data Upload", tabName = "data_upload"),
                menuItem("Community Detection", tabName = "community_detection")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .previous-button { position: fixed; top: 60px; left: 250px; z-index: 1050; }
        .next-button { position: absolute; top: 60px; right: 20px; z-index: 100; }
      "))
    ),
    uiOutput("prevButtonUI"),
    uiOutput("nextButtonUI"),
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
                div(
                  p("In order to detect the communities present in your network you can make use of several different algorithms. To help you selecting the most appropriate one we have included a brief explanation of each. Here is the run-down:"),
                  tags$ul(
                    tags$li(strong("Fast Greedy: "), "this algorithm is like organizing a set of objects into clusters based on how closely they are related. Imagine you have a bunch of items that need to be grouped by similarity; the Fast Greedy method starts by considering each item in its own group. It then combines these groups step by step, each time choosing the combination that results in the most cohesive groups, until no further improvement is possible. This approach is fast and efficient, making it suitable for quickly finding a good grouping in large datasets where each item has many connections."),
                    tags$li(strong("Louvain: "), "this algorithm is akin to sorting a large collection into subsets where each subset contains items that are more similar to each other than to items in other subsets. It begins with each item in its own subset and iteratively merges these subsets to maximize \"modularity,\" a measure of how well the collection is divided. The process continues until the modularity cannot be increased further, indicating that the items are grouped in an optimal way. This method is known for its ability to handle very large collections, quickly identifying an optimal division."),
                    tags$li(strong("Girvan-Newman: "), "this algorithm focuses on identifying the connections that are most critical for maintaining the overall structure of the network. It works by progressively removing these connections, which are identified through measures like \"betweenness\" (a measure of how often a connection lies on the shortest path between pairs of items). This process gradually separates the network into distinct groups based on the connectivity between items. Although thorough, this method can be slower than others, especially for networks with a large number of items or connections."),
                    tags$li(strong("Walktrap: "), "this algorithm is inspired by the idea of random walks within a network to discover groups of closely related items. It posits that short random walks are likely to stay within the same group because the items within a group are more densely interconnected. By analyzing the paths taken during these walks, Walktrap identifies which items tend to cluster together. This approach is effective for revealing the natural grouping within networks based on the connectivity and density of the connections between items.")
                  ),
                ),
                selectInput("algorithm", "Choose a Community Detection Algorithm:",
                            choices = c("Fast Greedy", "Louvain", "Girvan-Newman", "Walktrap"),
                            selected = "Louvain"),
                actionButton("runAnalysis", "Run Analysis"),
                h4("Modularity Score"),
                div(
                  p("Modularity is a measure that quantifies the strength of the division of a network into communities. It ranges from -1 to 1, where a high modularity score indicates a strong presence of community structure within the network. Specifically, it means that nodes within a community are more densely connected to each other than to nodes in different communities. "),
                  tags$ul(
                    tags$li(strong("Significance: "), "Values closer to 1 indicate a clear and well-defined community structure. A modularity score above 0.3 generally signifies meaningful community structures. Scores near 0 or negative suggest weak community structures or randomness. Note, however, that lower scores might indicate a need to revisit the detection strategy or a highly interconnected 'small-world' network."),
                    tags$li(strong("Interpretation: "), "High modularity scores imply dense connections within communities and sparser connections between them, indicating strong community boundaries."),
                    tags$li(strong("Application: "), "This insight can guide strategic decisions, such as enhancing collaboration within clusters or identifying key influencers."),
                  ),
                  p("By interpreting the modularity score in the context of your specific network, you can uncover insights into its underlying structure and dynamics. Let's have a look your network's modulariy score:")
                ),
                verbatimTextOutput("modularityOutput"),
                h4("Community Memberships"),
                div(
                  p("Community membership assigns each node in the network to one or more groups, based on the structure of connections. This reflects the node's role and position within the overall network."),
                  tags$ul(
                    tags$li(strong("Significance: "), "Each node's membership helps identify not only its immediate community but also can indicate its role within the broader network. For instance, nodes that bridge communities might be pivotal for information flow or innovation diffusion."),
                    tags$li(strong("Interpretation: "), "Analyzing membership scores across the network allows for the identification of tightly-knit groups, potential outliers, or bridges between communities. This can inform on the network's cohesion, potential bottlenecks, or facilitators of connectivity."),
                    tags$li(strong("Application: "), "Understanding node memberships can guide targeted interventions, such as personalized communication strategies, fostering collaborations, or strengthening community ties. In organizational contexts, this could support team formation or identify key influencers and innovators.")
                  ),
                  p("Community membership insights provide a granular view of how individuals or nodes are grouped within the network, offering a foundation for targeted strategies and initiatives. Here are the memberships found in your data:"),
                ),
                DT::dataTableOutput("membershipOutput"),
                h4("Network Visualization"),
                div(
                  p("Use the visualization tool below to explore your network as you wish. The communities are illustrated by node colour.")
                ),
                visNetworkOutput("networkVis", height = "600px"),
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
  
  # Initialize shouldAnalyze to control when to run analysis
  shouldAnalyze <- reactiveVal(FALSE)  
  
  # Observe file upload and update `dataset`
  observeEvent(input$file1, {
    inFile <- input$file1
    if (grepl("\\.csv$", inFile$name)) {
      dataset(read.csv(inFile$datapath))  # Load CSV file
    } else if (grepl("\\.(xlsx|xls)$", inFile$name)) {
      dataset(read_excel(inFile$datapath))  # Load Excel file
    }
  })
  
  # Reactively reset shouldAnalyze when algorithm changes
  observe({
    input$algorithm
    shouldAnalyze(FALSE)
  })
  
  # Render uploaded data table
  output$dataTable <- DT::renderDataTable({
    req(dataset())  # Ensure dataset is not NULL
    dataset()  # Return the dataset for rendering
  })
  
  # Dynamically render the "Previous" button
  output$prevButtonUI <- renderUI({
    if (!is.null(input$sidebar) && input$sidebar != "introduction") {  # Exclude on the first tab
      actionButton("prevTab", "Previous", class = "previous-button btn btn-primary")
    }
  })
  
  # Dynamically render the "Next" button
  output$nextButtonUI <- renderUI({
    if (!is.null(input$sidebar) && input$sidebar != "community_detection") {  # Exclude on the last tab
      actionButton("nextTab", "Next", class = "next-button btn btn-primary")
    }
  })
  
  # Define the sequence of tabs
  tabNames <- c("introduction", "data_upload", "community_detection")
  
  # Function to navigate to the next tab
  observeEvent(input$nextTab, {
    currentTab <- which(tabNames == input$sidebar)
    nextTab <- ifelse(currentTab < length(tabNames), currentTab + 1, currentTab)
    updateTabItems(session, "sidebar", tabNames[nextTab])
  })
  
  # Function to navigate to the previous tab
  observeEvent(input$prevTab, {
    currentTab <- which(tabNames == input$sidebar)
    prevTab <- ifelse(currentTab > 1, currentTab - 1, currentTab)
    updateTabItems(session, "sidebar", tabNames[prevTab])
  })
  
  # Activate analysis when "Run Analysis" button is clicked
  observeEvent(input$runAnalysis, {
    shouldAnalyze(TRUE)
  })
  
  # Check for multi-edges and show modal dialog if conditions are met
  observeEvent(input$runAnalysis, {
    
    req(dataset())
    g <- graph_from_data_frame(dataset(), directed = FALSE)
    
    # Simplify the graph to merge multiple edges and check for multi-edges
    g_simplified <- simplify(g)
    if (ecount(g) > ecount(g_simplified) && input$algorithm == "Fast Greedy") {
      showModal(modalDialog(
        title = "Incompatible Algorithm Selected",
        "The Fast Greedy algorithm cannot be run on graphs with multi-edges and the data you uploaded includes duplicate edges. Sorry but it won't work this time, you should select a different algorithm for your data.",
        easyClose = TRUE,
        footer = modalButton("Got it!")
      ))
      # Prevent further execution
      return()
    }
    
    # Proceed with analysis if no error
    analysisResult()
  })
  
  # Analysis result reactive expression
  analysisResult <- eventReactive(input$runAnalysis, {
    
    req(dataset())
    g <- graph_from_data_frame(dataset(), directed = FALSE)
    result <- switch(input$algorithm,
                     "Fast Greedy" = cluster_fast_greedy(g),
                     "Louvain" = cluster_louvain(g),
                     "Girvan-Newman" = cluster_edge_betweenness(g),
                     "Walktrap" = cluster_walktrap(g))
    list(g = g, result = result, modularity = modularity(result), memberships = membership(result))
  })
  
  # Modularity Output
  output$modularityOutput <- renderPrint({
    req(analysisResult())
    if (!is.null(analysisResult()$error)) {
      analysisResult()$error
    } else {
      analysisResult()$modularity
    }
  })
  
  # Membership Output
  output$membershipOutput <- DT::renderDataTable({
    req(analysisResult())
    if (!is.null(analysisResult()$error)) {
      return(data.frame(Error = analysisResult()$error))
    } else {
      memberships <- analysisResult()$memberships
      data.frame(Node = names(memberships), Community = memberships)
    }
  })
  
  # Community Plot
  output$networkVis <- renderVisNetwork({
    req(shouldAnalyze(), analysisResult())
    if (input$runAnalysis && is.null(analysisResult()$error)) {
      analysis <- req(analysisResult())
      g <- analysis$g
      result <- analysis$result
      
      # Assign colors to communities
      memberships <- membership(result)
      community_colors <- rainbow(max(memberships))
      node_colors <- community_colors[memberships]
      
      # Set node options based on community color
      V(g)$color <- node_colors
      
      visNetwork::visIgraph(g) %>%
        visIgraphLayout(layout = "layout_with_fr") %>%
        visNodes(color = list(background = node_colors, border = "#2b2b2b", highlight = "#ff0000"), 
                 shadow = list(enabled = TRUE, size = 10, x = 0, y = 0)) %>%
        visEdges(
          # arrows = 'to',
          color = list(color = "#cccccc", highlight = "#ffff33"),
          shadow = list(enabled = FALSE)
        ) %>%
        visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), 
                   nodesIdSelection = list(enabled = TRUE, style = "width: 150px;")) %>%
        visLayout(randomSeed = 123) %>%
        visInteraction(navigationButtons = TRUE) %>%
        visEdges(smooth = FALSE) 
    }
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
