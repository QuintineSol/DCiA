# Load necessary libraries
library(shiny)
#library(shiny.router)
library(shinycssloaders)
library(shinydashboard)
library(DT)          # For data tables
library(readxl)      # For reading Excel files
library(shinyAce)    # For the Ace code editor
library(httr)        # For HTTP requests
library(jsonlite)    # For JSON processing
library(igraph)      # For network analysis (not explicitly used in provided snippet but may be needed)
library(visNetwork)
library(english)
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path), '/Comparison_script.R'))

# Setting the Hugging Face API key (ensure this is securely managed in production)
Sys.setenv(HUGGINGFACE_API_KEY = "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")

# Define the User Interface
ui <- dashboardPage(
  dashboardHeader(title = "Apollo"),
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                menuItem("Introduction", tabName = "introduction"),
                menuItem("Data Upload", tabName = "data_upload"),
                menuItem("CUG Test", tabName = "cug_test"),
                menuItem("Community Detection", tabName = "community_detection"),
                menuItem("Network Comparison", tabName = 'network_comparison'),
                menuItem("Data Export", tabName = "data_export")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .previous-button { position: fixed; top: 60px; left: 250px; z-index: 1050; }
        .next-button { position: fixed; top: 60px; right: 20px; z-index: 100; }
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
      tabItem(tabName = "cug_test",
              fluidPage(
                column(
                  width = 12,
                  h3("CUG Test", align = "center"),
                  p("This page allows you to conduct a CUG (Conditional Uniform Graph) test on your network data. In particular, we will focus on evaluating the betweenness centralization of edges in your network. This helps to assess how much influence certain edges have in connecting different parts of the network."),
                  p("The Conditional Uniform Graph (CUG) test is a statistical method used to measure the degree of centralization of edges in a network. Centralization refers to the extent to which a network's connectivity is concentrated around a few edges or nodes. In the context of the CUG test, we specifically examine betweenness centrality, which quantifies the importance of individual edges in facilitating communication between other nodes in the network."),
                  p("By conducting the CUG test, you can gain insights into the structural properties of your network and identify key edges that play a significant role in connecting different components. This information is valuable for understanding the flow of information, identifying potential bottlenecks, and optimizing network efficiency."),
                  actionButton("runCUG", "Run CUG Test"),
                  h4("CUG Test Results"),
                  verbatimTextOutput("cugTestOutput"),
                  h4("Hugging Face Explanation"),
                  textOutput("hfExplanationCUG")
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
                withSpinner(verbatimTextOutput("modularityOutput"), type = 4),
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
                withSpinner(DT::dataTableOutput("membershipOutput"), type = 4),
                h4("Network Visualization"),
                div(
                  p("Use the visualization tool below to explore your network as you wish. The communities are illustrated by node colour.")
                ),
                withSpinner(visNetworkOutput("networkVis", height = "600px"), type = 4),
                h4("Hugging Face Explanation"),
                textOutput("hfExplanation")
              ),
      ),
        tabItem(tabName = 'network_comparison',
                fluidPage(
                h3("Network comaprison Page", align = "center"),
                p('This tab will be focused on the comparison of two different networks, thus, please Upload a second network to compare to'),
                fileInput('file2', 'Choose CSV/Excel File', accept = c('.csv', '.xlsx', '.xls')),
                DT::dataTableOutput("dataTable2"),  # Renders the uploaded data table
                h4('Correlation in the ties between the networks'),
                p('In this part of the analysis a QAP methodology will be used to compare the correlation between two selected networks.\nThe QAP methodology is introduced to make sure that each of the observations do no longer have the dependent property which is normally the case when working with network data.\nIn short the QAP uses a simulation of the network to be able to see what would happen to the observations would they have been independent of each other. From these observations, the QAP can generate a probability density function, which can be used to generate the probabilities of a certain observation.\nUsing the table below as an exmaple, the ones represent that there is a tie between a pair of nodes and a 0 representing that there is not, which represents the network next to it.'),
                fluidRow(
                  column(width = 6,
                         tableOutput('QAPtable1')),
                  column(width = 6, 
                         plotOutput('QAPNet'))),
                p('After the network is established like in the example, the vertices get ordered randomly, so in our example it can for instance be 6,3,1,4,2,5. Than we get the following table'),
                tableOutput('QAPtable2'), 
                p('Finally we use this randomised network to calculate the correlation with the other non-randomised network. This correlation value is the value for a singular value. After this is repeated for a set number of times, onne could get a good idea of the distributions of the underlying statistics, which could finally be used to determine whether there is any statistical significance or not'),
                tags$head(tags$style(HTML("#QAPtable2 table {background-color: white; color = black; border: 1px solid black} .table>tbody>tr>td, .table>tbody>tr>th, .table>tfoot>tr>td, .table>tfoot>tr>th, .table>thead>tr>td, .table>thead>tr>th {border: 1px solid black} .my_table_aa01 th {text-align: center !important;}", media="screen", type="text/css"))),
                tags$head(tags$style(HTML("#QAPtable1 table {background-color: white; color = black; border: 1px solid black} .table>tbody>tr>td, .table>tbody>tr>th, .table>tfoot>tr>td, .table>tfoot>tr>th, .table>thead>tr>td, .table>thead>tr>th {border: 1px solid black} .my_table_aa01 th {text-align: center !important;}", media="screen", type="text/css"))),
                p('Therefore, the number of repetitions which are done in the simulation is a very important metric, as this determines how many times the network is randomised and thus how good the obtained probability density function represents the actual situation. Therefore, ideally the QAP model would run very often to get the best approximation possible. However, this comes with a trade-off in the time it takes before the model is completed, which is highly dependent on the size and complexity of each network. Thus, the exact optimal value differs for each network. Therefore, it is advised to start with a lower number, like 100, and build up slowly such that the best balance between execution time and accuracy can be found.'),
                numericInput('QAPreps', 'Number of repetitions:', 100, min = 10, max = 10000),
                actionButton('QAPAnalysis', 'Run Analysis'), 
                HTML('<h5><b>Output:</b></h5>'),
                verbatimTextOutput('QAP'),
                withSpinner(plotOutput('QAPplot'), type = 4),
                h4('Statistical differences between the networks'),
                selectInput('statisticChoice', 'What statistics should be compared in the networks?', 
                            choices = c('Degree', 'Closeness', 'Betweenness'), multiple = T)),
                actionButton('StatCompare', 'Run Analysis'),
                verbatimTextOutput('Stats'),
                conditionalPanel("input.statisticChoice.includes('Degree')", withSpinner(plotOutput('degree_plot'), type = 4)),
                conditionalPanel("input.statisticChoice.includes('Closeness')", withSpinner(plotOutput('closeness_plot'), type = 4)),
                conditionalPanel("input.statisticChoice.includes('Betweenness')", withSpinner(plotOutput('betweenness_plot'), type = 4)),
                h4('Most important actors in both networks:'),
                selectInput('ActorMetric', 'What statistic should be used to determine the most important actors?', 
                            choices = c('Degree','Closeness','Betweenness'), selected = 'betweenness'),
                numericInput('ActorNum', 'How many actors should be retrieved', 5, min = 1, max = NA),
                actionButton('ActorComparison', 'Run Analysis'),
                verbatimTextOutput('ActorCompText'),
                withSpinner(DT::dataTableOutput('DTActorComp'), type = 4),
                h4('Most important ties in both networks:'),
                selectInput('BridgeMetric', 'What statistic should be used to determine the most important ties?', 
                            choices = c('Absolute', 'Mean'), selected = 'mean'),
                numericInput('BridgeNum', 'How many ties should be retrieved', 5, min = 1, max = NA),
                checkboxGroupInput('BridgeVars', 'Extra choices', 
                                   choices = c('Only local bridges should be considered' = 'Bridges', 'Infinite values should be discarded' = 'drop.infs')),
                actionButton('BridgeComparison', 'Run Analysis'),
                verbatimTextOutput('BridgeCompText'),
                withSpinner(DT::dataTableOutput('DTBridgeComp'), type = 4),

              ),

      tabItem(tabName = "data_export",
              fluidPage(
                h3("Data Export", align = "center"),
                div(
                  p("Here you can download your network and communities. You can use this feature to save the current state of your network for further analysis or sharing with collaborators."),
                  downloadButton("downloadNetwork", "Network"),
                  downloadButton("downloadMemberships", "Communities"),
                  # Render error message only if there is an error
                  conditionalPanel(
                    condition = "output.errorMessage != ''",
                    verbatimTextOutput("errorMessage", placeholder = TRUE)
                    )
                )
              ),
      )
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  # Reactive value to store uploaded dataset
  dataset <- reactiveVal(NULL)
  dataset2 = reactiveVal(NULL)
  
  inet = reactiveVal(NULL)
  inet2 = reactiveVal(NULL)
  
  # Initialize The reactive variables to control when to run analysis
  shouldAnalyze <- reactiveVal(FALSE)
  shouldQAPAnalyse = reactiveVal(F)
  shouldStatisticCompare = reactiveVal(FALSE)
  shouldActorCompare = reactiveVal(FALSE)
  shouldBridgeCompare = reactiveVal(FALSE)
  buttonPressed = reactiveVal(FALSE)
  shouldContinue = reactiveVal(FALSE)
  

  degree_plot <- reactiveVal(NULL)

  # Reactive value to store community memberships
  community_memberships <- reactiveVal(NULL)
  
  # Reactive value to store network object
  network_object <- reactiveVal(NULL)
  
  # Initialize shouldAnalyze to control when to run analysis
  shouldAnalyze <- reactiveVal(FALSE)  

  
  # Observe file upload and update `dataset`
  observeEvent(input$file1, {
    inFile <- input$file1
    if (grepl("\\.csv$", inFile$name)) {
      dataset(read.csv(inFile$datapath))# Load CSV file
      inet(graph_from_data_frame(dataset(), directed = FALSE)) # Save as an igraph to save time on the conversion
    } else if (grepl("\\.(xlsx|xls)$", inFile$name)) {
      dataset(read_excel(inFile$datapath))  # Load Excel file
      inet(graph_from_data_frame(dataset(), directed = FALSE)) # Save as an igraph to save time on the conversion
    }
  })
  
  # Second data import for the second dataset in the comparison tab
  observeEvent(input$file2, {
    inFile <- input$file2
    if (grepl("\\.csv$", inFile$name)) {
      dataset2(read.csv(inFile$datapath))  # Load CSV file
      inet2(graph_from_data_frame(dataset2(), directed = FALSE))
    } else if (grepl("\\.(xlsx|xls)$", inFile$name)) {
      dataset2(read_excel(inFile$datapath))  # Load Excel file
      inet2(graph_from_data_frame(dataset2(), directed = FALSE))
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
  
  # Render the second dataset to show to the user what was put in
  output$dataTable2 <- DT::renderDataTable({
    req(dataset2())  # Ensure dataset is not NULL
    dataset2()  # Return the dataset for rendering
  })
  
  # Dynamically render the "Previous" button
  output$prevButtonUI <- renderUI({
    if (!is.null(input$sidebar) && input$sidebar != "introduction") {  # Exclude on the first tab
      actionButton("prevTab", "Previous", class = "previous-button btn btn-primary")
    }
  })
  
  # Dynamically render the "Next" button
  output$nextButtonUI <- renderUI({
    if (!is.null(input$sidebar) && input$sidebar != "data_export") {  # Exclude on the last tab
      actionButton("nextTab", "Next", class = "next-button btn btn-primary")
    }
  })
  
  # Define the sequence of tabs
  tabNames <- c("introduction", "data_upload", "cug_test", "community_detection", "network_comparison", "data_export")


  
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
  
  # Activate analysis when "Run Analysis" button is clicked
  observeEvent(input$runAnalysis, {
    shouldAnalyze(TRUE)
  })
  
  # Check for multi-edges and show modal dialog if conditions are met
  observeEvent(input$runAnalysis, {
    
    if (is.null(input$file1)) {
      # if there is no data uploaded,  and show a modal message
      showModal(
        modalDialog(title = 'No data uploaded',
                    'Please upload a network on the Data Upload screen before clicking the button',
        easyClose = TRUE,
        footer = modalButton("Got it!"))
      )
      # Prevent further execution
      return()
    }
    req(dataset())
    g <- inet()
    
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
    g <- inet()
    result <- switch(input$algorithm,
                     "Fast Greedy" = cluster_fast_greedy(g),
                     "Louvain" = cluster_louvain(g),
                     "Girvan-Newman" = cluster_edge_betweenness(g),
                     "Walktrap" = cluster_walktrap(g))

    community_memberships(membership(result)) # Store community memberships
    network_object(g) # Store network object
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
      memberships <- community_memberships()
      data.frame(Node = names(memberships), Community = memberships)
    }
  })
  
  # Activate analysis when "Run Analysis" button is clicked
  observeEvent(input$runAnalysis, {
    shouldAnalyze(TRUE)
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
  

  # Activate the QAP analysis when "Run Analysis" button is clicked
  observeEvent(input$QAPAnalysis, {
    shouldQAPAnalyse(TRUE)
  })
  
  # Get the qap test running
  QAP_test = eventReactive(input$QAPAnalysis, {
    req(input$QAPreps)
    req(shouldQAPAnalyse)
    req(dataset())
    req(dataset2())
    g1 <- inet()
    g2 <- inet2()
    result = QAP_networks(g1, g2, reps = input$QAPreps,
                          net1_name = gsub("\\.\\w+$", "", input$file1$name),
                          net2_name = gsub("\\.\\w+$", "", input$file2$name))
    print(paste('Test statistic:', result$testval))
    result
  })
  
  # Print the QAP test
  output$QAP = renderPrint({
    req(QAP_test())
    QAP_test()
  })
  
  # Plot the output of the QAP test
  output$QAPplot = renderPlot({
    req(QAP_test())
    sna::plot.qaptest(QAP_test())
  })
  
  # If the statistic for the statistical comparison changes, stop the constant analysis
  observe({
    input$statisticChoice
    shouldStatisticCompare(FALSE)
  })
  
  # Start the iteration of the analysis once the button is pressed
  observeEvent(input$StatCompare, {
    shouldStatisticCompare(TRUE)
  })
  
  # Load the values for the statistical comparisons once the button is pressed
  Statistical_comparisons = eventReactive(input$StatCompare, {
    req(shouldStatisticCompare())
    req(dataset())
    req(dataset2())
    if (is.null(input$statisticChoice)){
      showModal(
        modalDialog(title = 'No Choice made',
                    'Please select at least one statistic to be tested in the choice menu',
                    easyClose = TRUE,
                    footer = modalButton("Got it!"))
      )
    }
    statistics = c()
    if ('Degree' %in% input$statisticChoice){
      statistics = c(statistics, 'Degree' = T)
    } else {
      statistics = c(statistics, 'Degree' = F)
    }
    if ('Closeness' %in% input$statisticChoice){
      statistics = c(statistics, 'Closeness' = T)
    } else {
      statistics = c(statistics, 'Closeness' = F)
    }
    if ('Betweenness' %in% input$statisticChoice){
      statistics = c(statistics, 'Betweenness' = T)
    } else {
      statistics = c(statistics, 'Betweenness' = F)
    }
    g1 <- inet()
    g2 <- inet2()
    stats = compare_statistics(g1, g2, statistics = statistics,
                               net1_name = gsub("\\.\\w+$", "", input$file1$name),
                               net2_name = gsub("\\.\\w+$", "", input$file2$name))
  })
  
  # Print the output of the statistical comparison
  output$Stats = renderPrint({
    req(Statistical_comparisons())
    Statistical_comparisons()
  })
  
  # Retrieve the plot showing the density of the degrees in both networks
  degree_plot = eventReactive(input$StatCompare, {
    req(input$StatCompare)
    req(shouldStatisticCompare)
    if ('Degree' %in% input$statisticChoice){
    g1 <- inet()
    g2 <- inet2()
    compare_statistics(g1, g2, statistics = c('Degree' = T),
                       net1_name = gsub("\\.\\w+$", "", input$file1$name),
                       net2_name = gsub("\\.\\w+$", "", input$file2$name))$degree_plot
    } else{
      NULL
    }
  }, ignoreInit = T)
  
  # Plot the denisty plot showing the degree in both networks
  output$degree_plot = renderPlot({
    req(degree_plot())
    print(degree_plot())
  })
  
  # Retrieve the plot showing the density of the closeness in both networks
  closeness_plot = eventReactive(input$StatCompare, {
    req(input$StatCompare)
    req(shouldStatisticCompare)
    if ('Closeness' %in% input$statisticChoice){
      g1 <- inet()
      g2 <- inet2()
      compare_statistics(g1, g2, statistics = c('Closeness' = T),
                         net1_name = gsub("\\.\\w+$", "", input$file1$name),
                         net2_name = gsub("\\.\\w+$", "", input$file2$name))$closeness_plot
    } else{
      NULL
    }
  }, ignoreInit = T)
  
  # Plot the denisty plot showing the closeness in both networks
  output$closeness_plot = renderPlot({
    req(closeness_plot())
    print(closeness_plot())
  })
  
  # Retrieve the plot showing the density of the betweenness in both networks
  betweenness_plot = eventReactive(input$StatCompare, {
    req(input$StatCompare)
    req(shouldStatisticCompare)
    if ('Betweenness' %in% input$statisticChoice){
      g1 <- inet()
      g2 <- inet2()
      compare_statistics(g1, g2, statistics = c('Betweenness' = T),
                         net1_name = gsub("\\.\\w+$", "", input$file1$name),
                         net2_name = gsub("\\.\\w+$", "", input$file2$name))$betweenness_plot
    } else{
      NULL
    }
  }, ignoreInit = T)
  
  # Plot the denisty plot showing the betweenness in both networks
  output$betweenness_plot = renderPlot({
    req(betweenness_plot())
    print(betweenness_plot())
  })
  
  # When the metric for the actor comparison changes set the compare to FALSE
  observe({
    input$ActorMetric
    shouldActorCompare(FALSE)
  })
  
  # Execute the actor comparison when the button is pressed
  observeEvent(input$ActorComparison, {
    shouldActorCompare(TRUE)
  })
  
  # Get the values for the actor comparison
  actor_df = eventReactive(input$ActorComparison, {
    req(dataset())
    req(dataset2())
    req(input$ActorMetric)
    req(shouldActorCompare())
    req(input$ActorNum)
    if (input$ActorNum > igraph::vcount(inet())){
      showModal(modalDialog(
        title = "Too many nodes selected",
        paste('Too many nodes were selected,', gsub("\\.\\w+$", "", input$file1$name),'does not contain that many nodes', '\nRequest was', input$ActorNum, 'nodes, whilst a maximum of', igraph::vcount(inet()), 'can be selected'),
        easyClose = TRUE,
        footer = modalButton('Got it!')
      ))
      return()
    } else if (input$ActorNum > igraph::vcount(inet2())){
      showModal(modalDialog(
        title = "Too many nodes selected",
        paste('Too many nodes were selected,', gsub("\\.\\w+$", "", input$file2$name),'does not contain that many nodes', '\nRequest was', input$ActorNum, 'nodes, whilst a maximum of', igraph::vcount(inet2()), 'can be selected'),
        easyClose = TRUE,
        footer = modalButton('Got it!')
      ))
      return()
    }
    g1 <- inet()
    g2 <- inet2()
    compare_actors(g1, g2, metric = input$ActorMetric,
                   net1_name = gsub("\\.\\w+$", "", input$file1$name),
                   net2_name = gsub("\\.\\w+$", "", input$file2$name),
                   n_actors = input$ActorNum)
  })
  
  # Show the print statements made during the Actor Comparisons
  output$ActorCompText = renderPrint({
    req(actor_df())
    'Result:'
  })
  
  # Show the table which shows the actors and their variables
  output$DTActorComp = DT::renderDataTable({
    req(actor_df())
    actor_df()
  }
  )
  
  # When the metric for the bridge comparison changes set the compare to FALSE
  observe({
    input$BridgeMetric
    shouldBridgeCompare(FALSE)
  })
  
  # Execute the bridge comparison when the button is pressed
  observeEvent(input$BridgeComparison, {
    shouldBridgeCompare(TRUE)
  })
  
  # Get the values for the bridge comparison
  bridge_df = eventReactive(input$BridgeComparison, {
    req(dataset())
    req(dataset2())
    req(input$BridgeMetric)
    req(shouldBridgeCompare())
    req(input$BridgeNum)
    if (input$BridgeNum > igraph::ecount(inet())){
      showModal(modalDialog(
        title = "Too many edges selected",
        paste('Too many edges were selected,', gsub("\\.\\w+$", "", input$file1$name),'does not contain that many edges', '\nRequest was', input$BridgeNum, 'edges, whilst a maximum of', igraph::ecount(inet()), 'can be selected'),
        easyClose = TRUE,
        footer = modalButton('Got it!')
      ))
      return()
    } else if (input$BridgeNum > igraph::ecount(inet2())){
      showModal(modalDialog(
        title = "Too many edges selected",
        paste('Too many edges were selected,', gsub("\\.\\w+$", "", input$file2$name),'does not contain that many edges', '\nRequest was', input$BridgeNum, 'edges, whilst a maximum of', igraph::ecount(inet2()), 'can be selected'),
        easyClose = TRUE,
        footer = modalButton('Got it!')
      ))
      return()
    }
    
    g1 <- inet()
    g2 <- inet2()
    withProgress(bridge_comp(g1, g2, method = input$BridgeMetric,
                             net1_name = gsub("\\.\\w+$", "", input$file1$name),
                             net2_name = gsub("\\.\\w+$", "", input$file2$name),
                             n_actors = input$BridgeNum, 
                             drop.inf = 'drop.infs' %in% input$BridgeVars,
                             filter_bridge = 'Bridges' %in% input$BridgeVars),
                 message = 'Calculating Edge Importancies...')
  })
  
  # Show the print statements made during the Bridge Comparisons
  output$BridgeCompText = renderPrint({
    req(bridge_df())
    'Result:'
  })
  
  # Show the table which shows the bridges and their variables
  output$DTBridgeComp = DT::renderDataTable({
    req(bridge_df())
    bridge_df()
  }
  )
  
  output$QAPtable1 = renderTable({
    table = data.frame(A_1 = c(0,1,1,0,0,1), A_2 = c(1,0,0,0,1,0), A_3 = c(1,0,0,1,1,1), A_4 = c(0,0,1,0,0,1), A_5 = c(0,1,1,0,0,1), A_6 = c(1,0,1,1,1,0))
    rownames(table) = c('A<sub>1</sub>', 'A<sub>2</sub>', 'A<sub>3</sub>', 'A<sub>4</sub>', 'A<sub>5</sub>', 'A<sub>6</sub>')
    colnames(table) = c('A<sub>1</sub>', 'A<sub>2</sub>', 'A<sub>3</sub>', 'A<sub>4</sub>', 'A<sub>5</sub>', 'A<sub>6</sub>')
    table
  }, rownames = T, digits = 0, sanitize.text.function = function(x) x, width = '100%'
  )
  
  output$QAPNet = renderPlot({
    net = igraph::make_empty_graph() %>% igraph::add_vertices(6) %>% igraph::add.edges(c(1,2, 1,3, 1,6, 2,5, 3,4, 3,5, 3,6, 4,6, 5,6)) %>% igraph::as.undirected()
    plot(net, main = 'Example Network', 
         vertex.size = 20,
         edge.size =4/32,
         edge.arrow.size = 0.2,
         vertex.color = 'orange',
         vertex.label.cex = 1,
         vertex.label.color = 'black',
         vertex.frame.color = "black",
         layout = igraph::layout_with_graphopt)
  })
  
  output$QAPtable2 = renderTable({
    table = data.frame(A_1 = c(0,1,1,0,0,1), A_2 = c(1,0,0,0,1,0), A_3 = c(1,0,0,1,1,1), A_4 = c(0,0,1,0,0,1), A_5 = c(0,1,1,0,0,1), A_6 = c(1,0,1,1,1,0))
    rownames(table) = c('A<sub>1</sub>', 'A<sub>2</sub>', 'A<sub>3</sub>', 'A<sub>4</sub>', 'A<sub>5</sub>', 'A<sub>6</sub>')
    colnames(table) = c('A<sub>1</sub>', 'A<sub>2</sub>', 'A<sub>3</sub>', 'A<sub>4</sub>', 'A<sub>5</sub>', 'A<sub>6</sub>')
    table[c('A<sub>6</sub>', 'A<sub>3</sub>', 'A<sub>1</sub>', 'A<sub>4</sub>', 'A<sub>2</sub>', 'A<sub>5</sub>'),c('A<sub>6</sub>', 'A<sub>3</sub>', 'A<sub>1</sub>', 'A<sub>4</sub>', 'A<sub>2</sub>', 'A<sub>5</sub>')]
  }, rownames = T, digits = 0, sanitize.text.function = function(x) x, width = '50%'
  )

  # Function to export network data
  output$downloadNetwork <- downloadHandler(
    filename = function() {
      paste("network", ".graphml", sep = "")
    },
    content = function(file) {
      if (!is.null(network_object())) {
        write_graph(network_object(), file, format = "graphml")
      }
    }
  )
  
  # Function to export community memberships
  output$downloadMemberships <- downloadHandler(
    filename = function() {
      paste("community_memberships", ".csv", sep = "")
    },
    content = function(file) {
      if (!is.null(community_memberships())){
        write.csv(data.frame(Node = names(community_memberships()), Community = community_memberships()), file, row.names = FALSE)
      }
    }
  )

  output$errorMessage <- renderText({
    if (is.null(network_object()) && is.null(community_memberships())) {
      return("Error: No network and communities available. Please return to the 'Community Detection' tab.")
    } else if (is.null(network_object())) {
      return("Error: No network available. Please return to the 'Community Detection' tab.")
    } else if (is.null(community_memberships())) {
      return("Error: No communities available. Please return to the 'Community Detection' tab.")
    } else {
      return(NULL)
    }
  })

}



# Run the Shiny application
shinyApp(ui = ui, server = server)
