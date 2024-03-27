install.packages("shiny")
install.packages("shinyWidgets")

## Only run examples in interactive R sessions
if (interactive()) {
  
  library("shiny")
  library("shinyWidgets")
  
  ui <- fluidPage(
    tags$h2("pickerInput in dropdown"),
    br(),
    dropdown(
      
      tags$h3("List of Input"),
      
      pickerInput(inputId = 'xcol2',
                  label = 'X Variable',
                  choices = names(iris),
                  options = list(`style` = "btn-info")),
      
      pickerInput(inputId = 'ycol2',
                  label = 'Y Variable',
                  choices = names(iris),
                  selected = names(iris)[[2]],
                  options = list(`style` = "btn-warning")),
      
      sliderInput(inputId = 'clusters2',
                  label = 'Cluster count',
                  value = 3,
                  min = 1, max = 9),
      
      style = "unite", icon = icon("gear"),
      status = "danger", width = "300px",
      animate = animateOptions(
        enter = animations$fading_entrances$fadeInLeftBig,
        exit = animations$fading_exits$fadeOutRightBig
      )
    ),
    
    plotOutput(outputId = 'plot2')
  )
  
  server <- function(input, output, session) {
    
    selectedData2 <- reactive({
      iris[, c(input$xcol2, input$ycol2)]
    })
    
    clusters2 <- reactive({
      kmeans(selectedData2(), input$clusters2)
    })
    
    output$plot2 <- renderPlot({
      palette(c("#E41A1C", "#377EB8", "#4DAF4A",
                "#984EA3", "#FF7F00", "#FFFF33",
                "#A65628", "#F781BF", "#999999"))
      
      par(mar = c(5.1, 4.1, 0, 1))
      plot(selectedData2(),
           col = clusters2()$cluster,
           pch = 20, cex = 3)
      points(clusters2()$centers, pch = 4, cex = 4, lwd = 4)
    })
    
  }
  
  shinyApp(ui = ui, server = server)
  
}


test <- zoom_in_network(graph_knowledge, intervention_node = "83979631")
plot(test)

intervention_graph <- simulate_intervention(graph_knowledge, intervention_node = "83979631", delete_node = "77795114")
plot(intervention_graph)

# Function to obtain before and after graphs based on the intervention node
get_before_after_graphs <- function(original_graph, intervention_node) {
  # Simulate intervention to obtain after graph
  after_graph <- simulate_intervention(original_graph, intervention_node)
  
  # Create a copy of the original graph for before graph
  before_graph <- original_graph
  
  return(list(before_graph = before_graph, after_graph = after_graph))
}

library(shiny)
library(shinyWidgets)
library(igraph)

zoom_in_network <- function(graph, intervention_node, radius = 2) {
  ego <- igraph::make_ego_graph(graph, order = 2, nodes = intervention_node, mode = c("all"), mindist = 0)
  return(ego[[1]])
}

compare_networks <- function(before_graph, after_graph, intervention_node) {
  par(mfrow = c(1, 2))  # Set up side-by-side plotting
  
  # Plot the induced subgraph before intervention
  plot(before_graph, main = "Before Intervention",
       vertex.size = 8,
       edge.size =4/32,
       edge.arrow.size = 0.2,
       vertex.color = ifelse(V(before_graph)$name == intervention_node, "red", "blue"),, 
       vertex.label.cex = 0.4,
       vertex.label.color = 'black',
       vertex.frame.color = "#ffffff",
       vertex.label = NA,
       layout = igraph::layout.fruchterman.reingold)
  
  # Plot the induced subgraph after intervention
  plot(after_graph, main = "After Intervention",
       vertex.size = 8,
       edge.size =4/32,
       edge.arrow.size = 0.2,
       vertex.color = ifelse(V(after_graph)$name == intervention_node, "red", "blue"),
       vertex.label.cex = 0.4,
       vertex.label.color = 'black',
       vertex.frame.color = "#ffffff",
       vertex.label = NA,
       layout = igraph::layout.fruchterman.reingold)
  
  # Calculate and display betweenness centralization
  before_bc <- centralization.betweenness(before_graph)$centralization
  after_bc <- centralization.betweenness(after_graph)$centralization
  overall_bc <- centralization.betweenness(graph_knowledge)$centralization
  
  cat("Before Intervention - Betweenness Centralization:", before_bc, "\n")
  cat("After Intervention - Betweenness Centralization:", after_bc, "\n")
  cat("Overall Graph Knowledge - Betweenness Centralization:", overall_bc, "\n")
}

simulate_intervention <- function(graph, intervention_node, deletion_node, intervention_method) {
  sub_graph <- zoom_in_network(graph, intervention_node)
  
  if (intervention_method == "Remove Node") {
    new_graph <- delete_vertices(sub_graph, deletion_node)
  } else if (intervention_method == "Add Edge") {
    new_graph <- add_edges(sub_graph, cbind(rep(intervention_node, length(deletion_node)), deletion_node))
  }
  
  compare_networks(sub_graph, new_graph, intervention_node)
  
  return(sub_graph)
}


ui <- fluidPage(
  title = "Intervention Methods",
  fluidRow(
    column(4,
           selectizeInput(
             inputId = "InterventionMethod", 
             label = "Choose Intervention Method",
             multiple = FALSE,
             choices = c("Select Intervention Method" = "", "Remove Node", "Add Edge"),
             options = list(
               create = FALSE,
               placeholder = "Select an Intervention Method",
               maxItems = '1'
             )
           ),
           selectizeInput(
             inputId = "InterventionNode", 
             label = "Actor of Interest",
             multiple = FALSE,
             choices = c("Select Node" = "", igraph::V(graph_knowledge)$name),
             options = list(
               create = FALSE,
               placeholder = "Select an Actor of Interest",
               maxItems = '1'
             )
           ),
           selectizeInput(
             inputId = "DeletionNode", 
             label = "Deletion Node",
             multiple = TRUE,
             choices = c("Select Node(s)" = "", igraph::V(graph_knowledge)$name),
             options = list(
               create = FALSE,
               placeholder = "Select Deletion Node(s)",
               maxItems = NULL
             )
           ),
        htmlOutput("before_bc_display"),
        htmlOutput("after_bc_display"),
        htmlOutput("overall_bc_display")
    ),
    column(8,
           plotOutput(outputId = 'plot1')
    )
  )
)

server <- function(input, output, session) {
  # Show Selected Value in Console
  observe({
    print(input$InterventionMethod)
  })
  
  observe({
    print(input$DeletionNode)
  })
  
  output$plot1 <- renderPlot({
    if (!is.null(input$InterventionNode) && input$InterventionNode != "") {
      if (!is.null(input$DeletionNode) && input$DeletionNode != "") {
        simulate_intervention(graph_knowledge,
                              input$InterventionNode,
                              input$DeletionNode,
                              input$InterventionMethod)
      } else {
        plot(zoom_in_network(graph_knowledge, input$InterventionNode),
             vertex.size = 8,
             edge.size =4/32,
             edge.arrow.size = 0.2,
             vertex.color = "blue",
             vertex.label.cex = 0.4,
             vertex.label.color = 'black',
             vertex.frame.color = "#ffffff",
             vertex.label = NA,
             layout = igraph::layout.fruchterman.reingold)
      }
    }
  })
    
    observe({
      if (!is.null(input$InterventionNode) && input$InterventionNode != "") {
        before_graph <- zoom_in_network(graph_knowledge, input$InterventionNode)
        after_graph <- simulate_intervention(graph_knowledge, input$InterventionNode, input$DeletionNode, input$InterventionMethod)
        
        before_bc <- centralization.betweenness(before_graph)$centralization
        after_bc <- centralization.betweenness(after_graph)$centralization
        overall_bc <- centralization.betweenness(graph_knowledge)$centralization
        
        output$before_bc_display <- renderText({
          paste("Before Intervention - Betweenness Centralization:", before_bc)
        })
        
        output$after_bc_display <- renderText({
          paste("After Intervention - Betweenness Centralization:", after_bc)
        })
        
        output$overall_bc_display <- renderText({
          paste("Overall Graph Knowledge - Betweenness Centralization:", overall_bc)
        })
      }
    })
}


shinyApp(ui, server)



