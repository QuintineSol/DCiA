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
library(intergraph)
library(sna)
library(ggplot2)
library(english)
library(shinyalert)
library(markdown)

library(gemini.R)

setAPI("AIzaSyDLTNI3rcaATalpKe9q1PDbAQEAb5BGujQ") # check https://makersuite.google.com/app/apikey
#source(paste0(dirname(rstudioapi::getSourceEditorContext()$path), '/Comparison_script.R'))
source('Comparison_script.R')


# Setting the Hugging Face API key (ensure this is securely managed in production)
#Sys.setenv(HUGGINGFACE_API_KEY = "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")

# Define the User Interface
ui <- dashboardPage(
  title = "NetWORKHORSE",
  dashboardHeader(
    title = div(
      style = "display: flex; align-items: center; max-width: 230px; padding-right: 10px;",
      img(src = "workhorse.png", height = "30px", style = "margin-right: 5px; flex-shrink: 0;"),
      tags$span("NetWORKHORSE", style = "font-size: 16px; color: #FFFFFF; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;")
    ),
    titleWidth = 250
  ),
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                menuItem("Introduction", tabName = "introduction"),
                menuItem("Upload Data", tabName = "data_upload"),
                menuItem("Explore Network", tabName = "dashboard"),
                menuItem("Critical Connections", tabName = "cug_test"),
                menuItem("Find Communities", tabName = "community_detection"),
                menuItem("Compare Networks", tabName = 'network_comparison'),
                menuItem("Export Data", tabName = "data_export")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .skin-blue .main-header .logo {
        //background-color: #1E90FF;
        //color: #FFFFFF;
        font-size: 20px;
        font-weight: bold;
      }
      .skin-blue .main-header .navbar {
        //background-color: #1E90FF;
      }
      .previous-button { 
        position: fixed; 
        top: 55px; 
        z-index: 1050; 
      }
      .cheatsheet-button, .next-button { 
        position: fixed; 
        top: 55px;
        z-index: 1050; 
      }
      previous-button {
        left: 250px;
      }  
      .cheatsheet-button {
        right: 80px;
      }
      .next-button {
        right: 20px;
      }
      .shinyalert-content { 
        max-width: 600px; 
      }
    "))),
    tags$script(HTML("
      $(document).on('shiny:connected', function(event) {
        function adjustButtonPosition() {
          // Measure the width of the sidebar
          // var sidebarWidth = $('.main-sidebar').width();
          if ($('body').hasClass('sidebar-collapse')) {
            // If the sidebar is collapsed, set the button's left margin to a minimal
            $('.previous-button').css('left', '10px');
          } else {
            // If the sidebar is not collapsed, adjust according to its width
            $('.previous-button').css('left', 250 + 'px');
          }
        }
        
        // Initial call to set the button's position correctly when the app loads
        adjustButtonPosition();
        
        // Detects when the sidebar menu has been expanded or collapsed
        $('.sidebar-toggle').click(function() {
          // Wait briefly for sidebar transition to complete
          setTimeout(function() {
            adjustButtonPosition();
          }, 500); // Adjust timing if necessary
        });
    
        // Adjusts button position upon window resizing
        $(window).resize(function() {
          adjustButtonPosition();
        });
      });
    ")),
    uiOutput("prevButtonUI"),
    uiOutput("nextButtonUI"),
    uiOutput("cheatsheetButtonUI"),
    tabItems(
      # tabItem(tabName = "introduction",
      #         h1("Welcome!", align = "center"),
      #         h3("Introduction:", align = "center"),
      #         p("Welcome to our interdisciplinary collaboration tutorial! This tutorial aims to provide researchers in the fields of digital design, marketing, and music with a user-friendly and non-technical introduction to understanding network data. By exploring the connections and dynamics within your research networks, we hope to empower you to enhance interdisciplinary collaboration and innovation within your respective domains.", style = "text-align: justify; padding: 20px;"),
      #         p("Throughout the application, you'll come across 'Gemini Explanations' on certain pages. These explanations provide insights into the information presented on each page, utilizing Google's powerful large language model, Gemini. Think of it as Google's equivalent of ChatGPT. It's worth noting that these explanations are dynamic, meaning they change based on your input, similar to how ChatGPT's responses can change.", style = "text-align: justify; padding: 20px;"),
      #         p("You can access a comprehensive statistical cheatsheet by simply clicking on the button provided.", style = "text-align: justify; padding: 20px;")
      # ),
      tabItem(tabName = "introduction",
              fluidRow(
                column(width = 12,
                       wellPanel(
                         div(style = "text-align: center;",  # Center the logo
                             img(src = "networkhorse.png", height = "300px", alt = "NetWORKHORSE Logo")
                         ),
                         # h1(tags$strong("NetWORKHORSE"), align = "center"),
                         h3(tags$strong("Your Steed in the Race for Innovation"), align = "center"),
                         p("Welcome to NetWORKHORSE, your advanced tool for pioneering generative AI research through interdisciplinary collaboration. Tailored for researchers across diverse academic fields such as digital design, marketing, and music, NetWORKHORSE demystifies social network analysis (SNA), offering a user-friendly and non-technical introduction to understanding network data."),
                         p("At the core of NetWORKHORSE is a dedication to enhancing your research capabilities and fostering innovative collaborations. This tool aids you in navigating through the intricate networks of academic and industry partnerships, offering insights and highlighting potential pathways for pioneering collaborative projects. Whether you are identifying key players within your network or seeking out new collaborative opportunities, NetWORKHORSE provides robust analytics and intuitive visualizations designed to simplify complex data interactions and improve your research outcomes."),
                         tags$ul(
                           tags$li(tags$strong("Effortless Integration: "), "Just mount and ride — NetWORKHORSE manages the intricate details of SNA, allowing you to focus on the insights."),
                           tags$li(tags$strong("Guided Discovery: "), "You’re not navigating your research journey alone. This tool guides you step-by-step, illuminating the complexities of network data to ensure a seamless exploration experience."),
                           tags$li(tags$strong("Powerful Analysis Made Simple: "), "Engage with dynamic and powerful analytics and visualizations that make complex data interactions comprehensible, regardless of your expertise level.")
                         ),
                         p("As you delve into NetWORKHORSE, you'll discover its multiple interactive tabs, each designed to enhance your research experience. Two consistent elements you'll find throughout are ", tags$strong("'Gemini Explanations'"), " and a handy ", tags$strong("'Cheatsheet'"), ". Gemini leverages Google's large language model — think of it as Google's equivalent of ChatGPT — provides you adaptive insights to enhance your understanding of the data. Tailored to your input, it supports you in making informed decisions based on sophisticated AI-generated insights. The 'Cheatsheet' offers a comprehensive overview of any statistical jargon you may encounter, accessible with a simple click on the button provided in the top-right corner."),
                         p("Embark on your journey with NetWORKHORSE now, and transform how you engage with interdisciplinary research. This tool is here to ensure that your exploration of network dynamics is both enlightening and productive. Enjoy the ride!"),
                         style = "text-align: justify; padding: 20px;")
                )
              )
      ),
      tabItem(tabName = "data_upload",
              fluidRow(
                column(width = 12,
                       h3("Upload Data", align = "center"),
                       p("Please upload an edgelist in CSV or Excel format
                         (.csv, .xlsx, .xls). The edgelist should represent the connections in your network, with each row indicating an edge between two nodes."),
                       fileInput('file1', 'Choose CSV/Excel File', accept = c('.csv', '.xlsx', '.xls')),
                       DT::dataTableOutput("dataTable"),  # Renders the uploaded data table
                       textOutput("userDatasetDescription")  # Display user's description
                )
              )
      )
      ,
      tabItem(tabName = "dashboard",
              fluidPage(
                column(width = 12, 
                       div(style = "background-color: #f8f8f8; border: 1px solid #ddd; padding: 20px; border-radius: 5px;",
                           h3("Explore Network", align = "center"),
                           p("Now that a network has been imported, it is time to get a better understanding of its characteristics. The current page allows for a quick and comprehensive overview of your network through a variety of statistics, findings, and visualizations. The dashboard is designed to empower the laymen that have access to network data. Getting a better understanding of the network starts with a visual inspection of its structure. Therefore, the network is displayed in the interactive visualization below.")
                       ),
                       withSpinner(visNetworkOutput("networkPlot1", height = "350px"), type = 4),
                       p("Although visualizing the network serves as a useful method to obtain a holistic view of the network's structure and connections, it only scratches the surface of what can be discovered. Through further exploration with statistical measures and thus representing network characteristics as numbers, we can extract meaningful patterns, trends, and relationships that may not be immediately apparent from the visualization alone. These insights can help us better understand the underlying dynamics of the network, identify key nodes or clusters, detect anomalies or trends over time, and make informed decisions to optimize network performance or address specific challenges. Explanations of the network statistics are defined in the statistical cheatsheet."),
                       fluidRow(
                         column(width = 4, 
                                plotOutput("CountPlot", width = "100%", height = "300px")
                         ),
                         # column(width = 3, 
                         #        plotOutput("ScorePlot", width = "100%", height = "300px")
                         #        ),
                         column(width = 4, 
                                withSpinner(plotOutput("DistancePlot", width = "100%", height = "300px"), type = 4)
                         ),
                         column(width = 4,
                                plotOutput("CentralizationPlot", width = "100%", height = "300px"))
                       ),
                       hr(),
                       
                       fluidRow(
                         column(width = 6,
                                div(style = "display: flex; justify-content: center;",
                                    div(style = "width: 85%; background-color: #1bbbff; border: 1px solid #ddd; padding: 16px; border-radius: 5px; color: white;",
                                        div(style = "display: flex; align-items: center; justify-content: space-between;",
                                            h3(style = "font-size: 16px; font-weight: bold; margin: 0;", "Transitivity:"),
                                            h3(style = "font-size: 16px; font-weight: bold; margin: 0;", textOutput("transitivityOutput"))
                                        ),
                                        column(width = 9,
                                               p(style = "font-size: 14px; margin-top: 10px; margin-bottom: 0;", "The transitivity of the network measures the likelihood of two individuals who are connected to the same person in the network are also directly connected to each other. Higher transitivity values indicate a higher tendency for actors to connected with the 'friend of a friend'."),
                                        ),
                                        column(width = 3,
                                               div(style = "font-size: 14px; margin-top: 10px; margin-bottom: 0; white-space: nowrap;",
                                                   p(style = "margin-right: 10px;", "Low: < 0.2"),
                                                   p(style = "margin-right: 10px;", "Average: 0.2 - 0.4"),
                                                   p("High: > 0.4"),
                                               ),
                                        ),
                                        div(style="text-align:center",
                                        column(width= 12,
                                               tags$img(src='https://i.imgur.com/Pb2x05v.jpeg', width = '71%'), align="center")
                                    )
                                    )
                                )
                         ),
                         column(width = 6,
                                div(style = "display: flex; justify-content: center;",
                                    div(style = "width: 85%; background-color: #FF69B4; border: 1px solid #ddd; padding: 16px; border-radius: 5px; color: white;",
                                        div(style = "display: flex; align-items: center; justify-content: space-between;",
                                            h3(style = "font-size: 16px; font-weight: bold; margin: 0;", "Density:"),
                                            h3(style = "font-size: 16px; font-weight: bold; margin: 0;", textOutput("densityOutput"))
                                        ),
                                        column(width = 9,
                                               p(style = "font-size: 14px; margin-top: 10px; margin-bottom: 0;", "Density represents the proportion of actual connections in a network relative to the total number of possible connections. It reflects the extent of the connections or interaction between individuals, with higher values indicating many collaborative opportunities. "),
                                        ),
                                        column(width = 3,
                                               div(style = "font-size: 14px; margin-top: 10px; margin-bottom: 0; white-space: nowrap;",
                                                   p(style = "margin-right: 10px;", "Low: < 0.1"),
                                                   p(style = "margin-right: 10px;", "Average: 0.1 - 0.3"),
                                                   p("High: > 0.3"),
                                               ),
                                        ),
                                        column(width= 12,
                                        tags$img(src='https://i.imgur.com/4D5HTL5.jpeg', width = '100%'), align="center")
                                    )
                                )
                         ),
                                
                       
                       ),
                       hr(),
                       div(style = "background-color: #f8f8f8; border: 1px solid #ddd; padding: 16px; border-radius: 5px;",
                           h3("Discover Distributions", align="center"),
                           p("The following section delves into visualizations of distributions. There are various graphical representations that can be used to portray distributions. In statistics, distributions refer to the frequency of possible values or outcomes occurring in a dataset.  In this regard, distributions can also be used to obtain valuable insights into the overall structure and characteristics of the network. The most common visualizations of distributions are histograms and boxplots, which are implemented below for inspection. There is a bit more flexibility here compared to previous visualizations. It means that can choose the centrality measure of interest, allowing you to gain a deeper understanding of its relevance in the provided network. Do not hesitate to refer to the statistical cheatsheet!"),
                       ),
                       hr(),
                       div(
                         p("When evaluating visualizations of distributions in the form of histograms and boxplots there are distinct key points of interest."),
                         tags$ul(
                           tags$li("Try to examine the shape and spread of the distribution. For example, a centrality measure of interest might exhibits a shape that is primarily right-skewed, with the bulk of the values situated on the left and a tail on the right. This often suggests that there the network is subject to a hierarchical differences between the actors in the network."),
                           tags$li("Assess the shape and spread of the distribution. The shape serves as the  overview of the distributions. However, the shape is dictated by the ranges and scales presented in the visualization. The spread thus helps interpreting the distribution. Especially, both extremes of the x-axis, the minimum and maximum value, are interesting in this regard. "),
                           tags$li("Lastly, these visualizations, in particular the boxplot, excel in highlighting outliers in the network. Outlying actor in this context represents those actors that exert significantly difference connectivity patterns compared to the majority of the actors. The boxplot visualizes the outliers with box dots."),
                         ),
                       ),
                       fluidRow(column(width = 4,
                                       h4("Histogram"),
                                       p("The left-hand visualization represents a histogram. Histograms offer a comprehensive view of the distribution of node-level measures or centralities across the entire network. By visualizing the frequency of values within predefined bins, histograms enable you to identify the range, skewness, and outliers of the distribution."),
                                       selectInput("centrality", "Choose the centrality measure of interest:",
                                                   choices = c("Degree", "Betweenness", "Closeness", "Eccentricity"),
                                                   selected = "Degree"),
                       ),
                       column(width = 8,
                              h4("Boxplot"),
                              div(
                                p("Compared to a histogram, a boxplot is can be considerd more statistically-driven. Hence, the boxplot requires more explanation. It aims to visualize a summary of key statistical characteristics of the centrality measure distribution. The main components of the boxplot are:"),
                                tags$ul(
                                  tags$li(strong("The box: "),"This represents the middle 50% of the values proportion, known as the interquartile range (IQR). The bottom of the box is the 25th percentile (Q1) and the top is the 75th percentile (Q3). The line in the middle of the box is the median (Q2), which divides the data in half. The placement of the box indicates skewedness."),
                                  tags$li(strong("The whiskers: ")," These lines extending from the box show the range of the data, up to 1.5 times the midspread. Any values beyond the whiskers are considered potential outliers."),
                                  tags$li(strong("The outliers: "), "Any data points that fall outside the whiskers are plotted individually as outliers. These are values that are unusually high or low compared to the rest of the distribution."),
                                ),
                              ),
                       ),
                       ),
                       fluidRow(
                         column(width = 6, 
                                withSpinner(plotOutput("DistPlot", width = "100%", height = "350px"), type = 4)
                         ),
                         column(width = 6, 
                                plotOutput("BoxPlot", width = "100%", height = "350px")
                         ),
                         
                       ),
                       hr(),
                       fluidRow(
                         column(width = 4,
                                div(style = "background-color: #f8f8f8; border: 1px solid #ddd; padding: 16px; border-radius: 5px;",
                                    h3("Most Important Actors"),
                                    p("One of the most insightful information about the network is are the most important actors. Importance in this context is subjective. For instance, one might find a popular actor with many connections the most important, whereas others define them as those actors that connect faculties. In other words, your definition of importance is dictated by your interest. This interest can be encapsulated by a centrality measure. In the following section, you can choose a centrality measure to base the most important actors on. Choose wisely! "),
                                ),
                                selectInput('ActorMetricD', 'What statistic should be used to determine the most important actors?', 
                                            choices = c('Degree','Closeness','Betweenness'), selected = 'Degree'),
                                numericInput('ActorNumD', 'How many actors should be retrieved?', 5, min = 1, max = NA)
                         ),
                         column(width = 8,
                                withSpinner(DT::dataTableOutput("ActorTable"), type = 4)
                         )
                       )
                )
              )

      ),
      tabItem(tabName = "cug_test",
              fluidPage(
                column(
                  width = 12,
                  h3("Critical Connections", align = "center"),
                  p("This page allows you to see how important certain connections are in your provided network data. It helps us identify the most important 'bridges' in our network, the ones that connect different parts of the network together."),
                  p("Imagine you have a big group of researchers and stakeholders from different fields. By using the Conditional Uniform Graph (CUG) test, you can figure out where there might be gaps in communication or places where people aren't working together as well as they could be. This helps to create better networks between researchers and stakeholders, which leads to more innovative ideas and better understanding of how generative AI can impact different industries."),
                  actionButton("runCUG", "Run CUG Test"),
                  h4("CUG Test Results"),
                  withSpinner(verbatimTextOutput("cugTestOutput"), type = 4),
                  h4("Static Explanation"),
                  p("Univariate Conditional Uniform Graph Test: This means we're looking at how important individual connections are in the network."),
                  p("Conditioning Method: This tells us what part of the network we're focusing on. In this case, we're looking specifically at the connections between different nodes (researchers) in the network."),
                  p("Diagonal Used: This tells us if we're including cases where researchers are connected to themselves."),
                  p("Replications: This just says how many times we repeated the test to make sure our results are reliable."),
                  p("Observed Value: This is what we actually found when we looked at the network. It tells us how important the connections are."),
                  p("Pr(X>=Obs): This tells us how likely it is that the results we found are just random chance, meaning that the."),
                  p("Pr(X<=Obs): This tells us how likely it is a result we got unusual compared to what we might expect by chance. This would therefore meaning that the Observed Value is statistically significant. This suggests that there is a real and meaningful pattern in the network data."),
                  h4("Gemini Explanation"),
                  uiOutput("Gemini_cug_explanation")
                )
              )
      ),
      tabItem(tabName = "community_detection",
              fluidPage(
                h3("Find Communities", align = "center"),
                div(
                  p("In order to detect the communities present in your network you can make use of several different algorithms. To help you selecting the most appropriate one we have included a brief explanation of each. Here is the run-down:"),
                  tags$ul(
                    tags$li(strong("Fast Greedy: "), "this algorithm quickly groups together researchers based on their collaboration patterns. Imagine you have a large group of researchers from different fields. Fast Greedy starts by considering each researcher separately and then combines them into clusters, choosing combinations that create the most cohesive groups. For example, if researchers want to foster interdisciplinary collaborations within the organization, they can use Fast Greedy to efficiently identify and capitalize on the existing but perhaps hidden collaborative ties and complementary strengths within their research community."),
                    tags$li(strong("Louvain: "), "this algorithm sorts researchers into subsets where each subset contains researchers who collaborate more closely with each other than with researchers in other subsets. It starts by placing each researcher in their own subset and then merges them to maximize how well the researchers are divided (i.e., their modularity). This method is great for handling large datasets and quickly finding an optimal division. For instance, if the organization wants to identify groups of researchers with similar research interests or methodologies, they can use Louvain to efficiently categorize them into clusters."),
                    tags$li(strong("Girvan-Newman: "), "this algorithm focuses on identifying critical connections between research communities within the organization. It gradually separates the network into distinct groups by removing connections that are important for maintaining the overall structure of collaboration (using measures like betweenness: how often a connection lies on the shortest path between pairs of items). For example, if the organization wants to analyze collaboration networks among academic departments or research teams, Girvan-Newman can help identify key connections that bridge different research fields or groups."),
                    tags$li(strong("Walktrap:"), "this algorithm is inspired by the idea of random walks within a network, Walktrap discovers groups of closely connected researchers. It identifies clusters based on the strength and density of collaborations between researchers (it uses the length and frequency of random walks within groups). For example, if the organization wants to explore natural groupings within their research collaboration network, Walktrap can help identify clusters of researchers with strong interconnections, facilitating the discovery of new interdisciplinary research opportunities.")
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
                h4("Gemini Explanation"),
                uiOutput("Gemini_comm_explanation")
              ),
      ),
      tabItem(tabName = 'network_comparison',
              fluidPage(
                h3("Network comaprison Page", align = "center"),
                p('This tab will be focused on the comparison of two different networks, thus, please upload a second network to compare to. This network should meet the following requirements:'),
                # Add requirements based on import
                fileInput('file2', 'Choose CSV/Excel File', accept = c('.csv', '.xlsx', '.xls')),
                DT::dataTableOutput("dataTable2"),  # Renders the uploaded data table
                h4('Correlation between the networks'),
                p('The correlation between both networks will be computed using the ',strong('QAP method'),', as explained in the Cheatsheet. Before this is performed the function makes sure that the nodes in both of the networks match by adding missing nodes as isolates to the other network.'),
                p('Therefore, the number of repetitions of scrambled networks which are done in the simulation is a very important metric, as this determines how good the results represent the actual situation. These repetitions come with a trade-off in the time it takes before the model is completed, which is highly dependent on the size and complexity of each network. Thus, the exact optimal value differs for each network. Therefore, it is advised to start with a lower number, like 100, and build up slowly such that the best balance between execution time and accuracy can be found.'),
                numericInput('QAPreps', 'Number of repetitions:', 100, min = 10, max = 10000),
                p('The output will show two different representations of the same result. At first, the results will be shown in textual format. In here, first the', strong('test statistic'), 'will be shown, which represents the correlation observed between both networks. After this the text will output two estimated p-values. Which one is relevant is dependent on the chosen hypothesis:'),
                tags$ul(
                  tags$li('If the test is whether the correlation between the networks is', strong('higher'), 'than in the randomly observed networks, p(f(perm) >= f(d)) is the relevant p-value'),
                  tags$li('If the test is whether the correlation between the networks is', strong('lower'), 'than in the randomly observed networks, p(f(perm) <= f(d)) is the relevant p-value')
                ),
                p('Using this', strong('p-value'), 'it can then be decided whether the null hypothesis can be rejected or not'),
                actionButton('QAPAnalysis', 'Run Analysis'), 
                HTML('<h5><b>Output:</b></h5>'),
                verbatimTextOutput('QAP'),
                conditionalPanel("input.QAPAnalysis > 0",p('Next to the textual output, there is also a visual output of the results. Using the test statistic found in the textual output, we can compare it with the values of the random networks. These are displayed underneatch, with the dotted line representing the', strong('test statistic'), '(in special cases, the test statistic is so big or small that it will not show in the graph) and the other line representing how the values of the random networks are distributed. If the test statistic is all the way to the right, it indicates that the test statistic is significantly bigger than the random networks, whilst if it is all the way to the left, it is significantly smaller than the random observations. It can also be the case that the test statistic is in the middle of the observations. In this case, the statistic is likely to stem from randomness, and thus, there is no special correlation between the networks.')),
                withSpinner(plotOutput('QAPplot'), type = 4),
                p('After these results are obtained there are 3 possible outcomes:'),
                tags$ul(
                  tags$li('The test statistic is', strong('significantly higher'), 'than the random observations: There is a high correlation between the networks, thus, actors (not) connected in one network are also likely (not) to be connected to each other in the other network.'),
                  tags$li('The test statistic is', strong('significantly lower'), 'than the random observations: There is a high correlation between the networks, thus, actors connected in one network are likely not to be connected to each other in the other network .'),
                  tags$li('The test statistic is', strong('not significantly different'), 'from the random observations: In this case there no correlation between the networks, thus, the networks are unlikely to effect each other')
                ),
                p('These results come with the caveat that correlation does not mean causation, meaning that if two networks are correlated, it does not directly mean that the ties in one network are the causes for ties in the other network. To be able to establish this, more research should be done based on the methodologies presented in An, et al. (2022)', tags$sup('1')),
                h4('Statistical differences between the networks'),
                p('There are 2 different network measures which currently can be compared across both networks:'),
                tags$ul(
                  tags$li(strong('Mean degree centrality:'),'As said in the cheatsheet, this compares the mean number of ties each node in the network has. This can for instance be used to check whether an intervention has led to an increase in the number of ties across the network'),
                  tags$li(strong('Mean closeness centrality:'),'As said in the cheatsheet, this compares the mean distance each node has to all the other connected nodes. This can for instance be used to check whether an intervention has led to actors needing less people to reach each other, thus leading to a more closely connected network')
                ),
                p('The networks will be tested by the Mann-Whitney U Test',tags$sup('2') ,', which is a test that can be used to see whether the means of two sets of samples are statistically different. The', strong('null hypothesis'), 'for this test is that the means of the values in both networks do not significantly differ.'),
                selectInput('statisticChoice', 'What statistics should be compared in the networks?', 
                            choices = c('Degree', 'Closeness'), multiple = T),
                actionButton('StatCompare', 'Run Analysis'),
                p('First, the results of the test(s) will be shown. Here the', strong('p-value'), 'can be observed and a standard p-value of 0.05 is used to make conclusions on the networks. From these', strong('p-values'), 'it can be observed whether or not the intervention had any effect, in case of the aforementioned examples.'),
                p('For more information on', strong('p-values'), ', see the cheatsheet above, for an explanation'),
                verbatimTextOutput('Stats'),
                p('After this the distributions of the selected measures will be shown in graph form. From these outputs, no statistal inferences can be made. However, they often give a clearer overview on how certain distributions have changed and can form a beginning for future research as to why the values have changed in the way they did.'),
                conditionalPanel("input.statisticChoice.includes('Degree')", withSpinner(plotOutput('degree_plot'), type = 4)),
                conditionalPanel("input.statisticChoice.includes('Closeness')", withSpinner(plotOutput('closeness_plot'), type = 4)),
                conditionalPanel("input.statisticChoice.includes('Betweenness')", withSpinner(plotOutput('betweenness_plot'), type = 4)),
                h4('Most important actors in both networks:'),
                p('One of the most insightful information about the network is are the most important actors. However, what might be more useful to see is whether these important actors change over time or change between different types of networks. For different types of networks this could mean that someone who is thought of as less important for the integrity of one network, could be devestated to lose for the connectivity of another network. Furthermore, if the most important actor changes over time, this could signal whether an intervention to better distribute the connections within the network is working or not.'),
                p('As the definition of importance can differ based on the research question, there are multiple metrics which can be chosen as the metric of importance. These are:'),
                tags$ul(
                  tags$li(strong('Degree centrality:'),'As said in the cheatsheet, this uses the number of ties of each node to be able to determine the most important actors. Using this metric, the most important actors will be those who have the most connections to other and thus can be used to see whether the actor with the most connections changes between both networks.'),
                  tags$li(strong('Betweenness centrality:'),'As said in the cheatsheet, this measures on how many shortest paths each node lies. This metric can therefore be used to be able to see whether the nodes which lie on the most shortest paths have changed. This can for instance be used to see whether the nodes with the most important broker position changed between both networks.'),
                  tags$li(strong('Closeness centrality:'),'As said in the cheatsheet, this metric shows how close all nodes are to all other connected nodes in the network. This metric can therefore be used which nodes are the closest to all other connected nodes in the network. This comparison can thus be used to see whether the nodes which have the closest connections to all others have changed between both networks')
                ),
                p('This metric will then be used in both networks to be able to retrieve the most important actors. The number of actors which should be retrieved can also be specified underneath.'),
                selectInput('ActorMetric', 'What statistic should be used to determine the most important actors?', 
                          choices = c('Degree','Closeness','Betweenness'), selected = 'betweenness'),
                numericInput('ActorNum', 'How many actors should be retrieved', 5, min = 1, max = NA),
                actionButton('ActorComparison', 'Run Analysis'),
                p('Here it can be seen which actors are the most important in both networks based on the chosen metric. At first, the most important actors in each network will be presented, after which the values of the metric which they were selected upon will be shown.'),
                verbatimTextOutput('ActorCompText'),
                withSpinner(DT::dataTableOutput('DTActorComp'), type = 4),
                h4('Most important ties in both networks:'),
                p('Finally, the networks can be compared based upon the ties which they have. These ties will be ranked using either of the following two metrics:'),
                tags$ul(
                  tags$li(strong('Absolute method:'),'The absolute method will determine the most important ties by the distance increase between both nodes on the each of the tie. Therefore, this metric solely depends on the shortest path after the tie has been "removed", meaning that the information on the importance of the tie for other nodes in the network will be lost.'),
                  tags$li(strong('Mean method:'),'This method will determine the most important ties based on the increase in mean distance between all the sets of nodes in the entire network. This will therefore take into account how important the tie is for other nodes, however, this might mean that ties which are important for a small collection of nodes are flooded by ties which have a smaller impact on a big group of nodes.')
                ),
                p('Next to these metrics, there two more choices which can be selected:'),
                tags$ul(
                  tags$li(strong('Only local bridges should be considered:'),'This will mean that only local bridges (ties which increase the distance between two nodes by 2 or more if removed) will be considered in the output. This mainly has effect on the mean method, as for the absolute method, this can be read directly from the obtained statistic. This option can be used if the focus should be higher on ties which have a considerable impact on the nodes surrounding a specific tie.'),
                  tags$li(strong('Infinite values should be discarded:'),'The result of this option is very logical, whether infinite values should be considered in the output or not. These infinite values originate from ties which are the only connection between two nodes in the absolute method. These ties can however be a very important indication of where extra ties should be created, as a removal of this tie can have big consequences for the network and the real-life dynamics which arise from it.')
                ),
                selectInput('BridgeMetric', 'What statistic should be used to determine the most important ties?', 
                          choices = c('Absolute', 'Mean'), selected = 'mean'),
                numericInput('BridgeNum', 'How many ties should be retrieved', 5, min = 1, max = NA),
                checkboxGroupInput('BridgeVars', 'Extra choices', 
                                 choices = c('Only local bridges should be considered' = 'Bridges', 'Infinite values should be discarded' = 'drop.infs')),
                actionButton('BridgeComparison', 'Run Analysis'),
                p('Here it can be seen which ties are the most important in both networks based on the metric and other options which were chosen by the user. At first, the most important ties in each network will be presented, after which the values of the metric which they were selected upon will be shown.'),
                verbatimTextOutput('BridgeCompText'),
                withSpinner(DT::dataTableOutput('DTBridgeComp'), type = 4),
                p('   '),
                p(tags$sup('1'), 'An, W., Beauvile, R., & Rosche, B. (2022). Causal network analysis. Annual Review of Sociology, 48, 23-41.', tags$a('As seen here', href = 'https://www.annualreviews.org/docserver/fulltext/soc/48/1/annurev-soc-030320-102100.pdf?expires=1714399219&id=id&accname=guest&checksum=20AF90A77ADC63CB9E6D454E956302F1')),
                p(tags$sup('2'), 'For a more detailed explanation on the Mann-Whitney U test', tags$a('see the following page.', href = 'https://www.technologynetworks.com/informatics/articles/mann-whitney-u-test-assumptions-and-example-363425'))
              )
      ),
      
      tabItem(tabName = "data_export",
              fluidPage(
                h3("Export Data", align = "center"),
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
  user_dataset_text <- reactiveVal(NULL)  # Store user's description
  
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
    showModal(modalDialog(
      title = "Dataset Description",
      textInput("datasetDescription", "Please write in three sentences what this dataset is about:"),
      easyClose = FALSE,
      footer = tagList(
        actionButton("confirmDescription", "Confirm"),
        modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$confirmDescription, {
    user_dataset_text(input$datasetDescription)
    removeModal()
    
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
  
  # Dynamically render the "Previous" button
  output$cheatsheetButtonUI <- renderUI({
    actionButton("cheatsheet", "Cheatsheet", class = "cheatsheet-button btn btn-primary")
  })
  
  # Function to navigate to the next tab
  observeEvent(input$cheatsheet, {
    shinyalert(
      title = "Statistical Cheatsheet",
      text = 
        "<div style='text-align: left;'>
          <h3>General Statistical Terms</h3>
          <p><b>Metrics / Measures:</b> Quantitative measures used to describe characteristics of data, such as averages, variability, and relationships.</p>
          <p><b>Distributions:</b> Patterns showing how data is spread out, like bell curves or uneven distributions.</p>
          <p><b>Skewedness:</b> A way to describe the shape of the data distribution, indicating whether it's asymmetrical or balanced.</p>
          <p><b>Outliers:</b> Unusual data points that stand apart from the rest, potentially indicating errors or unique cases.</p>
          <p><b>Median:</b> The middle value in a dataset when arranged in ascending order, separating the higher and lower halves.</p>
          <p><b>Mode:</b> The most frequent value in a dataset, representing the data point that occurs most often.</p>
          <p><b>Minimum and Maximum:</b> The smallest and largest values in a dataset, respectively, defining the range of possible values.</p>
          <p><b>Quantile:</b> A value that divides a dataset into equal-sized intervals. For example, the median is the quantile that divides the data into two equal halves.</p>
          <p><b>Quartile:</b> Three values that divide a dataset into four equal parts. The first quartile (Q1) is the value below which 25% of the data falls, the second quartile (Q2) is the median, and the third quartile (Q3) is the value below which 75% of the data falls.</p>
          <p><b>Percentile:</b> A value that indicates the percentage of data points that are less than or equal to it. For example, the 75th percentile (Q3) is the value below which 75% of the data falls.</p>
          <p><b>Interquartile Range (IQR):</b> A measure of statistical dispersion, calculated as the difference between the third quartile (Q3) and the first quartile (Q1). It represents the spread of the middle 50% of the data.</p>
          <p><b>Infinite Values:</b> Infinite values are values that are infinitely large or small, often indicating outliers or extreme values
          
          <h3>Hypothesis Testing</h3>
          <p><b>Null Hypothesis (H0) & Alternative Hypothesis (H1):</b> In a statistical test, the null hypothesis is a statement that there is no effect or relationship, while the alternative hypothesis is the statement we want to find evidence for. For example, in a drug trial, the null hypothesis might be that the drug has no effect, while the alternative hypothesis is that the drug does have an effect.</p>
          <p><b>P-value:</b> The p-value measures the probability that the observation (or something even more extreme) can happen if the null hypothesis is true.
          <p><b>Significance Level:</b> The significance level, often denoted as α (alpha), is the threshold used to determine statistical significance. Commonly used significance levels include 0.05 and 0.01, corresponding to a 5% and 1% chance, respectively, of incorrectly rejecting the null hypothesis.</p>
        
          <h3>Network Terms</h3>
          <p><b>Nodes / Vertices:</b> Individual entities or points in a network, such as people in a social network or routers in a computer network.</p>
          <p><b>Edges:</b> Connections or links between nodes in a network, representing relationships, interactions, or flows.</p>
        
          <h3>Distance Terms</h3>
          <p><b>Radius:</b> The maximum distance from a central point to any point within a network or dataset, indicating its extent or reach.</p>
          <p><b>Mean Distance:</b> The average distance between all pairs of nodes in a network, showing the typical separation between nodes.</p>
          <p><b>Absolute Distance:</b> The distance between two nodes without considering direction or sign, measuring only the magnitude of the difference.</p>
          <p><b>Diameter:</b> The longest shortest path between any pair of nodes in a network, representing the maximum distance between nodes.</p>
      
          <h3>Network Metrics</h3>
          <p><b>Density:</b> Density measures how connected a network is, representing the proportion of actual connections to possible connections.</p>
          <p><b>Transitivity:</b> Transitivity, or clustering coefficient, measures the likelihood that two nodes connected to the same node are also connected to each other.</p>
          
          <h3>Centrality Measures</h3>
          <p><b>Betweenness Centrality:</b> Betweenness centrality measures the extent to which a node lies on the shortest paths between other nodes in the network.</p>
          <p><b>Closeness Centrality:</b> Closeness centrality measures how close a node is to all other nodes in the network, based on the length of its shortest paths to all other nodes.</p>
          <p><b>Degree Centrality:</b> Degree centrality measures the number of connections a node has in the network.</p>
          <p><b>Eigenvector Centrality:</b> Eigenvector centrality measures the importance of a node in the network, considering both the node's connections and the connections of its neighbors.</p>
        
          <h3>Community Detection</h3>
          <p><b>Communities:</b> Communities are groups of nodes in a network that are more densely connected to each other than to nodes outside the group.</p>
          <p><b>Modularity Score:</b> Modularity is a measure of the quality of the division of a network into communities, with higher values indicating a better division.</p>
          <p><b>Community Detection Algorithms:</b> Fast greedy, Louvain, Girvan-Newman, and Walktrap are algorithms used to detect communities in networks based on different criteria and approaches.</p>
          <ul>
            <li><strong>Fast Greedy:</strong>
              <ul>
                <li>This algorithm is like organizing a set of objects into clusters based on how closely they are related. Imagine you have a bunch of items that need to be grouped by similarity; the Fast Greedy method starts by considering each item in its own group. It then combines these groups step by step, each time choosing the combination that results in the most cohesive groups, until no further improvement is possible. This approach is fast and efficient, making it suitable for quickly finding a good grouping in large datasets where each item has many connections.</li>
              </ul>
            </li>
            <li><strong>Louvain:</strong>
              <ul>
                <li>This algorithm is akin to sorting a large collection into subsets where each subset contains items that are more similar to each other than to items in other subsets. It begins with each item in its own subset and iteratively merges these subsets to maximize 'modularity,' a measure of how well the collection is divided. The process continues until the modularity cannot be increased further, indicating that the items are grouped in an optimal way. This method is known for its ability to handle very large collections, quickly identifying an optimal division.</li>
              </ul>
            </li>
            <li><strong>Girvan-Newman:</strong>
              <ul>
                <li>This algorithm focuses on identifying the connections that are most critical for maintaining the overall structure of the network. It works by progressively removing these connections, which are identified through measures like 'betweenness' (a measure of how often a connection lies on the shortest path between pairs of items). This process gradually separates the network into distinct groups based on the connectivity between items. Although thorough, this method can be slower than others, especially for networks with a large number of items or connections.</li>
              </ul>
            </li>
            <li><strong>Walktrap:</strong>
              <ul>
                <li>This algorithm is inspired by the idea of random walks within a network to discover groups of closely related items. It posits that short random walks are likely to stay within the same group because the items within a group are more densely interconnected. By analyzing the paths taken during these walks, Walktrap identifies which items tend to cluster together. This approach is effective for revealing the natural grouping within networks based on the connectivity and density of the connections between items.</li>
              </ul>
            </li>
          </ul>
          <p><b>Local Bridges:</b> Local bridges are connections between nodes that bridge different communities in the network, facilitating communication between them.</p>
          
          <h3>Conditional Uniform Graph (CUG) Test</h3>
          <p><b>CUG Test:</b> The CUG test evaluates the importance of connections in a network, identifying whether there are any critical connections that bridge different parts of the network.</p>
          <p> &nbsp; </p>
          <p>The model does this by repeatedly creating random networks with the same number of nodes and number of edges, after which the important connections in these networks will be evaluated using the <b>Betweenness Centralization</b>, which shows how much the network deviates from the highest possible summed betweenness centrality values.</p>
          <p> &nbsp; </p>
          <p>Finally, the model will calculate this <b>Betweenness Centralization</b> for the network which was put into the model. The output value of this statistic will then be compared with the values in all of the simulated networks, after which the p-values will be based on the percentage of simulated networks which had smaller value and the percentage of simulated networks which showed a larger value for the <b>Betweenness Centralization</b>. The p-values can be interpreted in the following way:</p>
          <ul>
              <li> If we want to test whether there are <b>significantly more</b> important ties than in the random networks, we will use the proportion of random observations which were <b>smaller</b> than the observed value as the p-value </li>
              <li> If we want to test whether there are <b>significantly less</b> important ties than in the random networks, we will use the proportion of random observations which were <b>bigger</b> than the observed value as the p-value</li>
          </ul>
        
          <h3>QAP Test</h3>
          <p><b>QAP Test:</b> The QAP test compares a chosen network statistic between two different networks while controlling for dependencies among observations, helping to assess the similarity or difference between networks.</p>
          <p>Using the following two networks, the inner workings of the QAP test will be explained</p>
          <p> &nbsp; </p>
          <figure class='half'>
            <table>
              <tr>
                <td>
                  <p> The following network will be referenced by <i>Network 1</i>: &emsp;</p>
                  <img src='https://i.imgur.com/tT8eij5.png', width = '50%'>
                </td>
                <td>
                  <p> The following network will be referenced by <i>Network 2</i>: &emsp; </p>
                  <img src='https://i.imgur.com/sSS8wJ3.png', width = '50%'>
                </td>
              </tr>
            </table>
          </figure>
          <p> &nbsp; </p>
          <p> To be able to better show the workings, the networks need to be transferred to <b> Adjacency matrices</b>. These are tables which have <i>n</i> rows and <i>n</i> columns, where each row represents a node and and each column a node and <i>n</i> is therefore equal to the number of nodes in a network. The values of the table change from 0 to 1 based whether there is a connection between the node indicated by the row and the node indicated by the column. Thus, for instance, in network 1, the first row and 6th column will have a value of 1, due to the connection between nodes 1 and 6. Transfroming both of the example networks to these matrices, they will become:</p>
          <p> &nbsp; </p>
          <table>
            <tr>
              <td>
                <p> <b> Network 1: </b> </p>
                <table border='2' bgcolor = '#FFFFFF'>
                  <tr> 
                    <th></th>
                    <th>&ensp;Node 1&ensp;</th>
                    <th>&ensp;Node 2&ensp;</th>
                    <th>&ensp;Node 3&ensp;</th>
                    <th>&ensp;Node 4&ensp;</th>
                    <th>&ensp;Node 5&ensp;</th>
                    <th>&ensp;Node 6&ensp;</th>
                  </tr>
                  <tr>
                    <th>&ensp;Node 1&ensp;</th>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                  </tr>
                  <tr>
                    <th>&ensp;Node 2&ensp;</th>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                  </tr>
                  <tr>
                    <th>&ensp;Node 3&ensp;</th>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                  </tr>
                  <tr>
                    <th>&ensp;Node 4&ensp;</th>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                  </tr>
                  <tr>
                    <th>&ensp;Node 5&ensp;</th>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                  </tr>
                  <tr>
                    <th>&ensp;Node 6&ensp;</th>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                  </tr>
                </table>
              </td>
              <td>
              <p> &emsp;</p>
              </td>
              <td>
                <p> <b> Network 2: </b> </p>
                <table border='2'>
                  <tr> 
                    <th></th>
                    <th>&ensp;Node 1&ensp;</th>
                    <th>&ensp;Node 2&ensp;</th>
                    <th>&ensp;Node 3&ensp;</th>
                    <th>&ensp;Node 4&ensp;</th>
                    <th>&ensp;Node 5&ensp;</th>
                    <th>&ensp;Node 6&ensp;</th>
                  </tr>
                  <tr>
                    <th>&ensp;Node 1&ensp;</th>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                  </tr>
                  <tr>
                    <th>&ensp;Node 2&ensp;</th>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                  </tr>
                  <tr>
                    <th>&ensp;Node 3&ensp;</th>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                  </tr>
                  <tr>
                    <th>&ensp;Node 4&ensp;</th>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                  </tr>
                  <tr>
                    <th>&ensp;Node 5&ensp;</th>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                  </tr>
                  <tr>
                    <th>&ensp;Node 6&ensp;</th>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                  </tr>
                </table>
              </td>
            </tr>
          </table>
          <p> &nbsp; </p>
          <p> These tables will then be used in the process of the QAP model. The QAP essentially scrambles the first table over and over again. These scrambles are done by randomly altering the order of the nodes, which will be reflected accordingly in both the row and the column of the table. Thus, if the order after the scramble would be 4-1-3-5-2-6, than the table of <i>Network 1</i> would look the following: </p> 
          <p> &nbsp; </p>
          <p> <b> Network 1: </b> </p>
                <table border='2' bgcolor = '#FFFFFF'>
                  <tr> 
                    <th></th>
                    <th>&ensp;Node 4&ensp;</th>
                    <th>&ensp;Node 1&ensp;</th>
                    <th>&ensp;Node 3&ensp;</th>
                    <th>&ensp;Node 5&ensp;</th>
                    <th>&ensp;Node 2&ensp;</th>
                    <th>&ensp;Node 6&ensp;</th>
                  </tr>
                  <tr>
                    <th>&ensp;Node 4&ensp;</th>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                  </tr>
                  <tr>
                    <th>&ensp;Node 1&ensp;</th>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                  </tr>
                  <tr>
                    <th>&ensp;Node 3&ensp;</th>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                  </tr>
                  <tr>
                    <th>&ensp;Node 5&ensp;</th>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                  </tr>
                  <tr>
                    <th>&ensp;Node 2&ensp;</th>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                  </tr>
                  <tr>
                    <th>&ensp;Node 6&ensp;</th>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;1&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                    <td>&emsp;&ensp;&nbsp;0&nbsp;&emsp;</td>
                  </tr>
               </table>
            <p> &nbsp; </p>
            <p> Using this now scrambled table the algorithm will compute the correlation<sup>1</sup> between the scrambled network and the unscrambled second network, after which the value is saved and the scrambing and calculation is repeated many times (often 1000 times or higher, but it depends on the calculation time), where more repetitions give a better reflection of reality. Using all the obtained values, it can be calculated how 'special' the original value between the two networks is by comparing how many of the random observations are smaller than the actual value and how many of the random observations are larger than the actual value. Depending on the hypothesis which is being tested, the <b>p-value</b> will either of these two values:</p>
            <p> &nbsp; </p>
            <ul>
              <li> If we want to test whether the observed correlation is <b>significantly bigger</b> than the random networks, we will use the proportion of random observations which were <b>smaller</b> than the actual statistic as the p-value </li>
              <li> If we want to test whether the observed correlation is <b>significantly smaller</b> than the random networks, we will use the proportion of random observations which were <b>bigger</b> than the actual statistic as the p-value</li>
            </ul>
            <p> &nbsp; </p>
            <p> Using the according p-value, it can then be calculated whether the observed statistic is significant or not </p>
            <p> &nbsp; </p>
            <p> <sup>1</sup> The formula for the correlation is out of the scope of this tutorial, but more informtion on it can be found <a href = https://www.michelecoscia.com/wp-content/uploads/2021/10/nvd_corr.pdf> in this paper </a>.</p>
        </div>",
    size = "l",
      html = TRUE
    )
  })
  
  # Define the sequence of tabs
  tabNames <- c("introduction", "data_upload", "dashboard", "cug_test", "community_detection", "network_comparison", "data_export")
  
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
  
  # Reactive expression to handle API call for generating explanations
  explanationOutput_cug <- eventReactive(input$runCUG, {
    req(input$runCUG)
    cug_result <- cugTest()
    
    if (!is.null(cug_result) && !is.null(cug_result$obs.stat)) {
      user_description <- user_dataset_text()
      prompt_cug <- sprintf(
        "The Conditional Uniform Graph (CUG) test produced a statistic of %f. 
        The probability of observing a statistic greater than or equal to this is %f, 
        and the probability of observing a statistic less than or equal to this is %f. 
        This information is crucial for understanding the significance of network connections 
        in terms of network analysis and its potential impact.
        Here is more information about the dataset: %s. 
        Make sure you explain what the CUG result entails in this particular context ",
        cug_result$obs.stat,
        cug_result$pgteobs,
        cug_result$plteobs,
        user_description
      )
    } else {
      prompt_cug <- "Failed to retrieve valid results from the CUG test."
    }
    
    # Call the Gemini function with the prompt, if valid
    if (!grepl("Failed", prompt_cug)) {
      generated_text <- gemini(prompt_cug)
      return(generated_text)
    } else {
      return(prompt_cug)  # Return error message or incorrect prompt
    }
  })
  
  # Render the explanation text obtained from the gemini api
  output$Gemini_cug_explanation <- renderUI({
    explanation_text <-  explanationOutput_cug()  # Use the reactive expression to get the explanation text
    HTML(markdown::renderMarkdown(text = explanation_text))
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
  
  # Dashboard Network Plot
  output$networkPlot1 <- renderVisNetwork({
    req(dataset())
    g <- graph_from_data_frame(dataset(), directed = FALSE)
    
    # Set the color of the node
    V(g)$color <- "#1bbbff"
    
    visNetwork::visIgraph(g) %>%
      visIgraphLayout(layout = "layout_with_fr") %>%
      visNodes(color = list(background = V(g)$color, border = "#2b2b2b", highlight = "#1F51FF"), 
               shadow = list(enabled = TRUE, size = 10, x = 0, y = 0)) %>%
      visEdges(
        # arrows = 'to',
        color = list(color = "#cccccc", highlight = "#FF69B4"),
        shadow = list(enabled = FALSE)
      ) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), 
                 nodesIdSelection = list(enabled = TRUE, style = "width: 150px;")) %>%
      visLayout(randomSeed = 123) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visEdges(smooth = FALSE) 
  }
  )
  
  # Graph Indices Count Plot
  output$CountPlot <- renderPlot({
    req(dataset())
    g <- graph_from_data_frame(dataset(), directed = FALSE)
    vertex_count <- igraph::vcount(g)
    edge_count <- igraph::ecount(g)
    
    scores <- cbind(edge_count, vertex_count)
    distances <- cbind(edge_count, vertex_count)
    
    plot_df <- data.frame(
      Category = c("Edge", "Vertex"),
      Counts = c(edge_count, vertex_count)
    )
    
    ggplot(plot_df, aes(x = Counts, y = Category, fill = Category)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("Edge" = "#1bbbff", "Vertex" = "#FF69B4"), name = "Category") +
      labs(x = "Counts", y = "Category", title = "Vertex and Edge Counts") +
      theme_minimal() +
      theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5), # Center the plot title
        plot.margin = margin(10, 30, 10, 10) # Adjust plot margins
      )
    
  })
  

  # Graph Indices Distance Plot
  
  output$DistancePlot <- renderPlot({
    req(dataset())
    g <- graph_from_data_frame(dataset(), directed = FALSE)
    
    mean_dist <- igraph::mean_distance(g)
    radius <- igraph::radius(g)
    diameter <- igraph::diameter(g)
    
    plot_df <- data.frame(
      Category = c("Mean Distance", "Radius", "Diameter"),
      Distances = c(mean_dist, radius, diameter)
    )
    
    ggplot(plot_df, aes(x = Distances, y = Category, fill = Category)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("Mean Distance" = "#1bbbff", "Radius" = "#FF69B4", "Diameter" = "#A9A9A9"), name = "Category") +
      labs(x = "Distance", y = "Category", title = "Distance Statistics") +
      theme_minimal() +
      theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5), # Center the plot title
        plot.margin = margin(10, 30, 10, 10) # Adjust plot margins
      ) +
      coord_cartesian(xlim = c(0, max(plot_df$Distances))) # Adjust x-axis limits
  })
  
  # Render the transitivity
  output$transitivityOutput <- renderText({
    req(dataset())
    g <- graph_from_data_frame(dataset(), directed = FALSE)
    return (round(igraph::transitivity(g), 3))
  })
  
  # Render the density
  output$densityOutput <- renderText({
    req(dataset())
    g <- graph_from_data_frame(dataset(), directed = FALSE)
    return(round(igraph::graph.density(g), 3))
  })
  

  # Graph Indices Centralization Plot
  
  output$CentralizationPlot <- renderPlot({
    req(dataset())
    g <- graph_from_data_frame(dataset(), directed = FALSE)
    
    betweenness_centralization <- igraph::centr_betw(g, directed = FALSE)$centralization
    closeness_centralization <- igraph::centr_clo(g, mode = 'out')$centralization
    degree_centralization <- igraph::centr_degree(g, mode = 'all')$centralization
    eigen_centralization <- igraph::centr_eigen(g, directed = FALSE)$centralization
    
    plot_df <- data.frame(
      Category = c("Betweenness", "Closeness", "Degree", "Eigenvector"),
      Centralization = c(betweenness_centralization,
                         closeness_centralization,
                         degree_centralization, 
                         eigen_centralization)
    )
    
    ggplot(plot_df, aes(x = Centralization, y = Category, fill = Category)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("Betweenness" = "#1bbbff",
                                   "Closeness" = "#FF69B4",
                                   "Degree" = "#A9A9A9",
                                   "Eigenvector" = "#1F51FF"
      ), 
      name = "Category") +
      labs(x = "Centralization", y = "Category", title = "Network Centralization") +
      theme_minimal() +
      theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5), # Center the plot title
        plot.margin = margin(10, 30, 10, 10) # Adjust plot margins
      ) +
      coord_cartesian(xlim = c(0, max(plot_df$Centralization))) + # Adjust x-axis limits
      coord_flip() # Flip the coordinates to make the bars vertical
  })
  
  # Distribution Plot
  output$DistPlot <- renderPlot({
    req(dataset())
    g <- graph_from_data_frame(dataset(), directed = FALSE)
    
    if (input$centrality == "Degree"){
      result = igraph::degree(g, mode = "all")
      binwidth = 0.25
    }
    
    if (input$centrality == "Betweenness"){
      result = igraph::betweenness(g, directed = FALSE, normalized = TRUE)
      binwidth = 0.01
    }
    
    if (input$centrality == "Closeness"){
      result = igraph::closeness(g, mode = "all")
      binwidth = 0.1
    }
    
    if (input$centrality == "Eccentricity"){
      result = igraph::eccentricity(g, mode = 'all')
      binwidth = 0.1
    }
    
    # Create a data frame containing the centrality measures
    centrality_df <- data.frame(Centrality = result)
    
    # Create a histogram using ggplot2
    ggplot(centrality_df, aes(x = Centrality)) +
      geom_histogram(binwidth = binwidth, fill = "#1bbbff", alpha = 1) +
      labs(x = input$centrality, y = "Frequency", title = "Centrality Distribution Histogram") +
      theme_minimal() +
      theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5), # Center the plot title
      )
  })
  
  # Centrality Boxplots
  
  output$BoxPlot <- renderPlot({
    req(dataset())
    g <- graph_from_data_frame(dataset(), directed = FALSE)
    
    if (input$centrality == "Degree"){
      result = igraph::degree(g, mode = "all")
    }
    
    if (input$centrality == "Betweenness"){
      result = igraph::betweenness(g, directed = FALSE, normalized = TRUE)
    }
    
    if (input$centrality == "Closeness"){
      result = igraph::closeness(g, mode = "all")
    }
    
    if (input$centrality == "Eccentricity"){
      result = igraph::eccentricity(g, mode = 'all')
    }
    
    # Create a data frame containing the centrality measures
    centrality_df <- data.frame(Centrality = result)
    
    # Create a histogram using ggplot2
    ggplot(centrality_df, aes(x = Centrality)) +
      geom_boxplot(fill = "#1bbbff", alpha = 1) +
      labs(x = input$centrality, y = "Frequency", title = "Centrality Boxplots") +
      theme_minimal() +
      theme(
        legend.position = "top",
        plot.title = element_text(hjust = 0.5), # Center the plot title
      )
  })
  
  # Reactive expression to handle API call for generating explanations using Gemini
  explanationOutput_comm <- eventReactive(input$runAnalysis, {
    req(analysisResult())
    modularity_val <- analysisResult()$modularity
    communities <- toString(unique(analysisResult()$memberships))
    comm_alg <- input$algorithm
    user_description_test <- user_dataset_text()
    
    # Construct the prompt for Gemini
    prompt <- sprintf("Explain in detail the significance of a modularity score of %f and community memberships as follows: %s,
                    in terms of network analysis and its potential impact, 
                    note that to get the communities the %s algorithm was used. 
                      Make sure you explain what the modularity score result and 
                      community memberships entails in this particular context: %s.
                      ", modularity_val, communities, comm_alg, user_description_test)
    
    # Call the Gemini function with the prompt
    generated_text <- gemini(prompt)
    
    return(generated_text)
  })
  
  
  output$Gemini_comm_explanation <- renderUI({
    explanation_text_comm <- explanationOutput_comm()
    HTML(markdown::renderMarkdown(text = explanation_text_comm))
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
    req(dataset())
    req(dataset2())
    if (is.null(input$statisticChoice)){
      showModal(
        modalDialog(title = 'No Choice made',
                    'Please select at least one statistic to be tested in the choice menu',
                    easyClose = TRUE,
                    footer = modalButton("Got it!"))
      )
    req(shouldStatisticCompare())
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
      return("Error: No network and communities available. Please return to the 'Find Communities' tab.")
    } else if (is.null(network_object())) {
      return("Error: No network available. Please return to the 'Find Communities' tab.")
    } else if (is.null(community_memberships())) {
      return("Error: No communities available. Please return to the 'Find Communities' tab.")
    } else {
      return(NULL)
    }
  })
  
  # Retrieve the table showing the most important actors
  output$ActorTable = DT::renderDataTable({
    req(dataset())
    req(input$ActorMetricD)
    req(input$ActorNumD)
    g <- graph_from_data_frame(dataset(), directed = FALSE)
    find_important_actors(g, metric = input$ActorMetricD, n_actors = input$ActorNumD)
  })
  
}



# Run the Shiny application
shinyApp(ui = ui, server = server)
