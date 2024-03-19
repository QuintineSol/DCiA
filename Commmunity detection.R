library(igraph)

# Community detection in the context of interdisciplinary collaboration 
# aims to identify groups or clusters of researchers who tend to collaborate 
# more closely with each other within and across different fields of study 
# such as digital design, marketing, and music. 

#_______________________________________________________

# Perform community detection
grant_communities <- cluster_walktrap(graph_grants)
coauthor_communities <- cluster_walktrap(graph_co_author)
knowledge_communities <- cluster_walktrap(graph_knowledge)


#_______________________________________________
print(paste("Grant network communities:", length(grant_communities)))
print(paste("Co-authorship network communities:", length(coauthor_communities)))
print(paste("Knowledge sharing network communities:", length(knowledge_communities)))


# Modularity measures how well a network is divided into groups of 
# closely connected nodes, called communities. It ranges between 0 and 1.

# Higher modularity means clearer neighborhood boundaries, while lower 
# modularity suggests communities are less distinct or more mixed 
# together.

print(paste("Modularity of grant network communities:", round(modularity(grant_communities), 2)))
print(paste("Modularity:", round(modularity(coauthor_communities), 2)))
print(paste("Modularity:", round(modularity(knowledge_communities), 2)))


# Plot the networks with communities for grant applications
plot(
  grant_communities, graph_grants, 
  vertex.label = NA, vertex.size = 3.5, 
  edge.arrow.size = 0.5, layout = layout_nicely, 
  main = "Grant Applications Network with Communities", 
  mark.border = "transparent", mark.col = "transparent"
)

# Community detection helps identify clusters of researchers who 
# frequently collaborate on grant applications within specific 
# disciplines or across interdisciplinary boundaries.

# Plot the co-author network with communities
plot(
  coauthor_communities, graph_co_author, 
  vertex.label = NA, vertex.size = 3.5, edge.arrow.size = 0.5,
  layout = layout_nicely, main = "Co-Author Network with Communities", 
  mark.border = "transparent", mark.col = "transparent"
)

# This network reveals clusters of researchers who have collaborated on 
# academic publications, indicating strong ties between individuals or 
# groups working on similar research topics.

# Plot the knowledge sharing network with communities
plot(
  knowledge_communities, graph_knowledge, 
  vertex.label = NA, vertex.size = 3.5, edge.arrow.size = 0.5,
  layout = layout_nicely, 
  main = "Knowledge sharing Network with Communities", 
  mark.border = "transparent", mark.col = "transparent"
)

# Community detection in the knowledge sharing network uncovers groups of 
# researchers who actively exchange ideas, insights, or expertise related 
# to digital design, marketing, and music.







