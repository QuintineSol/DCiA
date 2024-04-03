
library(sna)

# Define a function to calculate modularity for a given community detection
# calculate_modularity <- function(graph, communities) {
#   modularity(communities)
# }


# # Perform CUG test for modularity on the grant network
# cug_test_modularity_grants <- sna::cug.test(
#   network_grants,
#   FUN = calculate_modularity,
#   mode = "graph",
#   cmode = "edges",
#   reps = 10,
#   ignore.eval = TRUE,
#   FUN.args = list(graph = network_grants, communities = grant_communities)
# )
# 
# cug_test_modularity_coauthor <- sna::cug.test(
#   network_co_author,
#   FUN = calculate_modularity,
#   mode = "graph",
#   cmode = "edges",
#   reps = 10,
#   ignore.eval = TRUE,
#   FUN.args = list(graph = graph_co_author, communities = coauthor_communities)
# )
# 
# cug_test_modularity_knowledge <- sna::cug.test(
#   network_knowledge,
#   FUN = calculate_modularity,
#   mode = "graph",
#   cmode = "edges",
#   reps = 10,
#   ignore.eval = TRUE,
#   FUN.args = list(graph = graph_co_author, communities = coauthor_communities)
# )
# 
# # Print CUG test results for modularity
# print("CUG test results for modularity on grant network:")
# print(cug_test_modularity_grants)
# print("CUG test results for modularity on co-author network:")
# print(cug_test_modularity_coauthor)
# print("CUG test results for modularity on knkowledge network:")
# print(cug_test_modularity_knowledge)
#_________________________________________________________________________
#____________________________________________________________________________________

# Perform CUG test for centralization (betweenness) 
cug_test_centralization_betweenness_grants <- sna::cug.test(
  network_grants,
  FUN = sna::centralization,
  FUN.arg = list(FUN = sna::betweenness),
  mode = "graph",
  cmode = "edges",
  reps = 10,
  ignore.eval = TRUE
)

cug_test_centralization_betweenness_coauthor <- sna::cug.test(
  network_co_author,
  FUN = sna::centralization,
  FUN.arg = list(FUN = sna::betweenness),
  mode = "graph",
  cmode = "edges",
  reps = 10,
  ignore.eval = TRUE
)

cug_test_centralization_betweenness_knowledge <- sna::cug.test(
  network_knowledge,
  FUN = sna::centralization,
  FUN.arg = list(FUN = sna::betweenness),
  mode = "graph",
  cmode = "edges",
  reps = 10,
  ignore.eval = TRUE
)

Bet_Centralization <- c(
  cug_test_centralization_betweenness_grants$obs.stat,
  cug_test_centralization_betweenness_coauthor$obs.stat,
  cug_test_centralization_betweenness_knowledge$obs.stat
)

PctGreater <- c(
  cug_test_centralization_betweenness_grants$pgteobs,
  cug_test_centralization_betweenness_coauthor$pgteobs,
  cug_test_centralization_betweenness_knowledge$pgteobs
)

PctLess <- c(
  cug_test_centralization_betweenness_grants$plteobs,
  cug_test_centralization_betweenness_coauthor$plteobs,
  cug_test_centralization_betweenness_knowledge$plteobs
)

# Combine the results
Betweenness <- cbind(
  Bet_Centralization,
  PctGreater,
  PctLess
)

rownames(Betweenness) <- c("Grants", "Co-Author", "Knowledge")  # Change the row names
colnames(Betweenness) <- c("Centralization", "Pr(X>=Obs)", "Pr(X<=Obs)")  # Change the column names

# Print or view the combined results
print("Combined CUG test results for centralization (betweenness) across networks:")
print(Betweenness)
#____________________________________________________________________________________


# Perform CUG test for transitivity on the networks (not getowkrs_grant since this is bipartite )

cug_test_transitivity_coauthor <- sna::cug.test(
  network_co_author,
  FUN = sna::gtrans,
  mode = "graph",
  cmode = "edges",
  reps = 10,
  ignore.eval = TRUE
)

cug_test_transitivity_knowledge <- sna::cug.test(
  network_knowledge,
  FUN = sna::gtrans,
  mode = "graph",
  cmode = "edges",
  reps = 10,
  ignore.eval = TRUE
)

# Print CUG test results for transitivity
print("CUG test results for transitivity on co-author network:")
print(cug_test_transitivity_coauthor)
print("CUG test results for transitivity on knowledge network:")
print(cug_test_transitivity_knowledge)




