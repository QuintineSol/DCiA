load('Data/networks.RData');load('Data/graphs.RData')

network_co_author

eval(parse(text = 'attributes'))$people_table

ifelse(igraph::V(graph_grants)$type, 'red', 'green')

all(is.na(igraph::V(graph_grants)$type))

plot(graph_co_author, main = 'grants', 
     vertex.size = 5,
     edge.size =4/32,
     edge.arrow.size = 0.2,
     vertex.color = 'red',
     vertex.label.cex = 0.4,
     vertex.label.color = 'black',
     vertex.frame.color = "#ffffff",
     vertex.label = NA,
     layout = igraph::layout_with_graphopt)

plot(network_co_author)

ifelse(rep(all(is.na(igraph::V(graph_grants)$type)), times = igraph::vcount(graph_grants)),'orange',ifelse(igraph::V(graph_grants)$type,"navy", 'red'))                                                                                                                                       
ifelse(F, 'skyblue',ifelse(igraph::V(graph_grants)$type,"navy", 'red'))

igraph::vcount(graph_grants)


igraph::E(graph_co_author)$color = T
igraph::E(graph_knowledge)$color = T
igraph::E(graph_grants_people)$color = T
igraph::V(graph_co_author)$color = T
igraph::V(graph_knowledge)$color = T
igraph::V(graph_grants_people)$color = T





(combination_graph = igraph::union(igraph::union(graph_co_author, graph_grants_people), graph_knowledge))

sum(igraph::V(combination_graph) %in% igraph::V(graph_co_author))
sum(igraph::V(combination_graph) %in% igraph::V(graph_grants_people))
sum(igraph::V(combination_graph) %in% igraph::V(graph_knowledge))

(all3 = igraph::simplify(igraph::intersection(igraph::intersection(graph_co_author, graph_grants_people, keep.all.vertices = F), graph_knowledge, keep.all.vertices = F)))
plot(all3,
     vertex.size = 11,
     edge.size =8,
     edge.width = 4,
     edge.arrow.size = 0.8,
     vertex.color = 'darkorange',
     edge.color = 'black',
     vertex.label.cex = 0.6,
     vertex.label.color = 'blue',
     vertex.frame.color = "#ffffff",
     layout = igraph::layout.fruchterman.reingold)

all3

igraph::E(combination_graph)$color_1 =ifelse(is.na(igraph::E(combination_graph)$color_1), 1, 0)
igraph::E(combination_graph)$color_2 =ifelse(is.na(igraph::E(combination_graph)$color_2), 1, 0)
(igraph::E(combination_graph)$color_3 = ifelse(is.na(igraph::E(combination_graph)$color), 1, 0))
igraph::E(combination_graph)$color = mapply(rgb, igraph::E(combination_graph)$color_1, igraph::E(combination_graph)$color_2, igraph::E(combination_graph)$color_3)

colors = cbind(igraph::E(combination_graph)$color_1, igraph::E(combination_graph)$color_2, igraph::E(combination_graph)$color_3)


sum(igraph::E(combination_graph)$color_1, na.rm = T)


plot(igraph::induced_subgraph(combination_graph, vids = igraph::V(all3)$name),
     vertex.size = 10,
     edge.size =8,
     edge.width = 4,
     edge.arrow.size = 0.8,
     vertex.color = 'darkorange',
     vertex.label.cex = 1,
     vertex.label.color = 'blue',
     vertex.frame.color = "#ffffff",
     layout = igraph::layout_nicely)



table(igraph::E(igraph::induced_subgraph(combination_graph, vids = igraph::V(all3)$name))$color)

people_table[people_table$Node %in% igraph::V(all3)$name,]
people_table[!(people_table$Node %in% igraph::V(all3)$name) & (people_table$Organisation == 'Internal Staff'),]
not_3_staff = people_table[!(people_table$Node %in% igraph::V(all3)$name) & (people_table$Organisation == 'Internal Staff'),]$Node

sum(people_table$Organisation == 'Internal Staff')

igraph::V(combination_graph)$color_1 =ifelse(is.na(igraph::V(combination_graph)$color_1), 1, 0)
igraph::V(combination_graph)$color_2 =ifelse(is.na(igraph::V(combination_graph)$color_2), 1, 0)
(igraph::V(combination_graph)$color_3 = ifelse(is.na(igraph::V(combination_graph)$color), 1, 0))
igraph::V(combination_graph)$color = mapply(rgb, igraph::V(combination_graph)$color_1, igraph::V(combination_graph)$color_2, igraph::V(combination_graph)$color_3)

simple_staff = igraph::simplify(igraph::induced_subgraph(combination_graph, vids = as.character(people_table[people_table$Organisation == 'Internal Staff',]$Node)), remove.multiple = F)
plot(simple_staff,
     vertex.size = 2,
     edge.size =2,
     edge.width = 1,
     edge.arrow.size = 0.8,
     vertex.label.cex = 1,
     vertex.label.color = 'blue',
     vertex.frame.color = "#ffffff",
     vertex.label = NA,
     layout = igraph::layout_with_graphopt)



people_table[people_table$Organisation == 'Internal Staff',][!people_table[people_table$Organisation == 'Internal Staff',]$Node %in% igraph::V(combination_graph)$name,]


igraph::V(combination_graph)$which_graph = paste0(ifelse(substr(igraph::V(combination_graph)$color, 2, 2)=='F','', 'co-author-'), 
                                                  ifelse(substr(igraph::V(combination_graph)$color, 4, 4)=='F','', 'grants-'),
                                                  ifelse(substr(igraph::V(combination_graph)$color, 6, 6)=='F','', 'knowledge'))

igraph::E(combination_graph)$which_graph = paste0(ifelse(substr(igraph::E(combination_graph)$color, 2, 2)=='F','', 'co-author-'), 
                                                  ifelse(substr(igraph::E(combination_graph)$color, 4, 4)=='F','', 'grants-'),
                                                  ifelse(substr(igraph::E(combination_graph)$color, 6, 6)=='F','', 'knowledge'))

staff_net = igraph::induced.subgraph(combination_graph, vids = as.character(people_table[people_table$Organisation == 'Internal Staff',]$Node))

igraph::induced.subgraph(graph_co_author, vids = intersect(as.character(people_table[people_table$Organisation == 'Internal Staff',]$Node), 
                                                           igraph::V(graph_co_author)$name))

table(igraph::V(combination_graph)$which_graph)
table(igraph::E(combination_graph)$which_graph)

table(igraph::V(staff_net)$which_graph)
table(igraph::E(staff_net)$which_graph)

comb_no_weight = combination_graph
igraph::E(comb_no_weight)$weight = 1 

igraph::V(graph_grants_people)$name[as.numeric(igraph::V(graph_grants_people)$name)<999999]

plot(comb_no_weight,
     vertex.size = snafun::v_betweenness(comb_no_weight, directed = F)/50000,
     edge.size =2,
     edge.width = 1,
     edge.arrow.size = 0.8,
     vertex.label.cex = 0.6,
     vertex.label.color = 'blue',
     vertex.frame.color = "#ffffff",
     layout = igraph::layout_with_graphopt)

staff_no_weight = staff_net
igraph::E(staff_no_weight)$weight = 1 
plot(staff_no_weight,
     vertex.size = snafun::v_betweenness(staff_no_weight, directed = F)/5000,
     edge.size =2,
     edge.width = 1,
     edge.arrow.size = 0.8,
     vertex.label.cex = 0.6,
     vertex.label.color = 'blue',
     vertex.frame.color = "#ffffff",
     layout = igraph::layout_with_graphopt)

sort(snafun::v_betweenness(comb_no_weight, directed = F),decreasing = T)
sort(snafun::v_betweenness(staff_no_weight, directed = F),decreasing = T)

plot(comb_no_weight,
     vertex.size = snafun::v_degree(comb_no_weight)/30,
     edge.size =2,
     edge.width = 1,
     edge.arrow.size = 0.8,
     vertex.label.cex = 0.6,
     vertex.label.color = 'blue',
     vertex.frame.color = "#ffffff",
     layout = igraph::layout_with_graphopt)

plot(staff_no_weight,
     vertex.size = snafun::v_degree(staff_no_weight)/25,
     edge.size =2,
     edge.width = 1,
     edge.arrow.size = 0.8,
     vertex.label.cex = 0.6,
     vertex.label.color = 'blue',
     vertex.frame.color = "#ffffff",
     layout = igraph::layout_with_graphopt)

sort(snafun::v_degree(comb_no_weight),decreasing = T)
sort(snafun::v_degree(staff_no_weight),decreasing = T)


igraph::E(combination_graph)$weight = ifelse(igraph::E(combination_graph)$which_graph %in% c('co-author-', 'knowledge', 'grants-'), 3,
                                             ifelse(igraph::E(combination_graph)$which_graph %in% c('co-author-knowledge', 'grants-knowledge', 'co-author-grants-'), 2, 1))

sort(snafun::v_betweenness(comb_no_weight, directed = F),decreasing = T)
sort(igraph::betweenness(combination_graph, directed = F),decreasing = T)

plot(combination_graph,
     vertex.size = igraph::betweenness(combination_graph, directed = F)/50000,
     edge.size =2,
     edge.width = 1,
     edge.arrow.size = 0.8,
     vertex.label.cex = 0.6,
     vertex.label.color = 'blue',
     vertex.frame.color = "#ffffff",
     layout = igraph::layout_with_graphopt)
