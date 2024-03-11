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


sort(snafun::v_betweenness(graph_co_author, directed = F),decreasing = T)
plot(graph_co_author,
     vertex.size = igraph::betweenness(graph_co_author, directed = F)/50000,
     edge.size =2,
     edge.width = 1,
     vertex.color = 'cyan',
     edge.arrow.size = 0.8,
     edge.color = 'grey80',
     vertex.label.cex = 0.6,
     vertex.label.color = ifelse(igraph::V(graph_co_author)$name %in% people_table[people_table$Organisation == 'Internal Staff',]$Node, 'black', 'red'),
     vertex.frame.color = "#ffffff",
     layout = igraph::layout_with_graphopt)

sort(snafun::v_betweenness(graph_knowledge, directed = F),decreasing = T)
plot(graph_knowledge,
     vertex.size = igraph::betweenness(graph_knowledge, directed = F)/5000,
     edge.size =2,
     edge.width = 1,
     vertex.color = 'yellow',
     edge.color = 'grey80',
     edge.arrow.size = 0.8,
     vertex.label.cex = 0.6,
     vertex.label.color = ifelse(igraph::V(graph_knowledge)$name %in% people_table[people_table$Organisation == 'Internal Staff',]$Node, 'blue', 'cyan'),
     vertex.frame.color = "#000000",
     layout = igraph::layout_with_graphopt)


igraph::E(graph_grants_people)$weight = 1
sort(snafun::v_betweenness(graph_grants_people, directed = F),decreasing = T)
plot(graph_grants_people,
     vertex.size = igraph::betweenness(graph_grants_people, directed = F)/2500,
     edge.size =2,
     edge.width = 1,
     vertex.color = '#FF00FF',
     edge.color = 'grey80',
     edge.arrow.size = 0.8,
     vertex.label.cex = 0.6,
     vertex.label.color = ifelse(igraph::V(graph_grants_people)$name %in% people_table[people_table$Organisation == 'Internal Staff',]$Node, 'blue', 'cyan'),
     vertex.frame.color = "#000000",
     layout = igraph::layout_with_graphopt)

sum(igraph::V(graph_knowledge)$name %in% people_table[people_table$Organisation == 'Internal Staff',]$Node)
graph_knowledge

sum(igraph::V(graph_grants_people)$name %in% people_table[people_table$Organisation == 'Internal Staff',]$Node)
graph_grants_people

igraph::V(staff_net)$faculty = people_table[match(igraph::V(staff_net)$name, people_table$Node),]$Faculty
from(igraph::E(staff_net))

snafun::make_edgelist(snafun::to_frame(staff_net))

(edge_info = cbind(igraph::as_edgelist(staff_net), as.character(people_table$Faculty[match(igraph::as_edgelist(staff_net)[,1], people_table$Node)]),
                  as.character(people_table$Faculty[match(igraph::as_edgelist(staff_net)[,2], people_table$Node)])))

table(c(paste(edge_info[,3], edge_info[,4]), paste(edge_info[,4], edge_info[,3])))[c('Digital Design Digital Design', 'Digital Design Marketing', 'Digital Design Music', 'Digital Design Other', 'Marketing Marketing', 'Marketing Music', 'Marketing Other', 'Music Music', 'Music Other')]
table(people_table$Faculty)

sort(table(c(edge_info[edge_info[,3] != edge_info[,4],1], edge_info[edge_info[,3] != edge_info[,4],2])), decreasing = T)
sort(table(c(edge_info[edge_info[,3] == edge_info[,4],1], edge_info[edge_info[,3] == edge_info[,4],2])), decreasing = T)
sort(table(c(edge_info[edge_info[,3] != edge_info[,4],1], edge_info[edge_info[,3] != edge_info[,4],2]))/igraph::degree(staff_net), decreasing = T)
sort(table(factor(c(edge_info[edge_info[,3] == edge_info[,4],1], edge_info[edge_info[,3] == edge_info[,4],2]), levels = igraph::V(staff_net)$name))/igraph::degree(staff_net))

knowledge_staff_net = igraph::induced.subgraph(graph_knowledge, vids = intersect(as.character(people_table[people_table$Organisation == 'Internal Staff',]$Node), igraph::V(graph_knowledge)$name))
(edge_info_1 = cbind(igraph::as_edgelist(knowledge_staff_net), as.character(people_table$Faculty[match(igraph::as_edgelist(knowledge_staff_net)[,1], people_table$Node)]),
                   as.character(people_table$Faculty[match(igraph::as_edgelist(knowledge_staff_net)[,2], people_table$Node)])))
sort(table(factor(c(edge_info_1[edge_info_1[,3] == edge_info_1[,4],1], edge_info_1[edge_info_1[,3] == edge_info_1[,4],2]), levels = igraph::V(knowledge_staff_net)$name))/igraph::degree(knowledge_staff_net))

co_author_staff_net = igraph::induced.subgraph(graph_co_author, vids = intersect(as.character(people_table[people_table$Organisation == 'Internal Staff',]$Node), igraph::V(graph_co_author)$name))
(edge_info_2 = cbind(igraph::as_edgelist(co_author_staff_net), as.character(people_table$Faculty[match(igraph::as_edgelist(co_author_staff_net)[,1], people_table$Node)]),
                   as.character(people_table$Faculty[match(igraph::as_edgelist(co_author_staff_net)[,2], people_table$Node)])))
sort(table(factor(c(edge_info_2[edge_info_2[,3] == edge_info_2[,4],1], edge_info_2[edge_info_2[,3] == edge_info_2[,4],2]), levels = igraph::V(co_author_staff_net)$name))/igraph::degree(co_author_staff_net))

grants_staff_net = igraph::induced.subgraph(graph_grants_people, vids = intersect(as.character(people_table[people_table$Organisation == 'Internal Staff',]$Node), igraph::V(graph_grants_people)$name))
(edge_info_3 = cbind(igraph::as_edgelist(grants_staff_net), as.character(people_table$Faculty[match(igraph::as_edgelist(grants_staff_net)[,1], people_table$Node)]),
                   as.character(people_table$Faculty[match(igraph::as_edgelist(grants_staff_net)[,2], people_table$Node)])))
sort(table(factor(c(edge_info_3[edge_info_3[,3] == edge_info_3[,4],1], edge_info_3[edge_info_3[,3] == edge_info_3[,4],2]), levels = igraph::V(grants_staff_net)$name))/igraph::degree(grants_staff_net))

plot_df = cbind('grants' = table(factor(c(edge_info_3[edge_info_3[,3] == edge_info_3[,4],1], edge_info_3[edge_info_3[,3] == edge_info_3[,4],2]), levels = igraph::V(grants_staff_net)$name))/igraph::degree(grants_staff_net),
           'knowledge' = table(factor(c(edge_info_1[edge_info_1[,3] == edge_info_1[,4],1], edge_info_1[edge_info_1[,3] == edge_info_1[,4],2]), levels = igraph::V(knowledge_staff_net)$name))/igraph::degree(knowledge_staff_net),
           'co_author' = table(factor(c(edge_info_2[edge_info_2[,3] == edge_info_2[,4],1], edge_info_2[edge_info_2[,3] == edge_info_2[,4],2]), levels = igraph::V(co_author_staff_net)$name))/igraph::degree(co_author_staff_net))
plot_df[sapply(plot_df, is.infinite)] <- NA
boxplot(plot_df)

# GIVES ERRORS
people_table$`Internal Staff vs. External Staff` = as.character(people_table$`Internal Staff vs. External Staff`)
network_knowledge_full = network::add.vertices(intergraph::asNetwork(igraph::simplify(graph_knowledge)), 
                                               sum(!(as.character(people_table[as.character(people_table$`Internal Staff vs. External Staff`) == 'Internal','Node']) %in% igraph::V(graph_knowledge)$name)), 
                                               list(
                                                 vertex.names = 
                                                   as.character(people_table[as.character(people_table$`Internal Staff vs. External Staff`) == 'Internal',][!(as.character(people_table[as.character(people_table$`Internal Staff vs. External Staff`) == 'Internal',]$Node) %in% igraph::V(graph_knowledge)$name),'Node'])))
  
network::add.vertices(intergraph::asNetwork(igraph::simplify(graph_knowledge)),4)%v%
sna::qaptest(list(network_knowledge, network_grants_people), fun = sna::gcor)
