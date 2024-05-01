#load(paste0(dirname(rstudioapi::getSourceEditorContext()$path), '/Data/networks.RData'));load(paste0(dirname(rstudioapi::getSourceEditorContext()$path),'/Data/graphs.RData'))

####  QAP
QAP_networks = function(network1, network2, reps = 100, net1_name, net2_name){
  #' Given the input of 2 networks, which have to be igraph networks, get the correlation between the two networks
  #' If two nodes have the same names, it is ought to be the same node, which appears in both networks. 
  #' Thus, if two networks have the node John, they should be the same person appearing in both networks
  #' If the networks contain multiple ties between the same actors, or if they contain self-loops, these will be removed before the calculations are done
  #' Nodes which only appear in a singular network will be treated as an isolate in the other network
  #' 
  #' Parameters which can be defined:
  #' - network1: The first network which should be entered into the comparison
  #' - network2: The second network which should be entered into the comparison
  #' - reps: The number of repetitions which should be used in the Monte Carlo Simulation
  
  if (length(intersect(igraph::V(network1)$name, igraph::V(network2)$name)) == length(igraph::V(network1)$name) && length(intersect(igraph::V(network1)$name, igraph::V(network2)$name)) == length(igraph::V(network2)$name)){
    # Networks have the same nodes, thus there do not need to be any nodes added to any of the networks
    
    # Simplify the networks and convert them to network class
    print('Converting networks...')
    network1 = intergraph::asNetwork(igraph::simplify(network1))
    network2 = intergraph::asNetwork(igraph::simplify(network2))
    # Return the qaptest, such that further applications can be built on top
    return(sna::qaptest(list(network1, network2), FUN = sna::gcor, reps = reps, g1 = 1, g2 = 2))
    
  } else if (length(setdiff(igraph::V(network1)$name, igraph::V(network2)$name))){
    # There are nodes in network 1 which are not in network 2
    print(paste0('Unseen nodes in ', net1_name ,'...'))
    if (length(setdiff(igraph::V(network2)$name, igraph::V(network1)$name))){
      print(paste0('Unseen nodes in ', net2_name, '...'))
      # Both networks have nodes which are not in the other network
      
      # Get the name of the nodes which need to be added to the other network (as isolates)
      nodes_in_2 = setdiff(igraph::V(network2)$name, igraph::V(network1)$name)
      nodes_in_1 = setdiff(igraph::V(network1)$name, igraph::V(network2)$name)
      
      #Add the vertices to the new graphs
      print(paste('Adding', length(setdiff(igraph::V(network2)$name, igraph::V(network1)$name)), 'Nodes to', net1_name))
      network1 = igraph::add_vertices(network1, length(nodes_in_2), name = nodes_in_2)
      print(paste('Adding', length(setdiff(igraph::V(network1)$name, igraph::V(network2)$name)), 'Nodes to', net2_name))
      network2 = igraph::add_vertices(network2, length(nodes_in_1), name = nodes_in_1)
      
      # Simplify the networks and convert them to network class
      print('Converting networks...')
      network1 = intergraph::asNetwork(igraph::simplify(network1))
      network2 = intergraph::asNetwork(igraph::simplify(network2))
      # Return the qaptest, such that further applications can be built on top
      return(sna::qaptest(list(network1, network2), FUN = sna::gcor, reps = reps, g1 = 1, g2 = 2))
      
    } else{
      # Only network 1 has additional nodes
      
      # Get the name of the nodes which need to be added to the other network (as isolates)
      nodes_in_1 = setdiff(igraph::V(network1)$name, igraph::V(network2)$name)
      
      #Add the vertices to the new graphs
      print(paste('Adding', length(setdiff(igraph::V(network1)$name, igraph::V(network2)$name)), 'Nodes to network 2'))
      network2 = igraph::add_vertices(network2, length(nodes_in_1), name = nodes_in_1)
      
      # Simplify the networks and convert them to network class
      print('Converting networks...')
      network1 = intergraph::asNetwork(igraph::simplify(network1))
      network2 = intergraph::asNetwork(igraph::simplify(network2))
      # Return the qaptest, such that further applications can be built on top
      return(sna::qaptest(list(network1, network2), FUN = sna::gcor, reps = reps, g1 = 1, g2 = 2))
      
    }
  } else{
    print(paste0('Unseen nodes in ', net2_name, '...'))
    # Only network 2 has additional nodes
    
    # Get the name of the nodes which need to be added to the other network (as isolates)
    nodes_in_2 = setdiff(igraph::V(network2)$name, igraph::V(network1)$name)
    
    #Add the vertices to the new graphs
    print(paste('Adding', length(setdiff(igraph::V(network2)$name, igraph::V(network1)$name)), 'Nodes to', net1_name))
    network1 = igraph::add_vertices(network1, length(nodes_in_2), name = nodes_in_2)
    
    # Simplify the networks and convert them to network class
    print('Converting networks...')
    network1 = intergraph::asNetwork(igraph::simplify(network1))
    network2 = intergraph::asNetwork(igraph::simplify(network2))
    # Return the qaptest, such that further applications can be built on top
    return(sna::qaptest(list(network1, network2), FUN = sna::gcor, reps = reps, g1 = 1, g2 = 2))
  }
}

#sna::plot.qaptest(QAP_networks(graph_grants_people, graph_knowledge, reps = 100))
#QAP_networks(graph_grants_people, graph_knowledge, reps = 100)
#QAP_networks(igraph::graph_from_data_frame(read.csv('C:/Users/woute/OneDrive/Documenten/GitHub/Wouter/DCiA/Data/grants_people_to_people.csv'), directed = F), igraph::graph_from_data_frame(read.csv('C:/Users/woute/OneDrive/Documenten/GitHub/Wouter/DCiA/Data/knowledge_sharing_people_to_people.csv'), directed = F), reps = 10)$dist

#sna::plot.qaptest(QAP_networks(igraph::graph_from_data_frame(read.csv('C:/Users/woute/OneDrive/Documenten/GitHub/Wouter/DCiA/Data/grants_people_to_people.csv'), directed = F), igraph::graph_from_data_frame(read.csv('C:/Users/woute/OneDrive/Documenten/GitHub/Wouter/DCiA/Data/knowledge_sharing_people_to_people.csv'), directed = F), reps = 10))

compare_statistics = function(network1, network2, significance_level = 0.95, statistics = NULL, nbins = 100, net1_name, net2_name){
  closeness_plot = NULL
  degree_plot = NULL
  betweenness_plot = NULL
  if (isTRUE(statistics['Degree'])){
    degrees_1 = igraph::degree(network1)
    print(paste('The average degree of', net1_name,' is:', mean(degrees_1)))
    degrees_2 = igraph::degree(network2)
    print(paste('The average degree of', net2_name, 'is:', mean(degrees_2)))
    degree_plot = ggplot2::ggplot() + 
      ggplot2::geom_density(ggplot2::aes(x = igraph::degree(network1)), color= 'Blue',fill="lightblue", alpha=0.4, bw =  max(quantile(degrees_2, c(.98))*1.05, quantile(degrees_1, c(.98))*1.05)/nbins) + 
      ggplot2::geom_density(ggplot2::aes(x = igraph::degree(network2)), color = 'Orange', fill = '#fcd997', alpha=0.4, bw =  max(quantile(degrees_2, c(.98))*1.05, quantile(degrees_1, c(.98))*1.05)/nbins) + 
      ggplot2::geom_vline(ggplot2::aes(xintercept = mean(igraph::degree(network2))), color = 'orange', linetype = 'dashed', linewidth = max(quantile(igraph::degree(network2), c(.98))*1.05, quantile(igraph::degree(network1), c(.98))*1.05)/100) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = mean(igraph::degree(network1))), color = 'blue', linetype = 'dashed', linewidth = max(quantile(igraph::degree(network2), c(.98))*1.05, quantile(igraph::degree(network1), c(.98))*1.05)/100) + 
      ggplot2::coord_cartesian(xlim = c(0, max(quantile(igraph::degree(network2), c(.98))*1.05, quantile(igraph::degree(network1), c(.98))*1.05)), expand = F)+ ggplot2::theme(legend.position="right")
    test = stats::t.test(degrees_1, degrees_2, alternative = 'two.sided')
    if (test$p.value < 1 - significance_level){
      print('There is a significant difference between the degree of both graphs')
      print(paste('p-value:', test$p.value))
      if (test$statistic< 0){
        print(paste('Mean degree of', net1_name,'is significantly lower than that of', net2_name))
      } else{
        print(paste('Mean degree of', net2_name, 'is significantly lower than that of', net1_name))
      }
    } else {
      print('The degree of the graphs are not significantly different')
      print(paste('p-value:', test$p.value))
    }
  }
  if (isTRUE(statistics['Betweenness'])){
    igraph::E(network1)$weight = 1
    igraph::E(network2)$weight = 1
    degrees_1 = igraph::betweenness(network1)
    print(paste('The average betweenness centrality of', net1_name, 'is:', mean(degrees_1)))
    degrees_2 = igraph::betweenness(network2)
    print(paste('The average betweenness centrality of', net2_name, 'is:', mean(degrees_2)))
    betweenness_plot = ggplot2::ggplot() + 
      ggplot2::geom_density(ggplot2::aes(x = degrees_1), color= 'Blue',fill="lightblue", alpha=0.4, bw =  max(quantile(degrees_2, c(.98))*1.05, quantile(degrees_1, c(.98))*1.05)/nbins) + 
      ggplot2::geom_density(ggplot2::aes(x = degrees_2), color = 'Orange', fill = '#fcd997', alpha=0.4, bw = max(quantile(degrees_2, c(.98))*1.05, quantile(degrees_1, c(.98))*1.05)/nbins) + 
      ggplot2::geom_vline(ggplot2::aes(xintercept = mean(degrees_2)), color = 'orange', linetype = 'dashed', linewidth = 1) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = mean(degrees_1)), color = 'blue', linetype = 'dashed', linewidth = 1) + 
      ggplot2::coord_cartesian(xlim = c(0, max(quantile(degrees_2, c(.98))*1.05, quantile(degrees_1, c(.98))*1.05)))+ ggplot2::theme(legend.position="right")
    test = stats::t.test(degrees_1, degrees_2, alternative = 'two.sided')
    if (test$p.value < 1 - significance_level){
      print('There is a significant difference between the degree of both graphs')
      print(paste('p-value:', test$p.value))
      if (test$statistic< 0){
        print(paste('Mean betweenness of', net1_name,'is significantly lower than that of', net2_name))
      } else{
        print(paste('Mean betweenness of', net2_name, 'is significantly lower than that of', net1_name))
      }
    } else {
      print('The betweeness centrality of the graphs are not significantly different')
      print(paste('p-value:', test$p.value))
    }
  }
  if (isTRUE(statistics['Closeness'])){
    igraph::E(network1)$weight = 1
    igraph::E(network2)$weight = 1
    degrees_1 = igraph::closeness(network1)
    print(paste('The average closeness centrality of', net1_name,'is:', mean(degrees_1, na.rm= T)))
    degrees_2 = igraph::closeness(network2)
    print(paste('The average closeness centrality of', net2_name,'is:', mean(degrees_2, na.rm = T)))
    closeness_plot = ggplot2::ggplot() + 
      ggplot2::geom_density(ggplot2::aes(x = degrees_1), color= 'Blue',fill="lightblue", alpha=0.4, bw =  1/nbins) + 
      ggplot2::geom_density(ggplot2::aes(x = degrees_2), color = 'Orange', fill = '#fcd997', alpha=0.4, bw = 1/nbins) + 
      ggplot2::geom_vline(ggplot2::aes(xintercept = mean(degrees_2, na.rm = T)), color = 'orange', linetype = 'dashed', linewidth = 1) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = mean(degrees_1, na.rm = T)), color = 'blue', linetype = 'dashed', linewidth = 1) + 
      ggplot2::coord_cartesian(xlim = c(0, 1))+ ggplot2::theme(legend.position="right")
    test = stats::t.test(degrees_1, degrees_2, alternative = 'two.sided')
    if (test$p.value < 1 - significance_level){
      print('There is a significant difference between the degree of both graphs')
      print(paste('p-value:', test$p.value))
      if (test$statistic< 0){
        print(paste('Mean closeness of', net1_name, 'is significantly lower than that of', net2_name))
      } else{
        print(paste('Mean closeness of', net2_name, 'is significantly lower than that of', net1_name))
      }
    } else {
      print('The closeness centrality of the graphs are not significantly different')
      print(paste('p-value:', test$p.value))
    }
  }
  return(list('degree_plot' = degree_plot, 'closeness_plot' = closeness_plot, 'betweenness_plot' = betweenness_plot))
}



#print(compare_statistics(network1 = graph_grants_people, network2 = graph_knowledge, statistics = c('degree' = F,'closeness' = T, 'betweenness' = F))$closeness_plot, bins = 300)

#igraph::E(graph_grants_people)$weight = 1
#igraph::E(graph_knowledge)$weight = 1
#max(quantile(igraph::closeness(graph_grants_people), c(.98), na.rm = T)*1.05, quantile(igraph::closeness(graph_knowledge), c(.98), na.rm = T)*1.05)
#igraph::closeness(graph_grants_people)

compare_actors = function(network1, network2, metric = 'Betweenness', net1_name, net2_name, n_actors = 5){
  print(paste('There are', length(intersect(igraph::V(network2)$name, igraph::V(network1)$name)), 'overlapping nodes in both networks'))
  print(paste0('This means that ', length(intersect(igraph::V(network2)$name, igraph::V(network1)$name))/length(igraph::V(network2)$name)*100, '% of the in ', net2_name, ' are in both networks'))
  print(paste0('This means that ', length(intersect(igraph::V(network2)$name, igraph::V(network1)$name))/length(igraph::V(network1)$name)*100, '% of the in ', net1_name, ' are in both networks'))
  print(length(setdiff(igraph::V(network2)$name, igraph::V(network1)$name)))
  print(length(setdiff(igraph::V(network1)$name, igraph::V(network2)$name)))
  if (metric == 'Closeness'){
    igraph::E(network1)$weight = 1
    igraph::E(network2)$weight = 1
    important_actors1 = names(sort(igraph::closeness(network1), decreasing = T)[1:n_actors])
    important_actors2 = names(sort(igraph::closeness(network2), decreasing = T)[1:n_actors])
    values1 = igraph::closeness(network1)[important_actors1]
    values2 = igraph::closeness(network2)[important_actors2]
  } else if (metric == 'Betweenness'){
    igraph::E(network1)$weight = 1
    igraph::E(network2)$weight = 1
    important_actors1 = names(sort(igraph::betweenness(network1), decreasing = T)[1:n_actors])
    important_actors2 = names(sort(igraph::betweenness(network2), decreasing = T)[1:n_actors])
    values1 = igraph::betweenness(network1)[important_actors1]
    values2 = igraph::betweenness(network2)[important_actors2]
  } else if (metric == 'Degree'){
    important_actors1 = names(sort(igraph::degree(network1), decreasing = T)[1:n_actors])
    important_actors2 = names(sort(igraph::degree(network2), decreasing = T)[1:n_actors])
    values1 = igraph::degree(network1)[important_actors1]
    values2 = igraph::degree(network2)[important_actors2]
  } else if (metric == 'Connectivity'){
    pass # Iets met igraph::delete_vertices, mean distances?
  } else{
    stop('No relevant statistic selected')
  }
  res_df = data.frame(cbind(important_actors1, important_actors2, values1, values2))
  colnames(res_df) = c(paste('Important actors', net1_name), paste('Important actors', net2_name), paste(metric, net1_name), paste(metric, net2_name))
  rownames(res_df) = stringr::str_to_title(paste0(ifelse(1:n_actors == 1, '' ,paste0(english::ordinal(1:n_actors), ' ')), rep('Most important', n_actors)))
  return(res_df)
}

#compare_actors(network1 = graph_grants_people, network2 = graph_knowledge)

bridge_comp = function(network1, network2, method = 'Mean', net1_name, net2_name, n_actors = 5, drop.inf = F, filter_bridge = F){
  network1 = igraph::simplify(network1)
  network2 = igraph::simplify(network2)
  if (method == 'Mean'){
    # most important bridges based upon the increase of the mean distance in the graph
    mean_distance_1 = igraph::mean_distance(network1, directed = F, weights = NA)
    mean_distance_2 = igraph::mean_distance(network2, directed = F, weights = NA)
    
    print(paste0('Original mean distance of ', net1_name, ' is: ', mean_distance_1))
    print(paste0('Original mean distance of ', net2_name, ' is: ', mean_distance_2))
    
    changed_distances_1 = c()
    changed_distances_2 = c()
    
    visits1 = c()
    visits2 = c()
    
    for (bridge in igraph::E(network1)){
      shiny::incProgress(amount = 1/(igraph::ecount(network1)+igraph::ecount(network2)))
      temp_network = igraph::delete.edges(network1, bridge)
      if (filter_bridge && igraph::distances(temp_network, 
                                             v = c(match(igraph::V(igraph::subgraph.edges(network1, igraph::E(network1)[bridge]))$name[1],
                                                         igraph::V(network1)$name)),
                                             to = c(match(igraph::V(igraph::subgraph.edges(network1, igraph::E(network1)[bridge]))$name[2],
                                                          igraph::V(network1)$name)),
                                             weights = NA)[1,1]<3){
        next
      }
      visits1 = c(visits1, bridge)
      changed_distances_1 = c(changed_distances_1, igraph::mean_distance(temp_network, directed = F, weights = NA))
    }
    changed_distances_1 = setNames(changed_distances_1, apply(igraph::get.edgelist(network1)[visits1,],1,paste, collapse = ' -- '))
    
    for (bridge in igraph::E(network2)){
      shiny::incProgress(amount = 1/(igraph::ecount(network1)+igraph::ecount(network2)))
      temp_network = igraph::delete.edges(network2, bridge)
      if (filter_bridge && igraph::distances(temp_network, 
                                             v = c(match(igraph::V(igraph::subgraph.edges(network2, igraph::E(network2)[bridge]))$name[1],
                                                         igraph::V(network2)$name)),
                                             to = c(match(igraph::V(igraph::subgraph.edges(network2, igraph::E(network2)[bridge]))$name[2],
                                                          igraph::V(network2)$name)),
                                             weights = NA)[1,1]<3){
        next
      }
      visits2 = c(visits2, bridge)
      changed_distances_2 = c(changed_distances_2, igraph::mean_distance(temp_network, directed = F, weights = NA))
    }
    changed_distances_2 = setNames(changed_distances_2, apply(igraph::get.edgelist(network2)[visits2,],1,paste, collapse = ' -- '))
    
    if (filter_bridge){
      print(paste(net1_name, 'has',length(changed_distances_1), 'Bridges'))
      print(paste(net2_name, 'has',length(changed_distances_2), 'Bridges'))
    } else {
      print(paste(net1_name, 'has',length(changed_distances_1), 'Edges'))
      print(paste(net2_name, 'has',length(changed_distances_2), 'Edges'))
    }
    
    best_bridges_1 = names(sort(changed_distances_1, decreasing = T)[1:n_actors])
    best_bridges_2 = names(sort(changed_distances_2, decreasing = T)[1:n_actors])
    
    dist_increase1 = changed_distances_1[best_bridges_1] - mean_distance_1
    dist_increase2 = changed_distances_2[best_bridges_2] - mean_distance_2
    
  } else if (method == 'Absolute'){
    changed_distances_1 = c()
    changed_distances_2 = c()
    
    for (bridge in igraph::E(network1)){
      shiny::incProgress(amount = 1/(igraph::ecount(network1)+igraph::ecount(network2)))
      nodes = igraph::V(igraph::subgraph.edges(network1, igraph::E(network1)[bridge]))$name 
      node1_id = match(nodes[1], igraph::V(network1)$name)
      node2_id = match(nodes[2], igraph::V(network1)$name)
      temp_network = igraph::delete.edges(network1, bridge)
      changed_distances_1 = c(changed_distances_1, igraph::distances(temp_network, v = c(node1_id), to = c(node2_id), weights = NA)[1,1])
    }
    changed_distances_1 = setNames(changed_distances_1, apply(igraph::get.edgelist(network1)[igraph::E(network1),],1,paste, collapse = ' -- '))
    
    for (bridge in igraph::E(network2)){
      shiny::incProgress(amount = 1/(igraph::ecount(network1)+igraph::ecount(network2)))
      nodes = igraph::V(igraph::subgraph.edges(network2, igraph::E(network2)[bridge]))$name 
      node1_id = match(nodes[1], igraph::V(network2)$name)
      node2_id = match(nodes[2], igraph::V(network2)$name)
      temp_network = igraph::delete.edges(network2, bridge)
      changed_distances_2 = c(changed_distances_2, igraph::distances(temp_network, v = c(node1_id), to = c(node2_id), weights = NA)[1,1])
    }
    changed_distances_2 = setNames(changed_distances_2, apply(igraph::get.edgelist(network2)[igraph::E(network2),],1,paste, collapse = ' -- '))
    
    if (drop.inf){
      changed_distances_1 = subset(changed_distances_1, changed_distances_1!=Inf)
      changed_distances_2 = subset(changed_distances_2, changed_distances_2!=Inf)
    } 
    
    if (filter_bridge){
      best_bridges_1 = names(sort(changed_distances_1[changed_distances_1>2], decreasing = T)[1:n_actors])
      best_bridges_2 = names(sort(changed_distances_2[changed_distances_2>2], decreasing = T)[1:n_actors])  
    } else{
      best_bridges_1 = names(sort(changed_distances_1, decreasing = T)[1:n_actors])
      best_bridges_2 = names(sort(changed_distances_2, decreasing = T)[1:n_actors])
    }
    
    if (filter_bridge){
      print(paste(net1_name, 'has',sum(changed_distances_1>2), 'Bridges'))
      print(paste(net2_name, 'has',sum(changed_distances_2>2), 'Bridges'))
    } else {
      print(paste(net1_name, 'has',length(changed_distances_1), 'Edges'))
      print(paste(net2_name, 'has',length(changed_distances_2), 'Edges'))
    }
    
    dist_increase1 = changed_distances_1[best_bridges_1] - 1
    dist_increase2 = changed_distances_2[best_bridges_2] - 1
    
  } else{
    stop('No relevant statistic selected')
  }
  res_df = data.frame(cbind(best_bridges_1, best_bridges_2, dist_increase1, dist_increase2))
  colnames(res_df) = c(paste('Important ties', net1_name), paste('Important ties', net2_name), paste(method, 'distance increase', net1_name), paste(method, 'distance increase',net2_name))
  rownames(res_df) = stringr::str_to_title(paste0(ifelse(1:n_actors == 1, '' ,paste0(english::ordinal(1:n_actors), ' ')), rep('Most important', n_actors)))
  return(res_df)
  
}



#bridge_comp(graph_grants_people, graph_co_author, net1_name = 'hoi', net2_name = 'hoi', method = 'Absolute')

#for (i in igraph::bridges(graph_grants_people)){
#  print(igraph::E(graph_grants_people)[i])
#}
#igraph::as_edgelist()
#igraph::mean_distance(graph_grants_people, directed = F, weights = NA, details = T)
#igraph::bridges(graph_grants_people)
#na
#data.frame(cbind(1:5, 6:10, 11:15))
#igraph::delete.edges(graph_grants_people, '2')
#graph_grants_people
#c('4' = 6, '2' = 7, '9' = 2)

#igraph::distances(graph_grants_people, v = c(361), to = c(362), weights = NA)[1,1]
#match(92996508, igraph::V(graph_grants_people)$name)

#igraph::as_edgelist(igraph::E(graph_grants_people)[4])

#igraph::V(igraph::subgraph.edges(graph_grants_people, igraph::E(graph_grants_people)[4]))$name

#igraph::V(igraph::subgraph.edges(graph_grants_people, igraph::E(graph_grants_people)[igraph::bridges(graph_grants_people)]))$name

#setNames(1:12, igraph::E(graph_grants_people)[igraph::bridges(graph_grants_people)])

#igraph::bridges(graph_grants_people)
#apply(igraph::get.edgelist(graph_grants_people)[igraph::bridges(graph_grants_people),],1,paste, collapse = ' -- ')
