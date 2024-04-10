####  QAP
QAP_networks = function(network1, network2, reps = 100){
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
  
  if (length(intersect(igraph::V(network1)$name, igraph::V(network2)$name)) == length(igraph::V(network1)$name)){
    # Networks have the same nodes, thus there do not need to be any nodes added to any of the networks
    
    # Simplify the networks and convert them to network class
    print('Converting networks...')
    network1 = intergraph::asNetwork(igraph::simplify(network1))
    network2 = intergraph::asNetwork(igraph::simplify(network2))
    # Return the qaptest, such that further applications can be built on top
    return(sna::qaptest(list(network1, network2), FUN = sna::gcor, reps = reps, g1 = 1, g2 = 2))
    
  } else if (length(setdiff(igraph::V(network1)$name, igraph::V(network2)$name))){
    # There are nodes in network 1 which are not in network 2
    print('Unseen nodes in network 1...')
    if (length(setdiff(igraph::V(network2)$name, igraph::V(network1)$name))){
      print('Unseen nodes in network 2...')
      # Both networks have nodes which are not in the other network
      
      # Get the name of the nodes which need to be added to the other network (as isolates)
      nodes_in_2 = setdiff(igraph::V(network2)$name, igraph::V(network1)$name)
      nodes_in_1 = setdiff(igraph::V(network1)$name, igraph::V(network2)$name)
      
      #Add the vertices to the new graphs
      print(paste('Adding', length(setdiff(igraph::V(network2)$name, igraph::V(network1)$name)), 'Nodes to network 1'))
      network1 = igraph::add_vertices(network1, length(nodes_in_2), name = nodes_in_2)
      print(paste('Adding', length(setdiff(igraph::V(network1)$name, igraph::V(network2)$name)), 'Nodes to network 2'))
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
    print('Unseen nodes in network 2...')
    # Only network 2 has additional nodes
    
    # Get the name of the nodes which need to be added to the other network (as isolates)
    nodes_in_2 = setdiff(igraph::V(network2)$name, igraph::V(network1)$name)
    
    #Add the vertices to the new graphs
    print(paste('Adding', length(setdiff(igraph::V(network2)$name, igraph::V(network1)$name)), 'Nodes to network 1'))
    network1 = igraph::add_vertices(network1, length(nodes_in_2), name = nodes_in_2)
    
    # Simplify the networks and convert them to network class
    print('Converting networks...')
    network1 = intergraph::asNetwork(igraph::simplify(network1))
    network2 = intergraph::asNetwork(igraph::simplify(network2))
    # Return the qaptest, such that further applications can be built on top
    return(sna::qaptest(list(network1, network2), FUN = sna::gcor, reps = reps, g1 = 1, g2 = 2))
  }
}

sna::plot.qaptest(QAP_networks(graph_grants_people, graph_knowledge, reps = 100))
QAP_networks(graph_grants_people, graph_knowledge, reps = 100)

compare_statistics = function(network1, network2, significance_level = 0.95, statistics = NULL, nbins = 100){
  if (isTRUE(statistics['degree'])){
    degrees_1 = igraph::degree(network1)
    print(paste('The average degree of the first network is:', mean(degrees_1)))
    degrees_2 = igraph::degree(network2)
    print(paste('The average degree of the second network is:', mean(degrees_2)))
    print(ggplot2::ggplot() + 
    ggplot2::geom_density(ggplot2::aes(x = igraph::degree(network1), y = ggplot2::after_stat(density)), color= 'Blue',fill="lightblue", alpha=0.4, bw =  max(quantile(degrees_2, c(.98))*1.05, quantile(degrees_1, c(.98))*1.05)/nbins) + 
    ggplot2::geom_density(ggplot2::aes(x = igraph::degree(network2), y = ggplot2::after_stat(density)), color = 'Orange', fill = '#fcd997', alpha=0.4, bw =  max(quantile(degrees_2, c(.98))*1.05, quantile(degrees_1, c(.98))*1.05)/nbins) + 
    ggplot2::geom_vline(ggplot2::aes(xintercept = mean(igraph::degree(network2))), color = 'orange', linetype = 'dashed', linewidth = max(quantile(igraph::degree(network2), c(.98))*1.05, quantile(igraph::degree(network1), c(.98))*1.05)/100) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = mean(igraph::degree(network1))), color = 'blue', linetype = 'dashed', linewidth = max(quantile(igraph::degree(network2), c(.98))*1.05, quantile(igraph::degree(network1), c(.98))*1.05)/100) + 
    ggplot2::coord_cartesian(xlim = c(0, max(quantile(igraph::degree(network2), c(.98))*1.05, quantile(igraph::degree(network1), c(.98))*1.05)), expand = F)+ ggplot2::theme(legend.position="right"))
    test = stats::t.test(degrees_1, degrees_2, alternative = 'two.sided')
    if (test$p.value < 1 - significance_level){
      print('There is a significant difference between the degree of both graphs')
      print(paste('p-value:', test$p.value))
      if (test$statistic< 0){
        print('Mean degree of network 1 is significantly lower than that of network 2')
      } else{
        print('Mean degree of network 2 is significantly lower than that of network 1')
      }
    } else {
      print('The degree of the graphs are not significantly different')
      print(paste('p-value:', test$p.value))
    }
  }
  if (isTRUE(statistics['betweenness'])){
    igraph::E(network1)$weight = 1
    igraph::E(network2)$weight = 1
    degrees_1 = igraph::betweenness(network1)
    print(paste('The average betweenness centrality of the first network is:', mean(degrees_1)))
    degrees_2 = igraph::betweenness(network2)
    print(paste('The average betweenness centrality of the second network is:', mean(degrees_2)))
    print(ggplot2::ggplot() + 
            ggplot2::geom_density(ggplot2::aes(x = degrees_1), color= 'Blue',fill="lightblue", alpha=0.4, bw =  max(quantile(degrees_2, c(.98))*1.05, quantile(degrees_1, c(.98))*1.05)/nbins) + 
            ggplot2::geom_density(ggplot2::aes(x = degrees_2), color = 'Orange', fill = '#fcd997', alpha=0.4, bw = max(quantile(degrees_2, c(.98))*1.05, quantile(degrees_1, c(.98))*1.05)/nbins) + 
            ggplot2::geom_vline(ggplot2::aes(xintercept = mean(degrees_2)), color = 'orange', linetype = 'dashed', linewidth = 1) +
            ggplot2::geom_vline(ggplot2::aes(xintercept = mean(degrees_1)), color = 'blue', linetype = 'dashed', linewidth = 1) + 
            ggplot2::coord_cartesian(xlim = c(0, max(quantile(degrees_2, c(.98))*1.05, quantile(degrees_1, c(.98))*1.05)))+ ggplot2::theme(legend.position="right"))
    test = stats::t.test(degrees_1, degrees_2, alternative = 'two.sided')
    if (test$p.value < 1 - significance_level){
      print('There is a significant difference between the degree of both graphs')
      print(paste('p-value:', test$p.value))
      if (test$statistic< 0){
        print('Mean betweenness of network 1 is significantly lower than that of network 2')
      } else{
        print('Mean betweenness of network 2 is significantly lower than that of network 1')
      }
    } else {
      print('The betweeness centrality of the graphs are not significantly different')
      print(paste('p-value:', test$p.value))
    }
  }
  if (isTRUE(statistics['closeness'])){
    igraph::E(network1)$weight = 1
    igraph::E(network2)$weight = 1
    degrees_1 = igraph::closeness(network1)
    print(paste('The average betweenness centrality of the first network is:', mean(degrees_1)))
    degrees_2 = igraph::closeness(network2)
    print(paste('The average betweenness centrality of the second network is:', mean(degrees_2)))
    print(ggplot2::ggplot() + 
            ggplot2::geom_density(ggplot2::aes(x = degrees_1), color= 'Blue',fill="lightblue", alpha=0.4, bw =  max(quantile(degrees_2, c(.98))*1.05, quantile(degrees_1, c(.98))*1.05)/nbins) + 
            ggplot2::geom_density(ggplot2::aes(x = degrees_2), color = 'Orange', fill = '#fcd997', alpha=0.4, bw = max(quantile(degrees_2, c(.98))*1.05, quantile(degrees_1, c(.98))*1.05)/nbins) + 
  ggplot2::geom_vline(ggplot2::aes(xintercept = mean(degrees_2)), color = 'orange', linetype = 'dashed', linewidth = 1) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = mean(degrees_1)), color = 'blue', linetype = 'dashed', linewidth = 1) + 
    ggplot2::coord_cartesian(xlim = c(0, max(quantile(degrees_2, c(.98))*1.05, quantile(degrees_1, c(.98))*1.05)))+ ggplot2::theme(legend.position="right"))
test = stats::t.test(degrees_1, degrees_2, alternative = 'two.sided')
if (test$p.value < 1 - significance_level){
  print('There is a significant difference between the degree of both graphs')
  print(paste('p-value:', test$p.value))
  if (test$statistic< 0){
    print('Mean betweenness of network 1 is significantly lower than that of network 2')
  } else{
    print('Mean betweenness of network 2 is significantly lower than that of network 1')
  }
} else {
  print('The betweeness centrality of the graphs are not significantly different')
  print(paste('p-value:', test$p.value))
}
}
}

compare_statistics(network1 = graph_grants_people, network2 = graph_co_author, statistics = c('degree' = T, 'betweenness' = T))

compare_actors = function(network1, network2){
  print(paste('There are', length(intersect(igraph::V(network2)$name, igraph::V(network1)$name)), 'overlapping nodes in both networks'))
  print(paste0('This meeans that ', length(intersect(igraph::V(network2)$name, igraph::V(network1)$name))/length(igraph::V(network2)$name)*100, '% of the in network 2 are in both networks'))
  print(paste0('This meeans that ', length(intersect(igraph::V(network2)$name, igraph::V(network1)$name))/length(igraph::V(network1)$name)*100, '% of the in network 1 are in both networks'))
  print(length(setdiff(igraph::V(network2)$name, igraph::V(network1)$name)))
  print(length(setdiff(igraph::V(network1)$name, igraph::V(network2)$name)))
}

compare_actors(network1 = graph_grants_people, network2 = graph_knowledge)

graph_grants_people

