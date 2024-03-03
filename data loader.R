
load_network = function(network, output = 'network', vlist = NULL, directed = F){
  #' The load network function takes a network-like object and returns an attribute with a network object
  #'
  #' The network input can be defined in mutiple different ways:
  #' - An igraph object
  #' - A network object
  #' - An edge list (Defined as a matrix with size x, 2) and optional vertex list
  #' - An incidence matrix (if its a matrix with size n, m)
  #' - An adjacency matrix (if its a matrix with size n, n)
  #'
  #' There multiple different parameters which can be defined:
  #' output: Defines which output is generated. Can be either 'network' (default) or 'igraph'
  #' vlist (optional): List of all vertices to provide when providing an edge list
  #' directed (optional): Whether the network is directed or not (default is undirected)
  if (snafun::is_igraph(network)){
    # igraph
    if (output == 'network'){
      return(snafun::to_network(network))
    } else if (output == 'igraph'){
      return(network)
    }
  }else if (snafun::is_network(network)){
    #network
    if (output == 'network'){
      return(network)
    } else if (output == 'igraph'){
      return(snafun::to_igraph(network))
    }
  }else if (class(network)[1]=='matrix'){
    if (dim(network)[1] == dim(network)[2]){
      if (all(t(network) == network)){
        # Undirected Adjacency matrix
        if (output == 'network'){
          return(snafun::to_network(igraph::graph_from_adjacency_matrix(network, mode = 'undirected', diag = FALSE)))
        } else if (output == 'igraph'){
          return(igraph::graph_from_adjacency_matrix(network, mode = 'undirected', diag = FALSE))
        }
      } else {
        # Directed Adjacency matrix (nxn)
        if (output == 'network'){
          return(snafun::to_network(igraph::graph_from_adjacency_matrix(network, mode = 'directed', diag = FALSE)))
        } else if (output == 'igraph'){
          return(igraph::graph_from_adjacency_matrix(network, mode = 'directed', diag = FALSE))
        }
      }
    } else if (dim(network)[2] == 2){
      # edge list
      bipartite = length(intersect(network[,1], network[,2])) == 0
      if (bipartite){
        net = igraph::graph_from_data_frame(network, directed = directed)
        igraph::V(net)$type = ifelse(igraph::V(net)$name %in% network[,1], 
                               yes = FALSE, no = TRUE
        ) # Create bipartite division between the nodes
        if (output == 'network'){
          return(snafun::to_network(snafun::to_matrix(net), bipartite = T))
        } else if (output == 'igraph'){
          return(net)
        }
      } else {
        if (output == 'network'){
          return(snafun::to_network(igraph::graph_from_data_frame(network, directed = directed, vertices = vlist)))
        } else if (output == 'igraph'){
          return(igraph::graph_from_data_frame(network, directed = directed, vertices = vlist))
        }
      }
    } else{
      # Incidence matrix (mxn)
      if (output == 'network'){
        return(snafun::to_network(igraph::graph_from_incidence_matrix(network, directed = directed, mode = 'out'), bipartite = T))
      } else if (output == 'igraph'){
        return(igraph::graph_from_incidence_matrix(network, directed = directed, mode = 'out'))
      }
    }
  }
}

apollo_network_from_excel = function(directory = NULL, sheet_number = 1, type = 'igraph'){
  #' A function to easily load the apollo dataset into an igraph or network object
  #'
  #' There multiple different parameters which can be defined:
  #' directory (optional): directory of the file. If empty, file will be searched for in working directory
  #' sheet_number (optional): sheet number which must be imported. If empty, will be equal to the first sheet
  #' type: Defines which output is generated. Can be either 'network' or 'igraph' (default)
  
  if (!is.null(directory)){
    return(load_network(as.matrix(readxl::read_excel(directory, sheet = sheet_number)), output = type))
  } else{
    return(load_network(as.matrix(readxl::read_excel(paste0(getwd(), '/Apollo_Student_Data 2024.xlsx'), sheet = sheet_number)), output = type))
  }
}

plot(network_from_excel())
apollo_network_from_excel(type = 'network')

