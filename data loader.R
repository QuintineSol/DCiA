
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

apollo_network_from_excel = function(directory = NULL, sheet = c('attributes', 'knowledge', 'co-author', 'grants_people', 'grants'), type = 'igraph'){
  #' A function to easily load the Apollo dataset into an igraph or network object (test)
  #'
  #' There multiple different parameters which can be defined: 
  #' directory (optional): directory of the excel files. If empty, file will be searched for in the data folder within the directory of this file
  #' sheet: sheet type which must be imported. Has to be one of the following:
  #' - attributes: Collects the attributes of all the nodes based on Attribute table_Final.xlsx and returns them in 3 separate tables including added explanation
  #' - knowledge: Returns the knowledge network
  #' - co-author: Returns the co-authorship network
  #' - grants_people: Returns the unipartite projection of the grant network
  #' - grants: Returns the bipartite grant network
  #' type: Defines which output is generated. Can be either 'network' or 'igraph' (default)
  suppressWarnings({
  match.arg(sheet)
  
  file_names = c('attributes' = 'Attribute table_Final.xlsx',
                 'knowledge' = 'knowledge_sharing_people_to_people.csv',
                 'co-author' = 'co_authorship_people_to_people.csv',
                 'grants_people' = 'grants_people_to_people.csv',
                 'grants' = 'grants_people_to_grant application.csv')
  
  if (sheet != 'attributes'){
    if (!is.null(directory)){
      return(load_network(as.matrix(read.csv(paste0(directory,'/', file_names[sheet]), sheet = sheet_number)), output = type))
    } else{
      return(load_network(as.matrix(read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path), '/Data/', file_names[sheet]))), output = type))
    }
  } else{
    if (is.null(directory)){
      temp = as.data.frame(readxl::read_xlsx(paste0(dirname(rstudioapi::getSourceEditorContext()$path), 
                                                    '/Data/', file_names[sheet]), col_types = 'text'))
      temp2 = as.data.frame(readxl::read_xlsx(paste0(dirname(rstudioapi::getSourceEditorContext()$path), 
                                                     '/Data/', file_names[sheet]), col_types = 'text', sheet = 2, col_names = F))
    } else{
      temp = as.data.frame(readxl::read_xlsx(paste0(directory, 
                                                    '/', file_names[sheet]), col_types = 'text'))
      temp2 = as.data.frame(readxl::read_xlsx(paste0(directory, 
                                                     '/', file_names[sheet]), col_types = 'text', sheet = 2, col_names = F))
    }
    person_start = rownames(temp[which(as.integer(temp$Node)>999999)[1],])
    doi_start = rownames(temp[which(is.na(as.integer(temp$Node)))[1],])
    paper_table = temp[doi_start:nrow(temp),]
    people_table = temp[person_start:doi_start,]
    grant_table = temp[1:person_start,]
    
    faculty_dct = setNames(temp2[2:5, '...1'], temp2[2:5, '...2'])
    PI_dct = setNames(temp2[c(9,10), '...1'], temp2[c(9,10), '...2'])
    staff_dct = setNames(temp2[c(2,3), '...5'], temp2[c(2,3), '...6'])
    grant_dct = setNames(temp2[c(9,10), '...5'], temp2[c(9,10), '...6'])
    explenations = temp2[which(!is.na(temp2$...9)), c('...8', '...9')]
    colnames(explenations) = c('Term', 'Definition')
    
    grant_table = grant_table[, c('Node', 'Grant Application Academic Year', 'Grant Awarded')]
    grant_table$`Grant Awarded` = grant_dct[grant_table$`Grant Awarded`]
    grant_table$Node = as.integer(grant_table$Node)
    grant_table$`Grant Awarded` = as.factor(grant_table$`Grant Awarded`)
    
    people_table = people_table[, !names(people_table) %in% c('Grant Application Academic Year', 'Grant Awarded', 'DOI Year')]
    people_table$`PI/Not a PI` = PI_dct[people_table$`PI/Not a PI`]
    people_table$Faculty = faculty_dct[people_table$Faculty]
    people_table$Faculty[is.na(people_table$Faculty)] = 'External Faculty'
    people_table$`Internal Staff vs. External Staff` = staff_dct[people_table$`Internal Staff vs. External Staff`]
    people_table$Node = as.integer(people_table$Node)
    people_table$Faculty = as.factor(people_table$Faculty)
    people_table$`PI/Not a PI` = as.factor(people_table$`PI/Not a PI`)
    people_table$`Internal Staff vs. External Staff` = as.factor(people_table$`Internal Staff vs. External Staff`)
    
    
    paper_table = paper_table[,c('Node', 'DOI Year')]
    paper_table$`DOI Year` = as.integer(paper_table$`DOI Year`)
    return(list('paper_table' = paper_table, 'people_table' = people_table, 'grant_table' = grant_table, 'explanations' = explenations))
  }
  })
}


apollo_network_from_excel(sheet= 'co-author', type = 'network')
