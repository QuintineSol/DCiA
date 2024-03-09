source('data loader.R')

# Basic descriptives and plots ____________________________________________

# Grants data
snafun::print(network_grants)

plot(network_grants, 
     vertex.size = 7,,
     main = 'Grants',
     edge.arrow.size = .2,  
     edge.curved = .1,
     vertex.label.cex = 0.6,  # vertex label size
     vertex.label.color = "black", 
     edge.color = "blue",
     vertex.color = "orange", 
     vertex.frame.color = "green") 
snafun::extract_comm_walktrap(network_grants)


# Grants-people data
snafun::print(network_grants_people)
snafun::g_summary(network_grants_people)

plot(network_grants_people, 
     vertex.size = 7,
     main = 'Grants people',
     edge.arrow.size = .2,  
     edge.curved = .1,
     vertex.label.cex = 0.6,  # vertex label size
     vertex.label.color = "black", 
     edge.color = "blue",
     vertex.color = "orange", 
     vertex.frame.color = "green") 
snafun::extract_comm_walktrap(network_grants_people)



# Knowledge data
snafun::print(network_knowledge)
snafun::g_summary(network_knowledge)

plot(network_knowledge, 
     vertex.size = 7,,
     main = 'Knowledge',
     edge.arrow.size = .2,  
     edge.curved = .1,
     vertex.label.cex = 0.6,  # vertex label size
     vertex.label.color = "black", 
     edge.color = "blue",
     vertex.color = "orange", 
     vertex.frame.color = "green") 
snafun::extract_comm_walktrap(network_knowledge)



# Co-author data
snafun::print(network_co_author)
snafun::g_summary(network_co_author)

plot(network_co_author, 
     vertex.size = 7,,
     main = 'Co-author',
     edge.arrow.size = .2,  
     edge.curved = .1,
     vertex.label.cex = 0.6,  # vertex label size
     vertex.label.color = "black", 
     edge.color = "blue",
     vertex.color = "orange", 
     vertex.frame.color = "green") 
snafun::extract_comm_walktrap(network_co_author)

 

#_____________________________________________________________________________________


