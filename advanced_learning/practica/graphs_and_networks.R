libraries = c("igraph", "network", "sne")
#install.packages(libraries)
library(igraph)

# By default, the graph.adjacency function 
# sees the networks as directed
A = cbind(c())
A = cbind(c(0, 1, 1, 1), # represents a star where
          c(1, 0, 0, 0), # every node is connected
          c(1, 0, 0, 0), # to the first one
          c(1, 0, 0, 0))

# To inform the function that the network is 
# undirected, we have to use the mode option
g = graph.adjacency(A) # creates a network object
g_un = graph.adjacency(A, mode="undirected")
plot(g)
degree(g)
plot(g_un)

# Another statistic: the numbers of cliques
cliques(g)
# here we have 4 1-cliques, and 3 2-cliques

# Exercise: write a small code which computes a signature 
# for the network by counting how many cliques of each type we
# have in this network
clq = cliques(g)

graph_signature <- function(graph){
  signature = rep(0, length(graph[1]))
  for (clique in cliques(graph)){
    len = length(clique)
    signature[len] = signature[len] + 1 
  }
  signature
}
barplot(graph_signature(g), col = c(1, 2))

