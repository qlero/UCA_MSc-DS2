#################################################################
# Generation of a graph by specific definition of nodes and arcs
# Notice that we deal with undirected graphs! 
#################################################################
import sys
sys.path.append("/Users/marcogori/anaconda3/lib/python3.7/site-packages")
from networkx.generators.random_graphs import erdos_renyi_graph
import numpy as np
import networkx as nx
import matplotlib.pyplot as plt

G=nx.Graph()
#G.add_node(1)
G.add_nodes_from([1, 2, 3, 4, 5])
#G.add_nodes_from([  (4, {"color": "red"}),(5, {"color": "green"}),])
#G.add_edge(1, 2)
G.add_edges_from([(1, 2), (1, 3), (2, 3), (3, 4), (4, 5)])

# Compute PageRank
pr=nx.pagerank(G,0.8)
print("PageRank of the nodes:")
print(pr)

sum=0
for i in range(1,6): sum=sum+pr[i]
print("\n")
print("Sum of the ranks:", sum)

nx.draw_shell(G, with_labels=True, font_weight='bold')
#nx.draw_shell(G, nlist=[range(5, 10), range(5)], with_labels=True, font_weight='bold')
plt.show()
