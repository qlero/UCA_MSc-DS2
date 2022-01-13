###############################################################
# Generation of a Random Directed Acyclic Graph
# It returns a random k-out graph with preferential attachment.
# a: The number of nodes in the returned graph.
# b: The out-degree of each node in the returned graph.
# c: Initial value of each vertex

#1. Begin with an empty digraph, and initially set each node to have weight alpha.
#2. Choose a node u with out-degree less than k uniformly at random.
#3. Choose a node v from with probability proportional to its weight.
#4. Add a directed edge from u to v, and increase the weight of v by one.
#5. If each node has out-degree k, halt, otherwise repeat from step 2.

###############################################################
import sys
sys.path.append("/Users/marcogori/anaconda3/lib/python3.7/site-packages")
from networkx.generators.random_graphs import erdos_renyi_graph
import numpy as np
import networkx as nx
import matplotlib.pyplot as plt

G = nx.generators.directed.random_k_out_graph(10, 3, 0.5)
#nx.draw(G, with_labels=True, font_weight='bold')
nx.draw_shell(G, with_labels=True, font_weight='bold') #shell layout
plt.show()
