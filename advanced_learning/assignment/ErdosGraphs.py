###############################################################
# Generation of a Random Graph - Erdos-Renyi graphs
# n: Number of nodes
# p: Probability for edge creation.
###############################################################

import sys
sys.path.append("/Users/marcogori/anaconda3/lib/python3.7/site-packages")
from networkx.generators.random_graphs import erdos_renyi_graph
import networkx as nx
import matplotlib.pyplot as plt
n = 30
p = 0.5
g = erdos_renyi_graph(n, p)
print(g.nodes)
print(g.edges)
nx.draw_shell(g)
plt.show()
