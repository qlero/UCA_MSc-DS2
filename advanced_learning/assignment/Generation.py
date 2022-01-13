#################################################################
# Generation of a graph by specific definition of nodes and arcs
#################################################################

import sys
sys.path.append("/Users/marcogori/anaconda3/lib/python3.7/site-packages")
from networkx.generators.random_graphs import erdos_renyi_graph
import networkx as nx
import matplotlib.pyplot as plt

g=nx.Graph()
# a list of nodes:
g.add_nodes_from(["a","b","c","d"])

# adding a list of edges:
g.add_edges_from([("a","c"),("a","b"),("a","d"),("b","c"),("b","d")])

print(g.nodes)
print(g.edges)

nx.draw_shell(g)
#nx.draw(g)
plt.savefig("simple_path.png") # save as png
plt.show() # display
