###############################################################
# Spectrum computation for a random graph
###############################################################
import sys
sys.path.append("/Users/marcogori/anaconda3/lib/python3.7/site-packages")
from networkx.generators.random_graphs import erdos_renyi_graph

import matplotlib.pyplot as plt
import networkx as nx
import numpy.linalg

n = 10  # 1000 nodes
m = 40  # 45000 edges
G = nx.gnm_random_graph(n, m)

plt.subplot(121)
nx.draw_shell(G)

L = nx.normalized_laplacian_matrix(G)
e = numpy.linalg.eigvals(L.A)
print("Largest eigenvalue:", max(e))
print("Smallest eigenvalue:", min(e))
plt.subplot(122)
plt.hist(e, bins=100)  # histogram with 100 bins
plt.xlim(0, 2)  # eigenvalues between 0 and 2
plt.show()
