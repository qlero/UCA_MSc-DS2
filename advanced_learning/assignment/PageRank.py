import sys
sys.path.append("/Users/marcogori/anaconda3/lib/python3.7/site-packages")
from networkx.generators.random_graphs import erdos_renyi_graph
import networkx as nx
import matplotlib.pyplot as plt

n = 5 # 100K, then 1M, then 10M...
p=0.2
Gs = nx.scale_free_graph(n)
Gr = nx.erdos_renyi_graph(n,p)
#G = nx.complete_graph(4)

#G=nx.barabasi_albert_graph(6,4) 
pr=nx.pagerank(Gr,0.4) 
pr
print(pr)

nx.draw_shell(Gr)
#nx.draw_shell(Gs)
plt.show()
