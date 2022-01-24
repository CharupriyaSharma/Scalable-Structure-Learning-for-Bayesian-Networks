import pandas as pd
import networkx as nx
import sys
import os
from cdt.causality.graph import GES
obj = GES(score='int')
df = pd.read_csv(sys.argv[1])
names = ["v" + str(i) for i in list(range(0,len(df.columns)))]
df.set_axis(names, axis=1, inplace=True)
G= obj.create_graph_from_data(df)
filename = os.path.basename(sys.argv[1])
prefix = os.path.splitext(filename)[0]
#print(prefix)


#f = open("learned_networks/"+prefix+".ges", "w")
f = open("temp", "w")

for n in G.nodes :
    s = n[1:] +"<-"
#    print(G.in_edges(n))
    for e in list(G.in_edges(n)):
     	s = s + e[0][1:] + ","
    s = s +" -0.01\n"
    f.write(s)
f.write("BN score is 0\n")
f.close()

#gd = obj.orient_undirected_graph(df,G)

#f = open("learned_networks/"+sys.argv[2]+"_"+prefix+".ges.directed", "w")
#for n in gd.nodes :
#    s = n[1:] +"<-"
#    print(gd.in_edges(n))
#    
#    for e in list(gd.in_edges(n)):
#     	s = s + e[0][1:] + ","
#    s = s +" -0.01\n"
#    f.write(s)
#f.write("BN score is 0\n")
#f.close()
