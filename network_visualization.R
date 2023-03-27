
# -------------- Read data -----------------------------
# loading igraph library
library(igraph)
data <- read.csv("Data/data-nameID-uni-country.csv", header=FALSE)
head(data)

# # ----------------------unipartite-------------------------------
# # trying to make a unipartite graph from edge list
# m <- as.matrix(get.adjacency(graph.data.frame(data)))
# head(m)
# g2 <- graph_from_incidence_matrix(m)
# g2.bp <- bipartite.projection(g2)
# plot(g2.bp$proj1, vertex.label.color="black", vertex.label.dist=1, vertex.size=7)

# ---------------Bipartite graph----------------------------------
# data <- read.csv("data.csv", header=FALSE)
head(data)
g <- graph.data.frame(data, directed=TRUE)
bipartite.mapping(g)
V(g)$type <- bipartite_mapping(g)$type 
V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
V(g)$shape <- ifelse(V(g)$type, "square", "circle")
E(g)$color <- "black"
plot(g, vertex.label.cex = 0.4,edge.arrow.size = 0.2, vertex.label.color = "black", edge.length=50)
# plot(g, layout=layout.bipartite, vertex.size=7, vertex.label.cex=0.6)

# Unipartite from Bipartite:
V(g)$size = degree(g)*1.5
uni_graph <- bipartite.projection(g)
g1 <- uni_graph$proj1 #faculty
g2 <- uni_graph$proj2 #institutes/universities
plot(uni_graph$proj2, edge.arrow.size = 0.4, vertex.label.cex = 0.4, vertex.label.color = "black", layout=layout.circle)
# ----------------------- archive --------------------------
# as_adjacency_matrix(g)
g <- graph_from_edgelist(as.matrix(data), directed = TRUE)
# plot(g)
V(g)$color = "yellow"
max_nodes = which.max(degree(g))
V(g)$color[max_nodes] = "red"
V(g)$size = 10
V(g)$size[max_nodes] = 15
# plot(g, vertex.label = NA, edge.arrow.size = 0.4, edge.color = "black")
plot(g, vertex.label = degree, edge.arrow.size = 0.4, edge.color = "black")
# hist(degree(g))
# hist(degree(g, mode="in"))
# hist(degree(g, mode="out"))
# plot(degree(g, mode="in"), degree(g, mode="out"))



















# ---- Export graph as edge list ------------------------
edge_list1 <- as_edgelist(uni_graph$proj1)
write.csv(edge_list1,'Data/data-taught-faculty.csv')

edge_list2 <- as_edgelist(uni_graph$proj2)
write.csv(edge_list2,'Data/data-taught-institutes.csv')

# ------- Network Measures ------------------------------------
print(average.path.length(g1))
d = degree(g1)

max_d = max(d)
print(max_d)
min_d = min(d)
print(min_d)

pos_max = which.max(d) # node with max degree
print(pos_max)

pos_min = which.min(d) # node with min degree
print(pos_min)

mean_d = mean(d)
print(mean_d)

greater_than_mean <- which(d>mean(d)) # which nodes are having degree value greater than mean degree
print(length(greater_than_mean))
# plot(greater_than_mean)

print(transitivity(g1)) # average clustering coefficient (CC) of the graph

cc_less_avg <- which(transitivity(g1, type="local")<transitivity(g1)) # which nodes are having clustering coefficient less than average CC
print(length(cc_less_avg))

plot(degree_distribution(g1, cumulative = FALSE),xlab="degree",ylab="Frequency") #plot network degree distribution

count_components(g1, mode ="strong")

print(length(g1))

vcount(g1)
print(edge_density(g1))

max_nodes = which(d==min_d)
print(max_nodes)

hist(degree(g1),xlab="Degree",ylab="Frequency",main="")














comm = cluster_walktrap(g2)
plot(comm,g2, edge.arrow.size = 0.4, vertex.label.cex = 0.4, vertex.label.color = "black", layout=layout.fruchterman.reingold)



