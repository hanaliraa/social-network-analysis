# Class Activity - Graph Visualization
# CS 361 / SDP 352 Social Network Analysis
# Habib University
# Shah Jamal / Qasim Pasta

# Objective:  The objective of this activity is to construct a network 
#             from data and use fundamental statistical analysis concepts 
#             with network analysis

# *********************************
# Student Information
# Name: 
# Student Id:
# **********************************

# ********** Activity **************

library(igraph)

# load the data from text file as a table
data = read.table(file = "celeg.txt")

# display initial 6 rows of the data
# notice the column names
head(data)

# change column names 
colnames(data) = c("from","to","weight")
# notice column names now
head(data)

# construct graph from data 
g = graph_from_data_frame(data, directed = F)
# show basic information about gaph (notice properties)
summary(g)

# display graph 
plot(g, vertex.label=NA, vertex.size=8)

# store weights in a vector
w = E(g)$weight
# find index of edges having weights less than average weight
filtered_edges = which(w<mean(w))

# delete edges having weight less than average weight
g2 = delete.edges(g,which(w<mean(w)))
# visualize the network (notice peripheral nodes)
plot(g2, vertex.label=NA, vertex.size=5)

# delete nodes with 0 degree (no connections)
g3 = delete.vertices(g2, which(degree(g2)<1))
# visualize the new network
plot(g3, vertex.label = NA, vertex.size=5))
