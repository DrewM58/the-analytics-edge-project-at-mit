# VISUALIZING NETWORK DATA
setwd("/Users/huihuiduan/1_MOOC_of_Coursera_edX_Udacity/22 MITx 15.071x The Analytics Edge/Week 8 Visualization")
# PROBLEM 1.1 - SUMMARIZING THE DATA
edges <- read.csv("edges.csv")
users <- read.csv("users.csv")
str(edges)
str(users)
table(edges$V1)
table(edges$V2)
( sum(table(edges$V1)) + sum(table(edges$V2)) ) / 59

# PROBLEM 1.2 - SUMMARIZING THE DATA
str(users)
table(users$school, users$locale)

# PROBLEM 1.3 - SUMMARIZING THE DATA
table(users$gender, users$school)

# PROBLEM 2.1 - CREATING A NETWORK
install.packages("igraph")
library(igraph)
?graph.data.frame
g = graph.data.frame(d = edges, directed = FALSE, vertices = users)

# PROBLEM 2.2 - CREATING A NETWORK
plot(g, vertex.size=5, vertex.label=NA)

# PROBLEM 2.3 - CREATING A NETWORK
d <- degree(g)
str(d)
sum(d >= 10)

# PROBLEM 2.4 - CREATING A NETWORK
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)

max(V(g)$size)
min(V(g)$size)

# PROBLEM 3.1 - COLORING VERTICES
V(g)$color = "black"

V(g)$color[V(g)$gender == "A"] = "red"

V(g)$color[V(g)$gender == "B"] = "gray"

plot(g, vertex.label=NA)

# PROBLEM 3.2 - COLORING VERTICES
table(users$school)
V(g)$color = "black"

V(g)$color[V(g)$school == "A"] = "red"

V(g)$color[V(g)$school == "AB"] = "gray"

plot(g, vertex.label=NA)

# PROBLEM 3.3 - COLORING VERTICES
table(users$locale)
V(g)$color = "black"

V(g)$color[V(g)$locale == "A"] = "red"

V(g)$color[V(g)$locale == "B"] = "gray"

plot(g, vertex.label=NA)

# PROBLEM 4 - OTHER PLOTTING OPTIONS
?igraph.plotting

plot(g,  vertex.label = NA, edge.width = 1)













