# DOCUMENT CLUSTERING WITH DAILY KOS

# PROBLEM 1.1 - HIERARCHICAL CLUSTERING

dailykos <- read.csv("dailykos.csv")
str(dailykos)

# Compute distances
distances = dist(dailykos[2:1546], method = "euclidean")
# answer: 1, 2 choice

# Hierarchical clustering
clusterDailykos = hclust(distances, method = "ward") 


# PROBLEM 1.2 - HIERARCHICAL CLUSTERING
# Plot the dendrogram
plot(clusterDailykos)
# answer: 1, 2 choice

# PROBLEM 1.3 - HIERARCHICAL CLUSTERING
# ni mei de: 7 or 8

# PROBLEM 1.4 - HIERARCHICAL CLUSTERING
# Assign points to clusters
clusterGroups = cutree(clusterDailykos, k = 7)
# Create a new data set with just the movies from cluster 2
cluster1 = subset(dailykos, clusterGroups == 1)
cluster2 = subset(dailykos, clusterGroups == 2)
cluster3 = subset(dailykos, clusterGroups == 3)
cluster4 = subset(dailykos, clusterGroups == 4)
cluster5 = subset(dailykos, clusterGroups == 5)
cluster6 = subset(dailykos, clusterGroups == 6)
cluster7 = subset(dailykos, clusterGroups == 7)

dim(cluster1)
dim(cluster2)
dim(cluster3)
dim(cluster4)
dim(cluster5)
dim(cluster6)
dim(cluster7)


# PROBLEM 1.5 - HIERARCHICAL CLUSTERING
tail(sort(colMeans(cluster1[-1])))


# PROBLEM 1.6 - HIERARCHICAL CLUSTERING
tail(sort(colMeans(cluster1[-1])))
tail(sort(colMeans(cluster2[-1])))
tail(sort(colMeans(cluster3[-1])))
tail(sort(colMeans(cluster4[-1])))
tail(sort(colMeans(cluster5[-1])))
tail(sort(colMeans(cluster6[-1])))
tail(sort(colMeans(cluster7[-1])))



# PROBLEM 2.1 - K-MEANS CLUSTERING
# Run k-means

set.seed(1000)
KMC = kmeans(dailykos[2:1546], centers = 7)
subset1 <- subset(dailykos, KMC$cluster == 1)
subset2 <- subset(dailykos, KMC$cluster == 2)
subset3 <- subset(dailykos, KMC$cluster == 3)
subset4 <- subset(dailykos, KMC$cluster == 4)
subset5 <- subset(dailykos, KMC$cluster == 5)
subset6 <- subset(dailykos, KMC$cluster == 6)
subset7 <- subset(dailykos, KMC$cluster == 7)

table(KMC$cluster)

dim(subset1)
dim(subset2)
dim(subset3)
dim(subset4)
dim(subset5)
dim(subset6)
dim(subset7)

# KmeansCluster = split(dailykos, KMC$cluster)
# summary(KmeansCluster)


# PROBLEM 2.2 - K-MEANS CLUSTERING
tail(sort(colMeans(subset1[-1])))
tail(sort(colMeans(subset2[-1])))
tail(sort(colMeans(subset3[-1])))
tail(sort(colMeans(subset4[-1])))
tail(sort(colMeans(subset5[-1])))
tail(sort(colMeans(subset6[-1])))
tail(sort(colMeans(subset7[-1])))


# PROBLEM 2.3 - K-MEANS CLUSTERING
# PROBLEM 2.4 - K-MEANS CLUSTERING
# PROBLEM 2.5 - K-MEANS CLUSTERING
# PROBLEM 2.6 - K-MEANS CLUSTERING
table(clusterGroups, KMC$cluster)















