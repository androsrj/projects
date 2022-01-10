library(dynamicTreeCut)

################# IRIS DATASET: 150 OBSERVATIONS (Built into Rstudio) #################

# Read in data (already built into R)
iris.dis <- dist(iris[,-ncol(iris)])
iris.truth <- as.numeric(iris[,ncol(iris)]) # True clustering of the data

# Try different linkages
ward <- hclust(iris.dis, method="ward.D2") # Ward linkage
avg <- hclust(iris.dis, method="average") # Average linkage
com <- hclust(iris.dis) # Complete linkage
single <- hclust(iris.dis, method='single') # Single linkage

# Plot dendrograms
plot(ward)
plot(avg)
plot(com)
plot(single)

# Cut linkage trees using the dynamicTreeCut package
est.ward <- cutreeDynamic(ward, distM=as.matrix(iris.dis))
est.avg <- cutreeDynamic(avg, distM=as.matrix(iris.dis))
est.com <- cutreeDynamic(com, distM=as.matrix(iris.dis))
est.single <- cutreeDynamic(single, distM=as.matrix(iris.dis))

# Compare each linkage estimate to the true clustering using the Rand Index 
RI(est.ward, iris.truth)
RI(est.avg, iris.truth)
RI(est.com, iris.truth)
RI(est.single, iris.truth)
# Single and complete linkages provide best estimates of the true clustering
# Note that the single linkage dendrogram is more difficult to interpret