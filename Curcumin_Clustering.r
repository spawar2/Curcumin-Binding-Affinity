# Curcumin clustering for binding affinity, 01/08/2025

# Set working directory
setwd("/Users/pawar/Desktop/Clustering-George")

# Read dataset
data <- read.csv("data.csv", header = TRUE, sep = ",")

# Add first column as row names
Newdata <- data[, which(names(data) != "X")]
Newdata <- t(Newdata)
set.seed(278613)

# K-means clustering
NewdataK3 <- kmeans(x= Newdata, centers=3)

library(useful)
plot(NewdataK3, data= Newdata)

# see the cluster sizes with 1 
set.seed(278613)
NewdataK3N25 <- kmeans(Newdata, centers=3, nstart=25)
NewdataK3$size
NewdataK3N25$size

table(NewdataK3N25$cluster)

# Export cluster data
write.csv(data, "data.csv")

# Hierarchical clustering
NewdataH <- hclust(d=dist(Newdata))
plot(NewdataH)

wineH1 <- hclust(dist(Newdata), method="single") 
wineH2 <- hclust(dist(Newdata), method="complete") 
wineH3 <- hclust(dist(Newdata), method="average") 
wineH4 <- hclust(dist(Newdata), method="centroid") 
par(mfrow=c(2,2))
plot(wineH1, labels=FALSE, main="Single")
plot(wineH2, labels=FALSE, main="Complete")
plot(wineH3, labels=FALSE, main="Average")
plot(wineH4, labels=FALSE, main="Centroid")

plot(NewdataH)
# split into 3 clusters
rect.hclust(NewdataH, h=200, border="red")
rect.hclust(NewdataH, h=800, border="blue")
