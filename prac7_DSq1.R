# Read data 
data("iris") 
names(iris) 
new_data <- subset(iris, select=c(-Species)) 
head(new_data) 
cl <- kmeans(new_data, 3) 
cl 
  
  data <- new_data 
  # Calculate the total within-cluster sum of squares (WSS) for different values of k 
  wss <- sapply(1:15, function(k){kmeans(data,k)$tot.withinss}) 
  wss 
  
  # To visualize how WSS changes with dfferent vlaues of kkk and find the "elbow"  
  #(the optimal number of cluster) 
  plot(1:15, wss, type="b", pch=19, frame=FALSE,xlab="Number of clusters K",ylab="Total within-clusters sum of squares") 
  
  library(cluster) 
  clusplot(new_data, cl$cluster,  color=TRUE, shade=TRUE, labels=2, lines=0)
  
  # To display the cluster assignments for each data point in the new_data dataset 
  cl$cluster 
  
  # To disply the centroid of the clusters 
  cl$centers 