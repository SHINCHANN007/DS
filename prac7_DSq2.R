# Read data 
data("iris") 
names(iris) 

# To perform heirarchical clustering 
clusters <- hclust(dist(iris[,3:4])) 
# To visualize the heirarchical clustering 
# To plot the dendrogram 
plot(clusters) 

clusterCut <- cutree(clusters, 3) 
table(clusterCut, iris$Species) 

library(ggplot2) 

# To create a scatter plot of Petal.Length vs Petal.Width 
ggplot(iris, aes(Petal.Length, Petal.Width, color=iris$Species)) + 
  geom_point(alpha=0.4, size=3.5) + geom_point(col=clusterCut) + 
  scale_color_manual(values=c('black','red','green')) 

# Creating clusters using average method 
clusters <- hclust(dist(iris[,3:4]), method='average') 
clusterCut1 <-cutree(clusters,3) 
table(clusterCut1, iris$Species) 

plot(clusters) 

ggplot(iris, aes(Petal.Length, Petal.Width, color=iris$Species)) + 
  geom_point(alpha=0.4, size=3.5) + 
  geom_point(col=clusterCut1) + 
  scale_color_manual(values=c('black','red','green'))