# K-Means Clustering

# Importing the dataset
library(tidyverse)
census <- read_csv('census2000.csv', na=c(".", "NA", "", "?"))
print(census)

census <- census %>% mutate(MedHHInc=parse_number(MedHHInc))
print(census)


# Exploring the dataset
library(Amelia)
missmap(census)
dev.off()  # reset graphic layout

library(dlookr)
plot_hist_numeric(census)


# Data Filtering
census.fltr <- census %>% filter(MeanHHSz > 0)
missmap(census.fltr)
dev.off() 


# Variable Standardization 
census.scale<-census.fltr
census.scale <- census.scale %>% select(c(RegDens, MedHHInc, MeanHHSz)) %>% scale




# Using the elbow method to find the optimal number of clusters
ncluster<-20
wcss <- numeric()
for (i in 1:ncluster) {
  set.seed(1234)
  wcss[i] <- kmeans(census.scale, i, iter.max = 100)$tot.withinss
}

plot(1:ncluster, wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')


# Fitting K-Means to the dataset
set.seed(1234)
km <- kmeans(census.scale, centers = 5, nstart=25) # Have 'nstart' sets of starting and ending centers.
                                                   # Pick the center set that have the lowest distance 
                                                   # from the data points to the centroids.

table(km$cluster)  # display cluster size


# Visualising the clusters
library(factoextra)
fviz_cluster(km, data = census.scale)


# Profiling
census.fltr <- census.fltr %>% mutate(cluster=as.factor(km$cluster))


# Side-by-side density plots by cluster
census.fltr%>%
  ggplot(aes(x=RegDens)) +  # specify var. of interest
  geom_density(lty=3) +      # add overall density curve
  geom_density(aes(fill=cluster, colour=cluster), alpha=0.5) # add density curves stratified by cluster


census.fltr%>%
  ggplot(aes(x=MedHHInc)) +  
  geom_density(lty=3) +     
  geom_density(aes(fill=cluster, colour=cluster), alpha=0.5) 


census.fltr%>%
  ggplot(aes(x=MeanHHSz)) +  
  geom_density(lty=3) +      
  geom_density(aes(fill=cluster, colour=cluster), alpha=0.5) 


# Side-by-side Boxplot of RegDens by cluster
census.fltr %>% 
  ggplot(aes(y=RegDens)) +
  geom_boxplot(notch=TRUE) +
  geom_boxplot(aes(x=cluster, fill=cluster, colour=cluster), notch=TRUE)





