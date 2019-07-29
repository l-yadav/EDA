#Latika Yadav
#ANLY 506-52- B-2019/Summer - Exploratory Data Analytics
#Code Portfolio - Week 9 - Clustering practice


library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(dendextend) # for comparing two dendrograms used in HC algorithm

#load data
df <- USArrests
#remove missing values
df <- na.omit(df)
#to have clustering not dependent on any variable use scaling 
df <- scale(df)
head(df)

#Clustering Distance Measures

#get_dist: for computing a distance matrix between the rows of a data matrix. 
#The default distance computed is the Euclidean; however, get_dist also supports other distances
#fviz_dist: for visualizing a distance matrix
distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#K-MEANS######

####Note***The basic idea behind k-means clustering consists of defining clusters so that the total intra-cluster variation (known as total within-cluster variation) is minimized.
#reccomended approach: 2 clusters and 25 configs
k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)
k2
#visualize these clusters using fviz_cluster() used before
fviz_cluster(k2, data = df)

#using standard pairwise scatter plots to generate similar view
df %>%
  as_tibble() %>% #converting to tibble for easy use and mutation
  mutate(cluster = k2$cluster,
         state = row.names(USArrests)) %>% #adding cluster info
  ggplot(aes(UrbanPop, Murder, color = factor(cluster), label = state)) + 
  geom_text() #plotting clusters, shows with colors


#compare to differnt cluster values instead of 2 used before

k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

#plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

#heat-maps of K-mean solutions with K=4
par(mfrow = c(1, 2))
image(t(df)[, nrow(df):1], yaxt = "n", main = "Original Data")
image(t(df)[, order(k4$cluster)], yaxt = "n", main = "Clustered Data")

#once we find the clusters how do we determine Optimal Clusters?
#1. Elbow method
set.seed(123)
fviz_nbclust(df, kmeans, method = "wss")
#2. Silhouette method
fviz_nbclust(df, kmeans, method = "silhouette")
#3. Gap statistic
#first we need to compute the gap statistic
set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
#Print the result
print(gap_stat, method = "firstmax")
#use the fviz function
fviz_gap_stat(gap_stat)


#slecting to use 4 as optimal clusters, computing k-means clustering with k = 4 generating sizes 13, 16, 13, 8
set.seed(123)
final <- kmeans(df, 4, nstart = 25)
print(final)
fviz_cluster(final, data = df)

#conclude with some descriptive stats on final cluster
USArrests %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")


#### Note*** One potential disadvantage of K-means clustering is that it requires us to pre-specify the number of clusters. Hierarchical clustering is an alternative approach which does not require that we commit to a particular choice of clusters. 

####Hierarchical Clustering Algorithms######

#1. Agglomerative Hierarchical Clustering
#creating a Dissimilarity matrix
d <- dist(df, method = "euclidean")
#performing HC using Complete Linkage, hclust()
hc1 <- hclust(d, method = "complete" )
#plotting the dendrogram
plot(hc1, cex = 0.6, hang = -1)

#using agnes function- get the agglomerative coefficient 
###Note*** agglomerative coefficient: measures the amount of clustering structure found (values closer to 1 suggest strong clustering structure)
#performing HC using Complete Linkage, agnes()
hc2 <- agnes(df, method = "complete")
#refer to the Agglomerative coefficient in hc2 df
hc2$ac
#plotting the dendrogram
pltree(hc2, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

#performing 4 different methods to compare - "average", "single", "complete", "ward"
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
# function to compute coefficient
ac <- function(x) {agnes(df, method = x)$ac}
map_dbl(m, ac)

#2. Divisive Hierarchical Clustering
#performing divisive hierarchical clustering using diana()
hc4 <- diana(df)
# Divise coefficient similar to agglomerative coefficient; amount of clustering structure found
hc4$dc
##plotting the dendrogram
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of diana")

### Note****The height of the cut to the dendrogram = number of clusters obtained; same as the k in k-means clustering

#using Ward's method performing Agglomerative Hierarchical Clustering
hc5 <- hclust(d, method = "ward.D2" )
plot(hc5, cex = 0.6, hang = -1)
#using cutree() to cut dendogram into 4 groups
sub_grp <- cutree(hc5, k = 4)
#Number of members in each cluster
table(sub_grp)

#mark the cluster borders showing cuts
plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 4, border = 2:5)

#visualize HC using fviz() used in K-means earlier
fviz_cluster(list(data = df, cluster = sub_grp))

#Comparing 2 dendograms by plotting them side by side
#as always first compute distance
res.dist <- dist(df, method = "euclidean")
#perform 2 hierarchical clusterings - using complete and ward menthods here
hc1 <- hclust(res.dist, method = "complete")
hc2 <- hclust(res.dist, method = "ward.D2")
# plotting the two dendrograms
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)
#use tanglegram to plots two dendrograms (side by side). here the labels are connected by lines
tanglegram(dend1, dend2) #labels which are not present in the other tree are highlighted with dashed lines

####Note**** Entanglement is a measure between 1 (full entanglement) and 0 (no entanglement). A lower entanglement coefficient corresponds to a good alignment. 

#once we find the clusters how do we determine Optimal Clusters? FUN = hcut instead of kmeans
#1. Elbow method
set.seed(123)
fviz_nbclust(df, FUN = hcut, method = "wss")
#2. Silhouette method
fviz_nbclust(df, FUN = hcut, method = "silhouette")
#3. Gap statistic
#first we need to compute the gap statistic
set.seed(123)
gap_stat <- clusGap(df, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

  

