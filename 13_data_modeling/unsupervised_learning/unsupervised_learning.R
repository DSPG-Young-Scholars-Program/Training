# Unsupervised Learning Training 7/17/19
# Examples from https://campus.datacamp.com/courses/unsupervised-learning-in-r

# ----------------------------------
# k-means Clustering
# ----------------------------------

head(iris)
iris.scaled <- scale(iris[,-5])
km.out <- kmeans(iris.scaled, centers=2, nstart=20)

km.out$cluster
km.out

# scatterplot of two dimensions
plot(iris[,c(1,2)], col=km.out$cluster, main="k-means with 2 clusters",
     xlab="",ylab="")

# initialize the total within sum of squares error: wss
wss <- 0

# choosing the number of clusters; hard problem!
for(i in 1:8){
  km.out <- kmeans(iris.scaled, centers=i, nstart=20)
  wss[i] <- km.out$tot.withinss
}

# heuristic: look for an 'elbow' in within cluster sum of squares;
# model no longer improves much as complexity increases
plot(1:8, wss, type="b",
     xlab="Number of Clusters",
     ylab="Within cluster sum of squares")

km.out <- kmeans(iris[,-5], centers=3, nstart=20)
plot(iris[,c(1,2)], col=km.out$cluster, main="k-means with 3 clusters",
     xlab="",ylab="")

# compare cluster results vs species
table(km.out$cluster, iris$Species)


# ----------------------------------
#  Hierarchical Clustering
# ----------------------------------

hclust.out <- hclust(d = dist(iris.scaled), method="complete")

# dendrogram
plot(hclust.out)

# choose 3 clusters
cutree(hclust.out, k = 3)

# alternately choose clusters by cutting the height of tree
abline(h=5, col="red")
cutree(hclust.out, h = 5)

plot(iris[,c(1,2)], col=cutree(hclust.out, h = 5),
     main="Hierarchical clustering",
     xlab="",ylab="")

table(cutree(hclust.out, h = 5), iris$Species)


# ----------------------------------
# Principal Components Analysis
# ----------------------------------

pr.iris <- prcomp(x = iris[,-5], scale=TRUE, center=TRUE)
summary(pr.iris)

pr.iris$rotation

# biplot
biplot(pr.iris)

# scree plot
pr.var <- pr.iris$sdev^2
pve <- pr.var / sum(pr.var)
par(mfrow=c(1,2))
plot(pve, xlab = "Principal Component",
     ylab="Proportion of Variance Explained", ylim=c(0,1), type="b")
plot(cumsum(pve), xlab = "Principal Component",
     ylab="Cumulative Variance Explained", ylim=c(0,1), type="b")


# ----------------------------------
# Illustrating problems with scaling
# ----------------------------------

head(mtcars)

biplot( prcomp(x=mtcars, scale=FALSE) )

biplot( prcomp(x=mtcars, scale=TRUE) )


