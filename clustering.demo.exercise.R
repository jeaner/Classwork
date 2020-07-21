library(data.table)

# the G1/G2 columns, which are period 1/2 grades,
# are strings, but should be numeric.
# kind of a complex way of loading the data, but it works
filename <- 'C:/MSDS/machl/wk6/student-mat.csv'
math.dt <- fread(filename)
str(math.dt)
G1.data <- math.dt[, as.numeric(G1)]
G2.data <- math.dt[, as.numeric(G2)]

hist(G1.data)
max(G1.data)
min(G1.data)

max(G2.data)
min(G2.data)
#load g1 g2 as numeric and not factors
math.dt <- fread(filename, stringsAsFactors = T)
math.dt[, G1:=G1.data]
math.dt[, G2:=G2.data]
str(math.dt)

# create dummy variables so we can calculate distances
# for factor variables
library(fastDummies)
dummy.dt <- as.data.table(dummy_cols(math.dt))
factor.columns <- colnames(Filter(is.factor, dummy.dt))
dummy.dt[, (factor.columns):=NULL] #get rid of the factors
str(dummy.dt)


wss <- c()
clust.nums <- 2:20
for (i in clust.nums) {
  km <- kmeans(dummy.dt, centers = i)
  wss <- c(wss, km$tot.withinss)
}
plot(clust.nums, wss)
# I'd say around 6 is good.

library(caret)
# centers and scales data
scaled.dt <- predict(preProcess(dummy.dt), dummy.dt)
hist(scaled.dt$G1)

wss <- c()
clust.nums <- 2:30
for (i in clust.nums) {
  km <- kmeans(scaled.dt, centers = i)
  wss <- c(wss, km$tot.withinss)
}
#wss scree plot y axis

plot(clust.nums, wss)


km <- kmeans(scaled.dt, centers = 5)
#scree plot looks like a hill, not very helpful

# curse of dimensionality!
# to combat we could get rid of all factor columns
# this ends up being a bit of a chore, especially to do
# quickly/easily

# We will stick with the original unscaled data, and 
# look at the clusters with the grades plotted
set.seed(42)
km <- kmeans(dummy.dt, center =6)
library(ggplot2)     
qplot(jitter(math.dt$G1, 2),
      jitter(math.dt$G2, 2),
      colour = as.factor(km$cluster),
      alpha=I(0.5))
# next step in exploration might be to look at stats of 
# other features for the clusters

# could also do PCA and plot 2d/3d with top few PCA dimensions


# HCA
hc.complete <- hclust(dist(dummy.dt), method="complete")
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex =.9)
cut <- cutree(hc.complete, 6)
cut
qplot(jitter(math.dt$G1, 2),
      jitter(math.dt$G2, 2),
      colour = as.factor(cut),
      alpha=I(0.5))

# pair exercise -- try other linkages (average, single)
# and compare.
#average, single, werd.D
?hclust

hc.wardd2 <- hclust(dist(dummy.dt), method="ward.D2")
plot(hc.wardd2,main="ward.d2 Linkage", xlab="", sub="", cex =.9)
cut <- cutree(hc.wardd2, 6)
cut
#library(cluster)
sil<- silhouette(cut, dist(dummy.dt))
summary(sil)
qplot(jitter(math.dt$G1, 2),
      jitter(math.dt$G2, 2),
      colour = as.factor(cut),
      alpha=I(0.5))
sil<- silhouette(cut, dist(dummy.dt))
?silhouette

hc.sgl <- hclust(dist(dummy.dt), method="single")
plot(hc.sgl,main="single Linkage", xlab="", sub="", cex =.9)
cut <- cutree(hc.sgl, 6)
cut
sil<- silhouette(cut, dist(dummy.dt))
summary(sil)

qplot(jitter(math.dt$G1, 2),
      jitter(math.dt$G2, 2),
      colour = as.factor(cut),
      alpha=I(0.5))
sil<- silhouette(cut, dist(dummy.dt))

hc.avg <- hclust(dist(dummy.dt), method="average")
plot(hc.avg,main="avg Linkage", xlab="", sub="", cex =.9)
cut <- cutree(hc.avg, 6)
cut
sil<- silhouette(cut, dist(dummy.dt))
summary(sil)

qplot(jitter(math.dt$G1, 2),
      jitter(math.dt$G2, 2),
      colour = as.factor(cut),
      alpha=I(0.5))
sil<- silhouette(cut, dist(dummy.dt))

hc.ward <- hclust(dist(dummy.dt), method="ward.d")
plot(hc.ward,main="ward Linkage", xlab="", sub="", cex =.9)
cut <- cutree(hc.ward, 6)
cut
sil<- silhouette(cut, dist(dummy.dt))
summary(sil)
plot(sil)
qplot(jitter(math.dt$G1, 2),
      jitter(math.dt$G2, 2),
      colour = as.factor(cut),
      alpha=I(0.5))
hc.ward <- hclust(dist(dummy.dt), method="ward.d")
plot(hc.ward,main="ward Linkage", xlab="", sub="", cex =.9)
cut <- cutree(hc.ward, 4)
cut
sil<- silhouette(cut, dist(dummy.dt))
summary(sil)




?hclust
str(hc.ward)
library(fpc)
?cluster.stats
library(mclust)
fit <- Mclust(dummy.dt)
plot(fit)
# use the clusters from HCA and kmeans to explore the data more
# calculate summary stats for some of the groups and compare

# if you want/have time, use PCA to plot clusters