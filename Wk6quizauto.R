library(data.table)

filename <- 'C:/MSDS/machl/wk7/auto.dt.nona.csv'
auto.dt <- fread(filename)
str(auto.dt)
#origin should be a factor because it is not ORDERED!


hist(auto.dt$mpg)
summary(auto.dt)
set.seed(223)
wss <- c()
clust.nums <- 2:20
for (i in clust.nums) {
  km <- kmeans(auto.dt, centers = i)
  wss <- c(wss, km$tot.withinss)
}
plot(clust.nums, wss)
# I'd say around 6 is the change

library(caret)
# centers and scales data
scaled.dt <- predict(preProcess(auto.dt), auto.dt)
hist(scaled.dt$displacement)

wss <- c()
clust.nums <- 2:30
for (i in clust.nums) {
  km <- kmeans(scaled.dt, centers = i)
  wss <- c(wss, km$tot.withinss)
}

plot(clust.nums, wss)


km <- kmeans(scaled.dt, centers = 6)

km <- kmeans(auto.dt, center =6)
library(ggplot2)  
#jitter might not be necessary because the data is spread 
qplot(jitter(auto.dt$displacement, 2),
      jitter(auto.dt$mpg, 2),
      colour = as.factor(km$cluster),
      alpha=I(0.5))

hc.complete <- hclust(dist(auto.dt), method="complete")
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex =.9)
cut <- cutree(hc.complete, 6)
cut
qplot(jitter(auto.dt$mpg, 2),
      jitter(auto.dt$displacement, 2),
      colour = as.factor(cut),
      alpha=I(0.5))

?hclust

hc.wardd2 <- hclust(dist(auto.dt), method="ward.D2")
plot(hc.wardd2,main="ward.d2 Linkage", xlab="", sub="", cex =.9)
cut <- cutree(hc.wardd2, 6)
cut
library(cluster)
sil<- silhouette(cut, dist(auto.dt))
summary(sil)
qplot(jitter(auto.dt$mpg, 2),
      jitter(auto.dt$displacement, 2),
      colour = as.factor(cut),
      alpha=I(0.5))
sil<- silhouette(cut, dist(auto.dt))


hc.sgl <- hclust(dist(auto.dt), method="single")
plot(hc.sgl,main="single Linkage", xlab="", sub="", cex =.9)
cut <- cutree(hc.sgl, 6)
cut
sil<- silhouette(cut, dist(auto.dt))
summary(sil)

qplot(jitter(auto.dt$mpg, 2),
      jitter(auto.dt$displacement, 2),
      colour = as.factor(cut),
      alpha=I(0.5))
sil<- silhouette(cut, dist(auto.dt))

hc.avg <- hclust(dist(auto.dt), method="average")
plot(hc.avg,main="avg Linkage", xlab="", sub="", cex =.9)
cut <- cutree(hc.avg, 6)
cut
sil<- silhouette(cut, dist(auto.dt))
summary(sil)
#0.5061672 0.5703728 0.4221829 0.4528025 0.5929152 0.9181102

qplot(jitter(auto.dt$mpg, 2),
      jitter(auto.dt$displacement, 2),
      colour = as.factor(cut),
      alpha=I(0.5))
sil<- silhouette(cut, dist(auto.dt))
?hclust
hc.ward <- hclust(dist(auto.dt), method="ward.D")
plot(hc.ward,main="ward Linkage", xlab="", sub="", cex =.9)
cut <- cutree(hc.ward, 6)
cut
sil<- silhouette(cut, dist(auto.dt))
summary(sil)
#0.3404610 0.6391038 0.5488831 0.5440690 0.3912098 0.591951

qplot(jitter(auto.dt$mpg, 2),
      jitter(auto.dt$displacement, 2),
      colour = as.factor(cut),
      alpha=I(0.5))
hc.ward <- hclust(dist(auto.dt), method="ward.D")
plot(hc.ward,main="ward Linkage", xlab="", sub="", cex =.9)
cut <- cutree(hc.ward, 4)
cut
sil<- silhouette(cut, dist(auto.dt))
summary(sil)
#0.3116898 0.6781334 0.6348000 0.6082748 


#single method looks like the worst method to use