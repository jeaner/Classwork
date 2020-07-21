library(data.table)
set.seed(42)
# load data -- already replaced NAs with KNN imputation
fn <- 'C:/MSDS/MachL/Wk2/auto.dt.nona.csv'

auto.dt <- fread(fn)

head(auto.dt)
targs <- auto.dt[, mpg]
# this selects all columns except mpg
features <- auto.dt[, -'mpg', with=F]

library(caret)

# create the train test split -- around 80% of data used for training
length(targs)
trainIdx = createDataPartition(targs, p = 0.8)$Resample1
length(trainIdx)
length(targs)

tr.feats <- features[trainIdx]
tr.targs <- targs[trainIdx]

# another way to get features/targets
te.feats <- auto.dt[-trainIdx, -'mpg']
te.targs <- auto.dt[-trainIdx]$mpg

# a few options for KNN regression, but I went with FNN
library(FNN)
te.preds <- knn.reg(train = tr.feats, test = te.feats, y = tr.targs, k = 3)
te.preds

# see if we can reproduce the results to better understand the algorithm
# get closest 3 points to check algorithm
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
dists <- c()

for (i in 1:length(tr.feats)) {
  dists <- c(dists, euc.dist(tr.feats[i], te.feats[1]))
}
closest <- order(dists)
top3 <- closest[1:3]
top3
tr.feats[top3]
te.feats[1]
tr.targs[top3]
# still don't know how it's getting the knn predictions...
# not clear which distance metric knn.reg is using or if knn.reg is normalizing data
mean(tr.targs[top3])
weighted.mean(tr.targs[top3], dists[top3])

# try normalizing with min/max -1 to 1
scale_range <- function(features, new.min, new.max) {
  new.feats <- copy(features)
  for (i in 1:dim(features)[2]) {
    vect <- features[, i, with=F]
    vect.max <- max(vect)
    vect.min <- min(vect)
    a <- (new.max - new.min) / (vect.max - vect.min)
    b <- new.min - (a * vect.min)
    new.vect <- a * vect + b
    new.feats[, i] <- new.vect
  }
  return(new.feats)
}

# correlation squared is almost the same as r^2
# https://stackoverflow.com/questions/40901445/function-to-calculate-r2-r-squared-in-r
# left this in because I found it, but postResample actually gets r^2
rsq <- function (x, y) cor(x, y) ^ 2

# this will get us some scoring metrics
caret::postResample(te.preds$pred, te.targs)

# have to calculate r^2 by hand in R AFAIK
sse <- sum((te.preds$pred - te.targs) ^ 2)
sst <- sum((te.targs - mean(te.targs)) ^ 2)
1 - sse/sst
rsq(te.preds$pred, te.targs)
# calculated the R2 for the train set to check for overfitting
# looks like its on the border of overfitting
te.preds <- knn.reg(train = tr.feats, test = tr.feats, y = tr.targs, k = 3)
sse <- sum((tr.preds$pred - tr.targs) ^ 2)
sst <- sum((tr.targs - mean(tr.targs)) ^ 2)
1 - sse/sst

sc.tr.feats <- scale_range(tr.feats, -1, 1)
hist(tr.feats$weight)
hist(sc.tr.feats$weight)

te.preds.sc <- knn.reg(train = tr.feats, test = te.feats, y = tr.targs, k = 3)
te.preds.sc
# interestingly, scaling seems to make no difference with performance
# knn.reg() is possibly already scaling the data, but there isn't an option for it
# this can be a disadvantage in R -- too much magic means we don't have as much control
# of our algorithms, and may not understand everything the algos are doing
caret::postResample(te.preds.sc$pred, te.targs)

# if we don't give test data to knn.reg(), it will do cross-validation for us
te.preds <- knn.reg(train = tr.feats, y = tr.targs, k = 3)
te.preds
# loop through 2-20 nearest neighbors and check cross-validation score
neighbors <- seq(2, 20)
r2s <- c()  # r-squared values (coefficient of determination)
sum.square.error <- c()  # sum of squares
for (k in neighbors) {
  preds <- knn.reg(train = tr.feats, y = tr.targs, k = k)
  r2s <- c(r2s, preds$R2Pred)
  sum.square.error <- c(sum.square.error, preds$PRESS)
}

# typically we look for an 'elbow' in the data, where the slope changes abruptly
# it's pretty clearly at 5 here
plot(neighbors, r2s)
plot(neighbors, sum.square.error)
length(r2s)
# diff gets first derivative of points
r2s.1st.deriv <- diff(r2s)
# fill in first point so we have same number as k
r2s.1st.deriv <- c(r2s.1st.deriv[1], r2s.1st.deriv)
# we can also look for when the slop gets close to or goes to 0
# and take the point just before that -- gets us k=5 again
plot(neighbors, r2s.1st.deriv)


best.preds.cv <- knn.reg(train = tr.feats, y = tr.targs, k = 6)
print(best.preds.cv)
best.preds <- knn.reg(train = tr.feats, test = te.feats, y = tr.targs, k = 5)
te.error <- (te.targs - best.preds$pred)

tr.pca.feats <- prcomp(tr.feats)
head(tr.pca.feats)
plot(cumsum(tr.pca.feats$sdev^2 / sum(tr.pca.feats$sdev^2)))

# http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
# this is not a great plot -- should change the labels so they are
# more informative, especially 'tr.targs'
tr.pca.dt <- as.data.table(tr.pca.feats$x)
sp2 <- ggplot(tr.pca.dt, aes(x=PC1, y=PC2, color=tr.targs)) + 
  geom_point() + 
  # Change the low and high colors
  # Sequential color scheme
  scale_color_gradient(low="blue", high="red")
sp2  # shows plot

# plot the residuals of predictions in pca space
te.pca.feats <- predict(tr.pca.feats, newdata = te.feats)
te.pca.dt <- as.data.table(te.pca.feats)
sp2 <- ggplot(te.pca.dt, aes(x=PC1, y=PC2, color=te.targs)) + 
  geom_point() + 
  # Change the low and high colors
  # Sequential color scheme
  scale_color_gradient(low="blue", high="red")
sp2

sp2 <- ggplot(te.pca.dt, aes(x=PC1, y=PC2, color=te.error)) + 
  geom_point() + 
  # Change the low and high colors
  # Sequential color scheme
  scale_color_gradient(low="blue", high="red")
sp2

# options to take it further would be to plot different variables on the x and y,
# or go to 3d with the features or PCA


# another way to do it would be this
# have to provide a dataframe and formula for this one...didn't get that far
# library(kknn)
# res <- kknn(formula = mpg ~ ., rtrain = tr.feats, test = te.feats)


