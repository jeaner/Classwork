# this is Siri's example code from FTE

# the 'class' library for KNN only does classification
library(class)
set.seed(1234) # can replicate the results
ind <- sample(2,nrow(iris),replace=TRUE,prob=c(0.67,0.33)) 
iris.training <- iris[ind==1,1:4]
iris.test <- iris[ind==2,1:4]
iris.trainLabels <- iris[ind==1,5] #classification label on training data
iris.testLabels <- iris[ind==2,5]
iris_pred <- knn(iris.training, iris.test, cl=iris.trainLabels, k=3) # use k=3 as nearest neighbour
table(iris.testLabels, iris_pred) #display the confusion matrix

library(gmodels)
CrossTable(x=iris.testLabels,y=iris_pred, prop.chisq = FALSE) # not perform chisq te
