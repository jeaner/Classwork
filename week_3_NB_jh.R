#Week 3 N Bayes - Jeanette Henry
set.seed(232)
library(caret)
library(data.table)
library(klaR)
library(e1071)


str(iris)
train.indices <- createDataPartition(iris$Species, p = 0.8)$Resample1
dt<-as.data.table(iris)
train <- dt[train.indices]
test <- dt[-train.indices]
#Crossvalidation in 3 groups repeating it twice (how many times its repeating the cv)
trControl <- trainControl(method  = "repeatedcv",
                          number  = 3,
                          repeats = 2)
igrid<-expand.grid(laplace=c(0,1), usekernel=c(TRUE,FALSE), adjust = 1)



#nb is # naive bayes from klaR package
model1<- train(Species~., 
               data=train, 
               trControl=trControl, 
               method="naive_bayes", 
               tuneGrid=igrid)
model1


fit2 <- train(Species ~ .,
              method     = 'nb', 
              tuneGrid   = igrid,
              trControl  = trControl,
              metric     = "Accuracy",
              data       = train)
#warnings the age has so many unique values that there are warnings
fit2
#accuracy  and coehen's kappa scoring metric
#fL smoothing factor
fit <- train(Species ~ .,
             method     = 'nb', 
             tuneGrid   = data.frame("fL"=0, "usekernel"=c(T, F), "adjust"=1),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = train)
fit

# get scores from predictions
postResample(predict(fit, train), train$Species)
#for knn it will give more metrics, for naieve bayes we get accuracy and kappa
postResample(predict(fit, test), test$Species)
confusionMatrix(predict(fit, train), train$Species)

