# https://topepo.github.io/caret/train-models-by-tag.html#Bayesian_Model
# search for 'bayes' to find models using Bayes' Law
library(data.table)
filename <- 'C:/MSDS/MachL/Wk2/heart.disease.data.clean.csv'
dt <- fread(filename)
str(dt)
sumdt[age>0, age:=as.factor(levels(4))]
str(dt)
dt[num > 0, num:=1] #binary classifications
dt[, num:=as.factor(num)] #factor to work with learning algorithms

str(dt)

library(caret)
set.seed(42)
train.indices <- createDataPartition(dt$num, p = 0.8)$Resample1 #80%training 
train <- dt[train.indices]
test <- dt[-train.indices]
#Crossvalidation in 3 groups repeating it twice (how many times its repeating the cv)
trControl <- trainControl(method  = "repeatedcv",
                          number  = 3,
                          repeats = 2)
mean(as.numeric(dt[age<60]$num))-1
#39% chance of having heart disease over age 60

# how to see what parameters are available for a model:
# search for model here
# https://topepo.github.io/caret/available-models.html
# then look up documentation for function from that package on rdocumentation.org
library(klaR)
library(e1071)
#nb is # naive bayes from klaR package
fit2 <- train(num ~ .,
             method     = 'nb', 
             tuneGrid   = data.frame("fL"=c(0,1), "usekernel"=c(T, F), "adjust"=1),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = train)
#warnings the age has so many unique values that there are warnings
fit2
#accuracy  and coehen's kappa scoring metric
#fL smoothing factor
fit <- train(num ~ .,
             method     = 'nb', 
             tuneGrid   = data.frame("fL"=0, "usekernel"=c(T, F), "adjust"=1),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = train)
fit

# get scores from predictions
postResample(predict(fit, train), train$num)
#for knn it will give more metrics, for naieve bayes we get accuracy and kappa
postResample(predict(fit, test), test$num)
confusionMatrix(predict(fit, train), train$num)
