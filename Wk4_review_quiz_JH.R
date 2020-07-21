library(data.table)
library(caret)
library(parallel)
library(rpart)
library(dplyr)  # so we can use the pipe operator (>%>)
library(ggplot2)
library(xgboost)

filename <- 'C:/MSDS/MachL/Wk5/auto.dt.clean.csv'

auto.dt <- fread(filename)
str(auto.dt)
auto.dt[, origin :=as.factor(origin)]
set.seed(223)

train.idxs <- createDataPartition(auto.dt$mpg, p = 0.8)$Resample1
train <- auto.dt[train.idxs]
test <- auto.dt[-train.idxs]
dim(auto.dt)
sqrt(7)
#choose mtry below 7
trControl <- trainControl(method = 'repeatedcv',
                          number = 3,
                          repeats = 4)

rf.model <- train(mpg ~ .,
                  data = train,
                  method = 'rf',
                  trControl = trControl, #defined above
                  ntree = 250,  # makes it run a bit faster
                  tuneGrid = expand.grid(mtry = c(1, 2, 3, 4, 7)))
rf.model



#crossvalidation

tr.preds <- predict(rf.model, train)
postResample(tr.preds, train$mpg)

te.preds <- predict(rf.model, test)
postResample(te.preds, test$mpg)

#R-sq values for the 
#train=.98
#test=.91
#Train >test so it is a bit overfit, we want them to be almost equal

varImpPlot(rf.model$finalModel, scale = F)

#test some more

rf.model2 <- train(mpg ~ .,
                   data = train,
                   method = 'rf',
                   trControl = trControl, 
                   ntree = 250, 
                   nodesize= 14,
                   maxnodes= 50,
                   tuneGrid = expand.grid(mtry = c(1, 2, 3, 4, 7)))


rf.model2
tr.preds2 <- predict(rf.model2, train)
postResample(tr.preds2, train$mpg)

te.preds2 <- predict(rf.model2, test)
postResample(te.preds2, test$mpg)

#we get them closer with .94 vs .91