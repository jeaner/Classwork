# data is from here:
# https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+%28Original%29

# set our random seed so results are more reproducible
set.seed(42)
library(data.table)
bc.dt <- fread('~/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week8/breast-cancer-wisconsin.data')
summary(bc.dt)
# convert the target column (last column)
# to a factor (2 is benign, or no cancer, 4 is cancer)
bc.dt[, V11:=as.factor(V11)]
head(bc.dt)
# replace the '?' with NA
bc.dt[bc.dt == '?', ] <- NA
summary(bc.dt)
str(bc.dt)
# V7 is the only one with missing values, convert it to an int datatype
bc.dt[, V7:=as.integer(V7)]
# drop the V1 column because it is just an ID
bc.dt[, V1:=NULL]
summary(bc.dt)

# fill in NAs with KNN
library(DMwR)
bc.dt <- knnImputation(bc.dt)
summary(bc.dt)

# create 2 models and compare their performance on a test set
library(caret)
tr.idxs <- createDataPartition(bc.dt$V11, p = 0.8)$Resample1
train <- bc.dt[tr.idxs]
test <- bc.dt[-tr.idxs]
rf <- train(V11 ~ .,
            data = train,
            method = 'rf',
            trControl = trainControl(method = 'cv', number = 3),
            tuneGrid = expand.grid(mtry = c(3, 5, 7)))

rf
svm <- train(V11 ~ .,
             data = train,
             method = 'svmRadial',
             trControl = trainControl(method = 'cv', number = 3),
             tuneGrid = expand.grid(C = c(0.1, 1, 10), sigma = c(0.001, 0.01, 0.1)))
svm

rf.preds <- predict(rf, test)
svm.preds <- predict(svm, test)
confusionMatrix(rf.preds, test$V11, positive = '4')
confusionMatrix(svm.preds, test$V11, positive = '4')
"""
The svm performed slightly better, but only by having one fewer false negative.  
Both accuracies were right around 96%, beating the no information rate of 
65% by quite a lot.  The sensitivity of the svm was slightly better than
the random forest (0.96 compared with 0.94).
"""