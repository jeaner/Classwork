# data is from here:
# https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+%28Original%29
# anywhere with three underscores (___) are spots for you to fill in


# set our random seed so results are more reproducible
set.seed(223)
library(data.table)  # library to read in the data into a data table
bc.dt <- fread('C:/MSDS/MachL/wk8/breast-cancer-wisconsin.data')
summary(bc.dt)
# convert the target column (last column)
# to a factor (2 is benign, or no cancer, 4 is cancer)

#v11 2 is no cancer and 4 means cancer
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
# I did random forest and svm, but feel free to try anything here
library(caret)
tr.idxs <- createDataPartition(bc.dt$V11, p = 0.8)$Resample1
train <- bc.dt[tr.idxs]
test <- bc.dt[-tr.idxs]

#"""
#Write a small summary of your results here.
#"""

dim(train)
library(caret)
rf.model <- train(V11 ~ .,
                  method = 'rf',
                  data = train,
                  trControl = trainControl(method = 'repeatedcv', number = 3, repeats = 3),
                  tuneGrid = expand.grid(mtry = c(4, 6, 8, 10, 12)),
                  ntree = 500)

rf.model

library(pROC)
#make numeric targets, converting to factors, 2 and 1 so much subtract
rf.preds <- predict(rf.model, test)
head(rf.preds)
confusionMatrix(rf.preds, test$V11, positive = '4')

knn.model <- train(V11 ~ .,
                   method = 'knn',
                   data = train,
                   trControl = trainControl(method = 'repeatedcv', number = 3, repeats = 3))
knn.model

knn.preds<-predict(knn.model, test)
confusionMatrix(knn.preds, test$V11, positive = '4')


#svm had 4 false with an accuracy of .9712
#rf had 4 false with an accruacy of .9712

#They both have equal accuracy so it does not matter which we use