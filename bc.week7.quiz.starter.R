# data is from here:
# https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+%28Original%29
# anywhere with three underscores (___) are spots for you to fill in


# set our random seed so results are more reproducible
___
library(___)  # library to read in the data into a data table
bc.dt <- fread('~/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week8/breast-cancer-wisconsin.data')
summary(bc.dt)
# convert the target column (last column)
# to a factor (2 is benign, or no cancer, 4 is cancer)
bc.dt[, V11:=___]
head(bc.dt)
# replace the '?' with NA
bc.dt[bc.dt == ___, ] <- ___
summary(bc.dt)
str(bc.dt)
# V7 is the only one with missing values, convert it to an int datatype
bc.dt[, ___]
# drop the V1 column because it is just an ID
bc.dt[, V1:=___]
summary(bc.dt)

# fill in NAs with KNN
library(DMwR)
bc.dt <- knnImputation(bc.dt)
summary(bc.dt)

# create 2 models and compare their performance on a test set
# I did random forest and svm, but feel free to try anything here
library(caret)
tr.idxs <- ___
train <- bc.dt[tr.idxs]
test <- bc.dt[-tr.idxs]

"""
Write a small summary of your results here.
"""