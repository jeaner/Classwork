library(data.table)
library(caret)
set.seed(223)

filename <- 'C:/MSDS/MachL/Wk4/bank.csv'
bank.dt <- fread(filename, stringsAsFactors = TRUE)
str(bank.dt)
dim(bank.dt) # 16 features + target variable "y"

# target variable is y
# has client subscribed to term deposit?
# binary, yes/no
bank.dt[, y:=as.factor(y)]
setnames(bank.dt, "y", c("target"))
bank.dt[, duration:=NULL]
str(bank.dt)

preprocessParams <- preProcess(bank.dt, method=c("range"))
print(preprocessParams)
norm.dt <- predict(preprocessParams, bank.dt)
summary(norm.dt)
train.index <- createDataPartition(norm.dt$target, p = 0.8)$Resample1
train <- norm.dt[train.index]
test <- norm.dt[-train.index]
####MODEL 1 - UNBALANCED###
library(parallel)
library(caretEnsemble)

control <- trainControl(method="repeatedcv", number=3, repeats=3, savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('lda', 'knn', 'svmRadial', 'LogitBoost')
set.seed(223)
models1 <- caretList(target~., data=train, trControl=control, methodList=algorithmList)
results <- resamples(models1)
summary(results)
dotplot(results)
#All the models have a mean accuracy above 88
# correlation between results
modelCor(results)
#No methods have a correlation about .75 so we do not need to remove any
splom(results)

#Stacking the results with glm General Linearized model
stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
set.seed(223)
stack.glm <- caretStack(models1, method="glm", metric="Accuracy", trControl=stackControl)
print(stack.glm)
#accuracy of 0.8899
glm.preds <- predict(stack.glm, test, type = 'prob')
head(glm.preds)
summary(glm.preds)
summary(as.numeric(test$target))
numeric.targets <- as.numeric(test$target) - 1
summary(numeric.targets)
summary(glm.preds)
#now they are both 0 vs 1

library(pROC)
glm.roc <- roc(numeric.targets, glm.preds)
print(glm.roc$auc)
#AUC 0.7228


plot(glm.roc, col='red')
glm.best.thresh <- coords(roc = glm.roc, x = 'best', ret = 'threshold')
glm.test.preds <- as.numeric(glm.preds >= glm.best.thresh)
confusionMatrix(glm.test.preds, numeric.targets, positive = '1')
#Reference
#Prediction
#   0   1
#0 7685  49
#1 115  55

#Accuracy : 0.8186


####MODEL 2####

#data set is unbalanced with only 1.29% being a positive outcome
#SMOTE to balance the outcomes
library(DMwR)
train_smote<-SMOTE(target~ . , train, perc.over = 300, perc.under = 150)
summary(train_smote)
#now they are more balanced at 1876 no and 1668 yes targets
#train_smote has 3544 observations original train set had 3617

library(parallel)
library(caretEnsemble)
set.seed(223)
modelsbal <- caretList(target~., data=train_smote, trControl=control, methodList=algorithmList)

resultsB <- resamples(modelsbal)
summary(resultsB)
#All now have mean accuracy in 0.70's
dotplot(resultsB)
modelCor(resultsB)
splom(resultsB)


# stack balanced model using glm
stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
set.seed(223)
stack.glmB <- caretStack(modelsbal, method="glm", metric="Accuracy", trControl=stackControl)
print(stack.glmB)
#accuracy of 0.8266

glmB.preds <- predict(stack.glmB, test, type = 'prob')
summary(glmB.preds)
summary(numeric.targets)

glmB.roc <- roc(numeric.targets, glmB.preds)
print(glmB.roc$auc)
#AUC 0.702

plot(glm.roc, col='red')
par(new=T)
plot(glmB.roc, col='pink')
glmB.best.thresh <- coords(roc = glmB.roc, x = 'best', ret = 'threshold')
glmB.test.preds <- as.numeric(glmB.preds >= glmB.best.thresh)
confusionMatrix(glmB.test.preds, numeric.targets, positive = '1')
#   0   1
#0 653  52
#1 147  52
#Accuracy : 0.7799   

#Our model made from a balance train data set had worse performance


######MODEL 3#####
#Try stacking the better model with rf

#stacking with random forrest

models<-models1
stack.rf <- caretStack(models, method="rf", metric="Accuracy", trControl=stackControl)
print(stack.rf)
rf.preds <- predict(stack.rf, test, type = 'prob')
str(rf.preds)
str(numeric.targets)
#numeric.targets <- as.numeric(test$target) - 1
rf.roc <- roc(numeric.targets, rf.preds)
print(rf.roc$auc)
#auc 0.671
par(new=T)
plot(rf.roc, col='blue')
legend('bottomright', c('stacked rf', 'stacked glm', 'stacked glm w/ SMOTE'), col = c('blue', 'red', 'pink'), lty=1)

rf.best.thresh <- coords(roc = rf.roc, x = 'best', ret = 'threshold')
rf.test.preds <- as.numeric(rf.preds >= rf.best.thresh)
confusionMatrix(rf.test.preds, numeric.targets, positive = '1')
#   0   1
#0 589  43
#1 211  61

#Accuracy : 0.719     


#########AVG of stacking###
cbind(rf.preds, glm.preds)
average.preds.numeric <- rowMeans(cbind(rf.preds, glm.preds))
avg.roc <- roc(numeric.targets, average.preds.numeric)
print(avg.roc$auc)
avg.best.thresh <- coords(avg.roc, x = 'best', ret = 'threshold')
average.preds <- as.numeric(average.preds.numeric >= avg.best.thresh)

postResample(as.factor(average.preds), as.factor(numeric.targets))
#accuracy .764
par(new=T)
plot(avg.roc, col='black')
legend('bottomright', c('stacked rf', 'stacked glm', 'stacked glm w/ SMOTE', 'avg rf & glm'),
       col = c('blue', 'red', 'pink', 'black'), lty=1)

