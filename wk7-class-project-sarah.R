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
bank.dt[, job:=NULL]
bank.dt[, duration:=NULL]
str(bank.dt)

preprocessParams <- preProcess(bank.dt, method=c("range"))
print(preprocessParams)
norm.dt <- predict(preprocessParams, bank.dt)
summary(norm.dt)

train.index <- createDataPartition(bank.dt$target, p = 0.8)$Resample1
train <- norm.dt[train.index]
test <- norm.dt[-train.index]

library(parallel)

library(caretEnsemble)

control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
algorithmList <- c('lda', 'rpart', 'glm', 'knn', 'svmRadial')
set.seed(223)
models <- caretList(target~., data=bank.dt, trControl=control, methodList=algorithmList)
results <- resamples(models)
summary(results)
dotplot(results)
#lda and rpart seem to be too closely correlated
#it would be beneficial to delete one of these

# correlation between results
modelCor(results)

splom(results)



# stack using glm
stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
set.seed(223)
stack.glm <- caretStack(models, method="glm", metric="Accuracy", trControl=stackControl)
print(stack.glm)
#accuracy of 0.891322
glm.preds <- predict(stack.glm, test, type = 'prob')
head(glm.preds)
summary(glm.preds)
summary(numeric.targets)
str(glm.preds)
str(numeric.targets)
numeric.targets <- as.numeric(test$target) - 1
glm.roc <- roc(numeric.targets, glm.preds)
print(glm.roc$auc)
#higher than svm curve

# here is an example of plotting multiple ROC curves at once
plot(glm.roc, col='red')

glm.best.thresh <- coords(roc = glm.roc, x = 'best', ret = 'threshold')
glm.test.preds <- as.numeric(glm.preds >= glm.best.thresh)
confusionMatrix(glm.test.preds, numeric.targets, positive = '1')


#stacking with random forrest
stack.rf <- caretStack(models, method="rf", metric="Accuracy", trControl=stackControl)
print(stack.rf)
#mtry  Accuracy   Kappa    
#2     0.8951564  0.2635198
rf.preds <- predict(stack.rf, test, type = 'prob')
head(rf.preds)
summary(rf.preds)
summary(numeric.targets)
str(rf.preds)
str(numeric.targets)
numeric.targets <- as.numeric(test$target) - 1
rf.roc <- roc(numeric.targets, rf.preds)
print(rf.roc$auc)
#higher than svm curve

# here is an example of plotting multiple ROC curves at once
plot(rf.roc, col='red')
par(new=T)
plot(glm.roc, col='blue')
legend('bottomright', c('stacked rf', 'stacked glm'), col = c('red', 'blue'), lty=1)

rf.best.thresh <- coords(roc = rf.roc, x = 'best', ret = 'threshold')
rf.test.preds <- as.numeric(rf.preds >= rf.best.thresh)
confusionMatrix(rf.test.preds, numeric.targets, positive = '1')
#only 0.7943 accuracy vs .885 no information rate
#########
cbind(rf.preds, glm.numeric)
average.preds.numeric <- rowMeans(cbind(rf.preds, glm.preds))
avg.roc <- roc(numeric.targets, average.preds.numeric)
print(avg.roc$auc)
avg.best.thresh <- coords(avg.roc, x = 'best', ret = 'threshold')
average.preds <- as.numeric(average.preds.numeric >= avg.best.thresh)

postResample(as.factor(average.preds), as.factor(numeric.targets))
#accuracy .888
par(new=T)
plot(avg.roc, col='black')
legend('bottomright', c('stacked rf', 'stacked glm', 'avg'), col = c('red', 'blue', 'black'), lty=1)








# SVM model
library(e1071)
model <- tune(svm,
              target ~ .,
              data = train, 
              ranges = list(gamma = c(0.001, 0.01, 0.1),
                            cost = c(0.1, 1, 5, 10)),
              tunecontrol = tune.control(sampling = "cross", cross = 3),
              kernel = 'radial'
)
# "performance" is classification error for classification
model
svm.model <- svm(target ~ .,
                 data = train,
                 kernel = 'radial',
                 gamma = 0.01,
                 C = 10,
                 probability = T)
svm.model
postResample(predict(svm.model, train), train$target)
postResample(predict(svm.model, test), test$target)

library(pROC)
numeric.targets <- as.numeric(test$target) - 1
svm.pred.proba <- predict(svm.model, test, probability = T)
# incredibly cryptic, but this is how you get the 
# prediction probabilities for heart.disease = T
svm.numeric.preds <- attr(svm.pred.proba, 'probabilities')[, 2]
svm.roc <- roc(numeric.targets, svm.numeric.preds)
print(svm.roc$auc)
plot(svm.roc)
svm.best.thresh <- coords(roc = svm.roc, x = 'best', ret = 'threshold')

svm.test.preds <- as.numeric(svm.numeric.preds >= svm.best.thresh)
confusionMatrix(svm.test.preds, numeric.targets, positive = '1')
# casting the predictions as a factor gets accuracy, otherwise it's RMSE
postResample(as.factor(svm.test.preds), as.factor(numeric.targets))  # 82% accuracy

rf.model <- train(target ~ .,
                  method = 'rf',
                  data = train,
                  trControl = trainControl(method = 'repeatedcv', number = 3, repeats = 3),
                  tuneGrid = expand.grid(mtry = c(4, 6, 8, 10, 12)),  # default is 4
                  ntree = 500)

rf.model

library(randomForest)
rf.model <- randomForest(target ~ .,
                         data = train,
                         mtry = 6)
rf.preds <- predict(rf.model, test, type = 'prob')
rf.preds.numeric <- rf.preds[, 2]
rf.roc <- roc(numeric.targets, rf.preds.numeric)
print(rf.roc$auc)

# here is an example of plotting multiple ROC curves at once
plot(rf.roc, col='red')
par(new=T)
plot(svm.roc)
legend('bottomright', c('rf', 'svm'), col = c('red', 'black'), lty=1)

rf.best.thresh <- coords(rf.roc, x = 'best', ret = 'threshold')
rf.test.preds <- as.numeric(rf.preds.numeric >= rf.best.thresh)
postResample(as.factor(rf.test.preds), as.factor(numeric.targets))

average.preds.numeric <- rowMeans(cbind(rf.preds.numeric, svm.numeric.preds))
avg.roc <- roc(numeric.targets, average.preds.numeric)
print(avg.roc$auc)
avg.best.thresh <- coords(avg.roc, x = 'best', ret = 'threshold')
average.preds <- as.numeric(average.preds.numeric >= avg.best.thresh)

postResample(as.factor(average.preds), as.factor(numeric.targets))

plot(avg.roc, col='red')
par(new=T)
plot(svm.roc)
legend('bottomright', c('rf+svm', 'svm'), col = c('red', 'black'), lty=1)

trControl <- trainControl(method  = "repeatedcv",
                          number  = 3,
                          repeats = 2)

fit <- train(target ~ .,
             method     = 'nb',  # naive bayes from klaR package
             tuneGrid   = data.frame("fL"=0, "usekernel"=c(T, F), "adjust"=1),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = train)

fit

nb.preds <- predict(fit, test, type = 'prob')
nb.preds.numeric <- nb.preds[, 2]
nb.roc <- roc(numeric.targets, nb.preds.numeric)
print(nb.roc$auc)

average.preds.numeric <- rowMeans(cbind(rf.preds.numeric, svm.numeric.preds, nb.preds.numeric))
avg.roc <- roc(numeric.targets, average.preds.numeric)
print(avg.roc$auc)
avg.best.thresh <- coords(avg.roc, x = 'best', ret = 'threshold')
average.preds <- as.numeric(average.preds.numeric >= avg.best.thresh)

postResample(as.factor(average.preds), as.factor(numeric.targets))

plot(avg.roc, col='red')
par(new=T)
plot(svm.roc)
par(new=T)
plot(rf.roc, col='blue')
par(new=T)
plot(nb.roc, col='green')
legend('bottomright', c('rf+svm+nb', 'svm', 'rf', 'nb'), col = c('red', 'black', 'blue', 'green'), lty=1)

