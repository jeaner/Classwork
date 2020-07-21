library(data.table)
library(caret)
set.seed(42)

# load data
filename <- 'C:/MSDS/MachL/wk7/heart.disease.data.clean.csv'
heart.dt <- fread(filename)

# label columns in a more informative way
col.labels <- c('age',
                'sex',
                'chest.pain.type',
                'resting.blood.pressure',
                'cholesterol',
                'num.cigs.per.day',
                'years.as.smoker',
                'fasting.blood.sugar',
                'family.history.heart.disease',
                'resting.ecg.results',
                'max.heart.rate.exercise',
                'exercise.induced.angina',
                'blood.disorder',  # Thalassemia I think
                'heart.disease')

names(heart.dt) <- col.labels
# convert heart disease to binary outcome, 1 is heart disease
heart.dt[heart.disease > 0, heart.disease:=1]
heart.dt[, heart.disease:=as.factor(heart.disease)]
# rename factor levels to more intuitive labels
levels(heart.dt$heart.disease) <- c('no', 'yes')

str(heart.dt)

# make a train/test split of the data
tr.idxs <- createDataPartition(heart.dt$heart.disease, p = 0.8)$Resample1
train <- heart.dt[tr.idxs]
test <- heart.dt[-tr.idxs]


# SVM model e1071 has built in tune - to do cross validation
library(e1071)
model <- tune(svm,
              heart.disease ~ .,
              data = train, 
              ranges = list(gamma = c(0.001, 0.01, 0.1),
                            cost = c(0.1, 1, 5, 10)),
              tunecontrol = tune.control(sampling = "cross", cross = 3),
              kernel = 'radial'
)
# "performance" is classification error for classification

model
#set best perameters from model in gamma
svm.model <- svm(heart.disease ~ .,
                 data = train,
                 kernel = 'radial',
                 gamma = 0.01,
                 C = 10,
                 probability = T)
svm.model
postResample(predict(svm.model, train), train$heart.disease)
postResample(predict(svm.model, test), test$heart.disease)

library(pROC)
#make numeric targets, converting to factors, 2 and 1 so much subtract
numeric.targets <- as.numeric(test$heart.disease) - 1
svm.pred.proba <- predict(svm.model, test, probability = T)
# incredibly cryptic, but this is how you get the 
# prediction probabilities for heart.disease = T
svm.numeric.preds <- attr(svm.pred.proba, 'probabilities')[, 2]
svm.roc <- roc(numeric.targets, svm.numeric.preds)
print(svm.roc$auc)
plot(svm.roc)
#roc curve for test set
svm.best.thresh <- coords(roc = svm.roc, x = 'best', ret = 'threshold')

svm.test.preds <- as.numeric(svm.numeric.preds >= svm.best.thresh)
confusionMatrix(svm.test.preds, numeric.targets, positive = '1')
# casting the predictions as a factor gets accuracy, otherwise it's RMSE
postResample(as.factor(svm.test.preds), as.factor(numeric.targets))  # 82% accuracy

rf.model <- train(heart.disease ~ .,
                  method = 'rf',
                  data = train,
                  trControl = trainControl(method = 'repeatedcv', number = 3, repeats = 3),
                  tuneGrid = expand.grid(mtry = c(4, 6, 8, 10, 12)),  # default is 4
                  ntree = 500)

rf.model
#mtry 6
library(randomForest)
rf.model <- randomForest(heart.disease ~ .,
                         data = train,
                         mtry = 6)
rf.preds <- predict(rf.model, test, type = 'prob')
rf.preds.numeric <- rf.preds[, 2]
rf.roc <- roc(numeric.targets, rf.preds.numeric)
print(rf.roc$auc)
#higher than svm curve

# here is an example of plotting multiple ROC curves at once
plot(rf.roc, col='red')
par(new=T)
plot(svm.roc)
legend('bottomright', c('rf', 'svm'), col = c('red', 'black'), lty=1)
#SVM doing better for true negative rate
rf.best.thresh <- coords(rf.roc, x = 'best', ret = 'threshold')
rf.test.preds <- as.numeric(rf.preds.numeric >= rf.best.thresh)
postResample(as.factor(rf.test.preds), as.factor(numeric.targets))
#77% accuracy vs 80% accuracy for svm
cbind(rf.preds.numeric, svm.numeric.preds)
average.preds.numeric <- rowMeans(cbind(rf.preds.numeric, svm.numeric.preds))
avg.roc <- roc(numeric.targets, average.preds.numeric)
print(avg.roc$auc)
avg.best.thresh <- coords(avg.roc, x = 'best', ret = 'threshold')
average.preds <- as.numeric(average.preds.numeric >= avg.best.thresh)

postResample(as.factor(average.preds), as.factor(numeric.targets))

plot(avg.roc, col='red')
par(new=T)
plot(svm.roc, col='blue')
par(new=T)
plot(rf.roc)
legend('bottomright', c('rf+svm', 'svm', 'rf'), col = c('red', 'blue', 'black'), lty=1)
