library(data.table)
set.seed(42)  # set the random seed for reproducibility

filename <- 'C:/MSDS/MachL/Wk2/heart.disease.data.clean.csv'
heart.dt <- fread(filename)
str(heart.dt)
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
str(heart.dt)
# convert heart disease to binary outcome, 1 is heart disease
#classification 
heart.dt[heart.disease > 0, heart.disease:=1]
heart.dt[, heart.disease:=as.factor(heart.disease)]
dim(heart.dt)  # 13 features -- we will need this for a hyperparameter in a sec
str(heart.dt)
library(caret)
tr.idxs <- createDataPartition(heart.dt$heart.disease, p = 0.8)$Resample1
train <- heart.dt[tr.idxs]
test <- heart.dt[-tr.idxs]

# enable parallel processing with all except for one of our CPU cores
library(parallel)

#library(doMC)
registerDoMC(cores = detectCores() - 1)

trControl <- trainControl(method = 'repeatedcv',
                          number = 3,
                          repeats = 4)

rf.model <- train(heart.disease ~ .,
                  data = train,
                  method = 'rf',
                  trControl = trControl,
                  ntree = 250,  # makes it run a bit faster
                  tuneGrid = expand.grid(mtry = c(4, 8, 12)))
#mtry features offset, 4 is defaualt, 13 features in heart data set sq.rt of 13 
#is about 4, so we will try 4, 8, and 12

rf.model
#cohen's kappa
#

tr.preds <- predict(rf.model, train)
postResample(tr.preds, train$heart.disease)

te.preds <- predict(rf.model, test)
postResample(te.preds, test$heart.disease)
#Overfitting test .79 and train was 1 (100%)
#decrease our varience and move to biased side
#hyperparameters ntree, mtry at each split, overfitting could be depth (tree too deep),
##nodesize-how many samples in leaf nodes classification is 1 and regression 5

rf.model2 <- train(heart.disease ~ .,
                  data = train,
                  method = 'rf',
                  trControl = trControl,
                  ntree = 250,  # makes it run a bit faster
                  nodesize= 13,
                  maxnodes= 13,
                  tuneGrid = expand.grid(mtry = c(4, 8, 12)))
rf.model2
tr.preds2 <- predict(rf.model2, train)
postResample(tr.preds2, train$heart.disease)

te.preds2 <- predict(rf.model2, test)
postResample(te.preds2, test$heart.disease)

#Feature importances!! Ranked score for the different features using gene/entropy
#what feature results in the biggest purification of the classes
varImp(rf.model, scale = F)
#blood disorder gives the most splits
varImpPlot(rf.model$finalModel, scale = F)
