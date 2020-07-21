library(data.table)
library(caret)
library(dplyr)  # so we can use the pipe operator (>%>)
library(ggplot2)
library(xgboost)
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
# convert heart disease to binary outcome, 1 is heart disease
heart.dt[heart.disease > 0, heart.disease:=1]
heart.dt[, heart.disease:=as.factor(heart.disease)]
# need to rename target factor levels
levels(heart.dt$heart.disease) <- c('no', 'yes')

dim(heart.dt)  # 13 features -- we will need this for a hyperparameter in a sec

tr.idxs <- createDataPartition(heart.dt$heart.disease, p = 0.8)$Resample1
train <- heart.dt[tr.idxs]
test <- heart.dt[-tr.idxs]

library(parallel)
library(doMC)
registerDoMC(cores = detectCores() - 1)

# adapted from here:
# https://stats.stackexchange.com/a/181615/120921

xgb.trcontrol = trainControl(
  method = "cv",
  number = 3,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",  # save losses across all models
  classProbs = TRUE,  # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)

xgb.hyperparameters = expand.grid(
  nrounds = 300,  # number of times residuals are fit
  max_depth = c(3, 6),  # make number of levels in trees
  eta = c(0.01, 0.001),  # learning rate, how much to weight the models fit to the residuals
  gamma = 1,  # regularization, like L2 regularization -- higher means more general model
  colsample_bytree = 1,  # subsampling columns
  min_child_weight = 1,  # default value, larger means higher bias
  subsample = c(0.4, 0.5)  # subsampling rows,  around 0.4 is usually best
)

# train the model for each parameter combination in the grid, 
# using CV to evaluate
xgb.model = train(
  x = as.matrix(train %>%
                  select(-heart.disease)),
  # factors have to have proper names, not "0" and "1", which is why we converted the names earlier
  y = train$heart.disease,
  trControl = xgb.trcontrol,
  tuneGrid = xgb.hyperparameters,
  method = "xgbTree",
  # this is binary classification -- this is the function we will use to calculate residuals
  # see the ?xgboost docs for more built-in objective functions
  objective = 'binary:logistic'
)

xgb.model

# scatter plot of the AUC against max_depth and eta
ggplot(xgb.model$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")

# feature importances
varImp(xgb.model)
varImp(xgb.model, scale = F)
vi <- varImp(xgb.model, scale = F)
class(vi)
vi$importance
# bottom, left, top, right margins
par(mar=c(12, 4.1, 4.1, 2.1))
# have to set the upper limit to the bar plot for a better format
upper.bound <- ceiling(max(vi$importance)*20)/20  # rounds to nearest 0.05
barplot(vi$importance$Overall, names.arg = rownames(vi$importance), las = 2, ylim = c(0, upper.bound))
# set margin back to default
par(mar=c(5.1, 4.1, 4.1, 2.1))

