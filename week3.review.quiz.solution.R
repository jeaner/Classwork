library(data.table)
library(caret)
set.seed(42)
iris.dt <- as.data.table(iris)

train.idxs <- createDataPartition(iris.dt$Species, p = 0.8)$Resample1
train <- iris.dt[train.idxs]
test <- iris.dt[-train.idxs]

trControl <- trainControl(method = 'repeatedcv', number = 3, repeats = 10)

# usekernel = F seems to be best consistently, but not by much
fit <- train(Species ~ .,
             method = 'nb',
             trControl = trControl,
             tuneGrid = expand.grid(fL=c(0, 1), usekernel = c(T, F), adjust = 1),
             data = train)

fit

preds <- predict(fit, test)
# depending on how the data was split (depends on our random seed),
# we will get perfect classification, or only a few misclassified
confusionMatrix(preds, test$Species)
