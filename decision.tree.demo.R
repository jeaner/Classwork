library(data.table)
library(caret)
set.seed(42)
iris.dt <- as.data.table(iris)

train.idxs <- createDataPartition(iris.dt$Species, p = 0.8)$Resample1
train <- iris.dt[train.idxs]
test <- iris.dt[-train.idxs]

library(rpart)
model <- rpart(Species ~ ., data = train, method = 'class', control = list(minsplit = 1))
plot(model)
text(model)
pred.matrix <- predict(model, test)
pred.matrix #probability of the different classes
class(pred.matrix)
# gets max across rows
preds <- colnames(pred.matrix)[apply(pred.matrix, 1, which.max)]
confusionMatrix(preds, test$Species)
# works perfectly, but too easy of an example


filename <- 'C:/MSDS/MachL/Wk2/auto.dt.nona.csv'
mpg.dt <- fread(filename)
train.idxs <- createDataPartition(mpg.dt$mpg, p = 0.8)$Resample1
train <- mpg.dt[train.idxs]
test <- mpg.dt[-train.idxs]

# we are setting some hyperparameters to be rediculous so we will overfit on purpose
model <- rpart(mpg ~ ., data = train, method = 'anova', control = list(minsplit = 1, cp = 0.00001))
#Anova for a regression
#Default cp is .01 we are making very small to show what overfitting looks like

plot(model)
text(model)


model2 <- rpart(mpg ~ ., data = train, method = 'anova', control = list(minsplit = 20, cp = 0.01))
plot(model2)
text(model2)
summary(model2)


model3 <- rpart(mpg ~ ., data = train, method = 'anova', control = list(minsplit = 10, cp = 0.001))
plot(model3)
text(model3)
summary(model3)

preds <- predict(model, test)
postResample(preds, test$mpg)
preds <- predict(model, train)
postResample(preds, train$mpg)
# test scoring much lower than train, sign of overitting
summary(model)
# still don't have any leaf nodes with 1 sample -- that would be the most extreme overfitting

#Single decision tree, simple