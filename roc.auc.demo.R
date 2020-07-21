pred.proba <- c(0.15, 0.23, 0.45, 0.55, 0.67, 0.88, 0.97)
actual <- c(0, 1, 0, 1, 0, 1, 1)

library(pROC)
roc.curve <- roc(actual, pred.proba)
roc.curve
plot(roc.curve)
#goes through predictions step by step as rounding point
roc.curve$auc

# gets the best point ahe ROC curve based on the sum of sensitivity and specificity
coords(roc = roc.curve, x = 'best', ret='threshold')
#asking to return threshold for what we should round predictions on
#best.method
#says to round at 0.775
library(data.table)
dt <- data.table(cbind(pred.proba, actual)) #cbind column binding two vectors
names(dt) <- c('pred.proba', 'actual')
dt[, pred:=1]
dt[pred.proba < 0.775, pred:=0]
#using threshold above to make prediction
library(caret)
# careful!  The positive class is auto-detected as 0... set positive value as 1
confusionMatrix(dt$pred, dt$actual, positive = '1')
#Sensitivity : 0.5000          
#Specificity : 1.0000  
#No information rate: baseline if you guess all positive that would be your accuracy