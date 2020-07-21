library(data.table)
dt <- fread('C:/MSDS/Meth/Wk6/titanic.csv')

str(dt)
summary(dt)
#missing 200 Fares

#change class to factor
dt[, Pclass:=as.factor(Pclass)]
str(dt)

dt[, Sex:=as.factor(Sex)]
str(dt)
#logistic regression- generalized linear model
model <- glm(Survived ~ Sex, data = dt, family = "binomial")

summary(model)
#autodummied 
#z-value is significant
#gives AIC value


preds <- predict(model, type = 'response', dt)
preds
#two values, males vs females
plot(dt$Sex, preds)
dt[, ismale:=0]
dt[Sex == 'male', ismale:=1]
#makes new columns


plot(jitter(dt$ismale, 1), jitter(dt$Survived, 1))

library(pROC)
roc_curve <- roc(dt$Survived, preds)
roc_curve
plot(roc_curve)

# some NAs in Fare
summary(dt)
dt_low <- dt
model <- glm(Survived ~ Fare, data = dt_low, family = "binomial")
summary(model)
preds <- predict(model, type = 'response', dt_low)
plot(dt_low$Fare, dt_low$Survived)
# creates 
plot(dt_low$Fare, jitter(dt_low$Survived))


newdat <- data.frame(Fare=seq(min(dt_low$Fare), max(dt_low$Fare),len=100))
newdat$Survived = predict(model, newdata=newdat, type="response")
lines(Survived ~ Fare, data = newdat)

# this is how far you have to zoom out to see the logistic function at work
plot(dt_low$Fare, dt_low$Survived, xlim=c(-1000, 1000))
preds <- data.frame(Fare=seq(-1000, 1000,len=1000))
preds$Survived = predict(model, newdata=preds, type="response")
lines(Survived ~ Fare, data = preds)
#weak predictor so the log curve is VERY wide

roc_curve <- roc(dt_low$Survived, predict(model, newdata=dt_low, type="response"))
roc_curve
plot(roc_curve)

?coords
#use roc and x coordinatess highest sum of specificity and sensitivity
coords(roc_curve, x="best")
model2 <- glm(Survived ~ Pclass, data = dt, family = "binomial")

summary(model2)

model3 <- glm(Survived ~ Fare, data = dt, family = "binomial")

summary(model3)
preds <- predict(model3, type = 'response', dt)
preds
#two values, males vs females
plot(dt$Fare, preds)
