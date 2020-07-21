library(data.table)
dt <- fread('Data Science/MSDS_660/Week 6/churn.csv')
View(dt)
str(dt)
summary(dt)
dt$Churn <- as.factor(dt$Churn)
str(cdt)
model <- glm(Churn ~ Contract + TotalCharges + gender + SeniorCitizen + Partner + Dependents + tenure
             + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup
             + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract
             + PaperlessBilling + MonthlyCharges + TotalCharges, data = dt, family = 'binomial')
summary(model)
library(MASS)
AIC(model)
stepAIC(model)
AICmodel <- glm(Churn ~ Contract + SeniorCitizen + tenure + InternetService + OnlineSecurity
                + TechSupport + PaymentMethod + StreamingTV + MultipleLines + Dependents
                + StreamingMovies + PaperlessBilling + MonthlyCharges + TotalCharges, data = dt, family = 'binomial')
summary(AICmodel)
library(pROC)
preds <- predict(AICmodel, type = 'response', dt)
preds
roc_curve <- roc(dt$Churn, preds)
roc_curve
plot(roc_curve)
coords(roc_curve, x = 'best')
