library(data.table)
library(installr)
updateR()

churn_path <- "C:/Users/buckf/Documents/MSDS 660 (Statistical Methods Experimental Design)/Week 6 (Logistic Regression)/churn.csv"
churn_dt <- fread(churn_path)

View(churn_dt)
str(churn_dt)
summary(churn_dt)


#Most feature variables can be made into factor datatypes with appropriate levels. 
churn_dt[, Contract:= as.factor(Contract)]
churn_dt[, gender:= as.factor(gender)]
churn_dt[, SeniorCitizen:=as.factor(SeniorCitizen)]
churn_dt[, Partner:=as.factor(Partner)]
churn_dt[, Dependents:=as.factor(Dependents)]
churn_dt[, PhoneService:=as.factor(PhoneService)]
churn_dt[, MultipleLines:=as.factor(MultipleLines)]
churn_dt[, InternetService:=as.factor(InternetService)]
churn_dt[, OnlineSecurity:=as.factor(OnlineSecurity)]
churn_dt[, DeviceProtection:=as.factor(DeviceProtection)]
churn_dt[, TechSupport:=as.factor(TechSupport)]
churn_dt[, StreamingTV:=as.factor(StreamingTV)]
churn_dt[, StreamingMovies:=as.factor(StreamingMovies)]
churn_dt[, PaperlessBilling:=as.factor(PaperlessBilling)]
churn_dt[, PaymentMethod:=as.factor(PaymentMethod)]
churn_dt[, OnlineBackup:=as.factor(OnlineBackup)]

#The Churn target variable can be factored as well.
churn_dt[, Churn:= as.factor(Churn)]


#Let's make sure the datatypes make sense and 
str(churn_dt)
#Looking Good! All of are variables have factor levels equal to or less than 4.

#Let's look for the infamous na's.
summary(churn_dt)
#Awesome it looks like our data is all there. Total Chrages has 11 NA's but that is not that substantial to throw off our model.



churn_model <- glm(formula = Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges, data = churn_dt, family = 'binomial')
summary(churn_model)

library(MASS)
AICmodel <- stepAIC(churn_model)

summary(AICmodel)


#ROC curve formulation
roc_curve <- roc(churn_dt$Churn, predict(AICmodel, newdata=churn_dt, type="response"))
roc_curve
plot(roc_curve)

coords(roc = roc_curve, x='best', best.method = 'closest.topleft')












