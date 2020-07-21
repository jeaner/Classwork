library(data.table)
library(pROC)
library(MASS)
chn <- fread('C:/MSDS/Meth/Wk6/churn.csv')

str(chn)
summary(chn)
# Omit any NA responses (looksl like theres 11 in the totalcharges)
churn <- na.omit(chn)
summary(churn)
# Convert everything to factors or numbers
churn[, gender:=as.factor(gender)]
churn[, SeniorCitizen:=as.factor(SeniorCitizen)]
churn[, Partner:=as.factor(Partner)]
churn[, Dependents:=as.factor(Dependents)]
churn[, tenure:=as.numeric(tenure)]
churn[, PhoneService:=as.factor(PhoneService)]
churn[, MultipleLines:=as.factor(MultipleLines)]
churn[, InternetService:=as.factor(InternetService)]
churn[, OnlineSecurity:=as.factor(OnlineSecurity)]
churn[, OnlineBackup:=as.factor(OnlineBackup)]
churn[, DeviceProtection:=as.factor(DeviceProtection)]
churn[, TechSupport:=as.factor(TechSupport)]
churn[, StreamingTV:=as.factor(StreamingTV)]
churn[, StreamingMovies:=as.factor(StreamingMovies)]
churn[, Contract:=as.factor(Contract)]
churn[, PaperlessBilling:=as.factor(PaperlessBilling)]
churn[, PaymentMethod:=as.factor(PaymentMethod)]
churn[, Churn:=as.factor(Churn)]
str(churn)
View(churn)

model1 <- glm(Churn ~ Contract + TotalCharges + gender + SeniorCitizen + Partner + Dependents + tenure
             + PhoneService + MultipleLines + InternetService + OnlineSecurity + OnlineBackup
             + DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract
             + PaperlessBilling + MonthlyCharges + TotalCharges, data = dt, family = 'binomial')

summary(model)
#RUN STEP AIC TO FIND SIGNIFICANT FACTORS
stepAIC(model)
model2 <- glm(Churn ~ Contract + SeniorCitizen + tenure + InternetService + OnlineSecurity
              + TechSupport + PaymentMethod + StreamingTV + MultipleLines + Dependents
              + StreamingMovies + PaperlessBilling + MonthlyCharges + TotalCharges, data = churn, family = 'binomial')
summary(model2)
model3 <- glm(Churn ~ Contract + SeniorCitizen + tenure + InternetService + Dependents +
              MonthlyCharges + PaperlessBilling + TotalCharges, data = churn, family = 'binomial')
summary(model3)
stepAIC(model3)

model4 <- glm(Churn ~ Contract + SeniorCitizen + tenure + InternetService
              + PaperlessBilling + TotalCharges, data = churn, family = 'binomial')
summary(model4)

model5 <- glm(Churn ~ Contract + SeniorCitizen + tenure + InternetService
           + StreamingTV + MultipleLines
              + StreamingMovies + PaperlessBilling + MonthlyCharges + TotalCharges, data = churn, family = 'binomial')
summary(model5)

prdct4<- predict(model4, type = 'response', churn)
prdct4
roc4 <- roc(churn$Churn, prdct4)
roc4
plot(roc4)
coords(roc4, x = "best")
coords(roc = roc4, x = "best", best.method = 'closest.topleft')

#NOTHING BEATS THE ORIGINAL STEP AIC WITH MODEL2, EVEN IF THERE ARE SOME HIGH P-VALUES AND N/A'S 
#FOR EXAMPLE REMOVING STEAMING VIDEO BECAUSE "NO INTERNET SERVICE" IS N/A ALSO REMOVES THE
#SIGNIFICANT FACTOR OF YES STREAMING VIDEOS, MAKING A WORSE STEP AIC SCORE

prdct<- predict(model2, type = 'response', churn)
prdct
#see the prediction values

#use these prediction values in a ROC curve
roc2 <- roc(churn$Churn, prdct)
roc2
plot(roc2)
coords(roc2, x = "best")
coords(roc = roc2, x = "best", best.method = 'closest.topleft')
