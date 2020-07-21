library(data.table)
library(pROC)
library(MASS)
chn <- fread('C:/MSDS/Meth/Wk6/churn.csv')

str(chn)
summary(chn)
# Omit any NA responses (looksl like theres 11 in the totalcharges)
churn <- na.omit(chn)
summary(churn)
# Convert data types to factors
churn[, gender:=as.factor(gender)]
churn[, SeniorCitizen:=as.factor(SeniorCitizen)]
churn[, Partner:=as.factor(Partner)]
churn[, Dependents:=as.factor(Dependents)]
churn[, tenure:=as.factor(tenure)]
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

#churn_model <- glm(Churn ~ ., data = churn, family = "binomial")
# Look at summary for overview of possible significant factors
#summary(churn_model)

# Let stepAIC identify significant factors
stepAIC(churn)

# Create a new model with only the significant factors identified
churn_model_2 <- glm(Churn ~ OnlineSecurity + Dependents + SeniorCitizen + DeviceProtection
                     + PaymentMethod + TechSupport + PaperlessBilling + MonthlyCharges 
                     + MultipleLines + StreamingTV + StreamingMovies + InternetService 
                     + Contract + tenure, data = churn, family = "binomial")
summary(churn_model_2)
plot(churn_model_2)
?glm

# Model again eliminating factors with high p-values
churn_model_3 <- glm(Churn ~ SeniorCitizen + PaperlessBilling +  MonthlyCharges
                     + MultipleLines + PaymentMethod + StreamingTV + StreamingMovies 
                     + InternetService + Contract,
                     data = churn, family = "binomial")
summary(churn_model_3)
plot(churn_model_3)

# Store prediciton values 
churn_preds <- predict(churn_model_2, type = 'response', churn)

# Create ROC curve from data and predictions
churn_roc <- roc(churn$Churn, churn_preds)
# Summary of ROC with AUC
churn_roc
# Plot of ROC
plot(churn_roc)

# Get best threshold value for predictions
coords(churn_roc, x = "best")
