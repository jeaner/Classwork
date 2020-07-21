
library (data.table)
crime <- fread('http://www.statsci.org/data/general/uscrime.txt')
head(crime)
str(crime)
nums <- sapply(crime, is.numeric)
head(nums)
plot(crime[, nums, with = F])
#let's adjust what columns we want to include
choice_cols <- c('Crime', 'Po1', 'Po2', 'LF','Pop','Wealth')
plot(crime[, choice_cols, with = F])
#now lets fit the model
fit <- lm(Crime ~ Po1 + Po2 + LF + Pop + Wealth, data=crime)
summary(fit)
library(MASS)
stepAIC(fit)
fit1 <- lm(Crime ~ Po1 + LF + Wealth, data=crime)#fit on stepAIC variables
summary(fit1)