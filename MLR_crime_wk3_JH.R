library(data.table)
dt <- fread('C:/MSDS/Meth/wk4/uscrime.txt')
View(dt)
str(dt)
summary(dt)
#Variable	 	Description
#M		percentage of males aged 14-24 in total state population
#So		indicator variable for a southern state
#Ed		mean years of schooling of the population aged 25 years or over
#Po1		per capita expenditure on police protection in 1960
#Po2		per capita expenditure on police protection in 1959
#LF		labour force participation rate of civilian urban males in the age-group 14-24
#M.F		number of males per 100 females
#Pop		state population in 1960 in hundred thousands
#NW		percentage of nonwhites in the population
#U1		unemployment rate of urban males 14-24
#U2		unemployment rate of urban males 35-39
#Wealth		wealth: median value of transferable assets or family income
#Ineq		income inequality: percentage of families earning below half the median income
#Prob		probability of imprisonment: ratio of number of commitments to number of offenses
#Time		average time in months served by offenders in state prisons before their first release
#Crime		crime rate: number of offenses per 100,000 population in 1960
dt$So <- as.factor(dt$So)
dt$Pop <- as.numeric(dt$Pop)
dt$Wealth <- as.numeric(dt$Wealth)
dt$Crime <- as.numeric(dt$Crime)
str(dt)
View(dt)
summary(dt)



hist(dt$LF)
hist(dt$Crime)

nums <- sapply(dt, is.numeric)
plot(dt[, nums, with = F])
library(corrplot)
cor(dt[,nums, with=F])>0.5
corrplot(cor(dt[,nums, with=F]), method='number')
#fit to all variables
fit <- lm(Crime ~ ., data=dt)
summary(fit)
library(MASS)
stepAIC(fit)
fit2<-lm(formula = Crime ~ M + Ed + Po1 + M.F + U1 + U2 + Ineq + Prob, 
         data = dt)
summary(fit2)
#get a visual with termplot showing us the residulas as slopes
par(mfrow=c(3,3))
termplot(fit2, data=dt, partial.resid = T)
