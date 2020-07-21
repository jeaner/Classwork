library(data.table)

# data from here (car lab 1 data):
# http://web.archive.org/web/20150318011405/http://web.grinnell.edu/individuals/kuipers/stat2labs/topics.html
# explanation here:
# http://web.archive.org/web/20120227071352/http://web.grinnell.edu:80/individuals/kuipers/stat2labs/Handouts/CarLab%20June%20Submitted.pdf
filename <- 'C:/MSDS/Meth/Wk3/used_cars.csv'
dt <- fread(filename)
dim(dt)
# selects just the numeric columns
nums <- sapply(dt, is.numeric)
plot(dt[, nums, with = F])
choice_cols <- c('Price', 'Mileage', 'Cylinder', 'Liter')
plot(dt[, choice_cols, with = F])

# this would fit to all variables, by the way...
# but we can't here because some variables are categorical (factors)
# fit <- lm(Price ~ ., data = dt)
fit <- lm(Price ~ Mileage + Cylinder + Liter + Doors + Cruise + Sound + Leather, data=dt)
summary(fit)

# you can do the same thing with glm (general linear model), although the p-values are calculated slightly differently
# more on the differences here: https://www.reddit.com/r/rstats/comments/2izyw1/difference_between_glm_and_lm_lmyxz_and_glmyxz/
# it will show AIC and not F-score by default
fit_glm <- glm(Price ~ Mileage + Cylinder + Liter + Doors + Cruise + Sound + Leather, data=dt)
summary(fit_glm)


termplot(fit, data=dt, partial.resid = T)





# plotting an F-distribution
# do ?df for more info
x <- seq(0.001, 5, len = 1000)
# x points, df1 (number of predictors, or p), df2 (number of points)
f_dist_800 <- df(x, 5, 800)
plot(x, f_dist_800, type = 'l')

f_dist_80 <- df(x, 5, 80)
lines(x, f_dist_80, col = 'red')

f_dist_8 <- df(x, 5, 8)
lines(x, f_dist_8, col = 'blue')

f_dist_800_10 <- df(x, 10, 800)
lines(x, f_dist_800_10, col = 'orange')

# need the car package for vif
library(car)

# anything over 4 is bad
# https://onlinecourses.science.psu.edu/stat501/node/347
# http://www.statmethods.net/stats/rdiagnostics.html
# page 102 in ISLR
vif(fit)

# corrplot plots correlations between variables
library(corrplot)
cor(dt[, nums, with = F])
cor(dt[, nums, with = F]) > 0.5
corrplot(cor(dt[, nums, with = F]))
corrplot(cor(dt[, nums, with = F]), method='number')

# look at original fit again
fit1 <- lm(Price ~ Mileage + Cylinder + Liter + Doors + Cruise + Sound + Leather, data=dt)
summary(fit1)
# liter has the highest VIF, p-value, and standard error, so let's remove it
fit2 <- lm(Price ~ Mileage + Cylinder + Doors + Cruise + Sound + Leather, data=dt)
summary(fit2)

AIC(fit1)
AIC(fit2)
BIC(fit1)
BIC(fit2)

# 6.1.2 - stepwise fit selection (page 207 ISLR)
# basically try adding or removing predictors, and pick the one with the 
# best score
# the stepAIC function is in MASS
library(MASS)

# I think this is working by removing each of the variables from the model,
# then calculating AIC and RSS.  Then I think it removes the lowest of 
# any variables that have an AIC (when removed) lower than the current AIC 
stepAIC(fit1)
fit3 <- lm(Price ~ Mileage + Cylinder + Doors + Cruise + Leather, data=dt)
summary(fit3)
AIC(fit3)  # clearly it's calculating AIC in a different way, or
# doing the algorithm different than I think it is.

# calculating 95% confidence intervals
summary(fit2)
confint(fit2, 'Mileage', level=0.95)
confint(fit2, 'Cruise', level=0.95)
# can also look at all confidence intervals.  Default is 95%
confint(fit2)

# for dummying variables
library('dummies')

str(dt)
dt$Make <- as.factor(dt$Make)
str(dt)
# you could dummy the other variables too, but there are many categories 
# for everything but make and type
make_dummies <- dummy(dt$Make, sep = "_")
str(make_dummies)
dt2 <- cbind(dt[, nums, with=F], make_dummies)
str(dt2)

fit <- lm(Price ~ ., data = dt2)
# NA for Make_Saturn, because if it isn't any other make, it's Saturn
# https://stats.stackexchange.com/questions/25804/why-would-r-return-na-as-a-lm-coefficient
# so we should always drop one of the dummies from our datatable,
# and then the base case is the dummy we dropped, i.e. it's built into the intercept
summary(fit)
stepAIC(fit)

new_fit <- lm(formula = Price ~ Mileage + Liter + Doors + Make_Buick + Make_Cadillac + 
                Make_SAAB, data = dt2)

summary(new_fit)
confint(new_fit)


new_fit <- glm(formula = Price ~ Mileage + Liter + Doors + Make_Buick + Make_Cadillac + 
                Make_SAAB + Make_Chevrolet + Make_Pontiac + Make_Saturn, data = dt2)

summary(new_fit)
confint(new_fit)

# So how might we describe this?  For every mile driven, 
# the price decreases by about $0.17 on average.
# For every unit increase in engine Liter size, the price increases
# by about $4,500 on average.
# If the make is a SAAB, the price increases by about $17,000 on average.
