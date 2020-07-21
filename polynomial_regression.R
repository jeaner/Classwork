str(cars)

with (cars,
  plot(speed, dist)
)

linear <- lm(dist ~ speed, data = cars)
plot(linear)

plot(cars$speed, cars$dist)
abline(linear, col = 'red')
summary(linear)


par(mfrow=c(2, 2))
plot(linear)

# can also do it like this
poly <- lm(dist ~ poly(speed, 2), data = cars)
summary(poly)
poly <- lm(dist ~ I(speed^2), data = cars)

library(MASS)
summary(poly)
par(mfrow=c(1, 1))
plot(cars$speed, cars$dist)
lines(sort(cars$speed), fitted(poly)[order(cars$speed)], col='red')

par(mfrow=c(2, 2))
plot(poly)

poly2 <- lm(dist ~ speed + I(speed^2), data = cars)
summary(poly2)
plot(poly2)

AIC(linear)
AIC(poly)
AIC(poly2)

poly3 <- lm(dist ~ speed + I(speed^2) + I(speed^3), data = cars)
summary(poly3)

# stepAIC doesn't seem to find the best model here, seems like dist ~ speed^2 is best
library(MASS)
stepAIC(poly3)

poly4 <- lm(formula = dist ~ speed + I(speed^3), data = cars)
summary(poly4)
AIC(poly4)
