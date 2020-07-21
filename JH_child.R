dim(infert)
str(infert)
summary(infert)
hist(infert$parity)

boxplot(parity ~ age, data = infert)
boxplot(parity ~ spontaneous, data = infert)
boxplot(parity ~ induced, data = infert)


infert1 <- as.data.table(infert)
infert1 <- infert1[age < 41]
infert1 <- infert1[age > 26]
infert1 <- infert1[parity < 5]
boxplot(parity ~ age, data = infert1)

fit <- aov(parity ~ age * induced, data = infert1)
summary(fit)

par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))

# null is that the distribution is normal, alternative is that it's not normal
shapiro.test(residuals(fit))
hist(residuals(fit))

shapiro.test(infert1$parity)
hist(infert1$parity)

interaction.plot(x.factor = infert1$age,
                 trace.factor = infert1$induced, 
                 response = infert1$parity,
                 fun = mean, 
                 type = "b",  # shows each point
                 main = "Interaction Plot",
                 legend = TRUE)


library(car)

leveneTest(infert1$parity ~ infert1$induced)

summary(lm(abs(model$res) ~ infert1$age))
summary(lm(abs(model$res) ~ infert1$induced))
par(mfrow = c(2,2))
plot(lm(abs(model$res) ~ infert1$age))
plot(lm(abs(model$res) ~ infert1$induced))
par(mfrow = c(1,1))

