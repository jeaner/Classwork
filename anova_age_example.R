library(data.table)

filename <- 'C:/MSDS/Meth/wk4/week4/eysenck_age_and_memory.txt'

dt <- fread(filename)
dim(dt)
str(dt)
summary(dt)

boxplot(Words ~ Age, data = dt)

hist(dt$Words)

boxplot(Words ~ Process, data = dt)

fit <- lm(Words ~ Age, data = dt)

plot(fit)

summary(fit)

anova(fit)

# another way to do the same thing...
anv <- aov(Words ~ Age, data = dt)
summary(anv)


# data from https://www.gwern.net/docs/statistics/bias/1981-lagakos.pdf
# RE means reticuloendothelial: https://en.wikipedia.org/wiki/Mononuclear_phagocyte_system
fn2 <- 'C:/MSDS/Meth/wk4/week4/red_40_expts.csv'
dt2 <- fread(fn2)
dim(dt2)
str(dt2)
summary(dt2)
dt2$red40_diet_pct <- as.factor(dt2$red40_diet_pct)
str(dt2)

boxplot(weeks ~ red40_diet_pct, data = dt2)

fit <- lm(weeks ~ dose, data = dt2)
plot(fit)

summary(fit)
anova(fit)

anv2 <- aov(weeks ~ dose, data = dt2)


# this package would not install easily for me.  You're free to give it a try if you want
# https://www.rdocumentation.org/packages/userfriendlyscience/versions/0.6-1/topics/oneway
install.packages('userfriendlyscience')

library(userfriendlyscience)
ow <- oneway(dt$Words, dt$Age, posthoc = 'games-howell')
# another library for post-hoc tests: https://www.rdocumentation.org/packages/agricolae/versions/1.2-8/topics/LSD.test
