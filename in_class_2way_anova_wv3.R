dim(wv3)
str(wv3)

hist(wv3$V228,
     prob=TRUE,
     main = "Histogram of Cancer Cases",
     xlab = "Number of cases",
     ylab = "Density",
     col="#76448a")
lines(density(wv3$V228), col="#1f618d", lwd=2) # add a density estimate with defaults
lines(density(wv3$V228, adjust=2), lty="dotted", col="#148f77", lwd=2)

boxplot(V228 ~ V65, data = wv3)
boxplot(V228 ~ V228, data = wv3)
boxplot(V228 ~ V227, data = wv3)

wv3_cln=wv3_cln[,V228:=as.factor(V228)]
wv3_cln=wv3_cln[,V227:=as.factor(V227)]
str(wv3_cln)
# need at least R version 3.3 for ggpubr, and it takes a while to install
#install.packages('ggpubr')
#library(ggpubr)
#ggboxplot(wv3_cln, x = "V65", y = "V228", color = "V227")

# let's throw out that outlier of 16 cases

#wv3_cln <- as.data.table(wv3)
#wv3_cln <- wv3_cln[V228 < 15,]


boxplot(V228 ~ V65, data = wv3_cln)
# check box plot again
#ggboxplot(wv3_cln, x = "V65", y = "V228", color = "red")
numeric_cols <- c('V65', 'V227', 'V228')
numeric_cols
wv3_new <- wv3_cln[, numeric_cols, with=F][][sample(.N,5000)]
#taking a smaller sample size
wv3_new

fit <- aov(V65 ~ V228 * V227, data = wv3_new)
summary(fit)

# the main factors are still included, even if we only specify interactions
#fit <- aov(V228 ~ V65 * V227, data = wv3_cln)
#summary(fit)

#check the fit!
par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))

# null is that the distribution is normal, alternative is that it's not normal
shapiro.test(residuals(fit))

hist(residuals(fit))

shapiro.test(wv3_new$V228)
hist(wv3_cln$V228)

interaction.plot(x.factor = wv3_new$V227,
                 trace.factor = wv3_new$V228, 
                 response = wv3_new$V65,
                 fun = mean, 
                 type = "b",  # shows each point
                 main = "Interaction Plot",
                 legend = TRUE,
                 trace.label = "TV",
                 xlab = "Income",
                 ylab="Life Sat.",
                 #pch=c(1, 2, 3, 4),
                 col = c("#76448a", "#1f618d", "#148f77"))

TukeyHSD(fit, conf.level = 0.05)

# install.packages('car')
library(car)
#Checking fit
leveneTest(wv3_new$V65 ~ wv3_new$V227)
leveneTest(wv3_new$V65 ~ wv3_new$V228)

summary(lm(abs(model$res) ~ wv3_new$V228))
summary(lm(abs(model$res) ~ wv3_new$V227))
par(mfrow = c(2,2))
plot(lm(abs(model$res) ~ wv3_new$V65))
plot(lm(abs(model$res) ~ wv3_new$V227))
par(mfrow = c(1,1))
