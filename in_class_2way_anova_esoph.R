dim(esoph)
str(esoph)

hist(esoph$ncases,
     prob=TRUE,
     main = "Histogram of Cancer Cases",
     xlab = "Number of cases",
     ylab = "Density",
     col="#76448a")
lines(density(esoph$ncases), col="#1f618d", lwd=2) # add a density estimate with defaults
lines(density(esoph$ncases, adjust=2), lty="dotted", col="#148f77", lwd=2)

boxplot(ncases ~ agegp, data = esoph)
boxplot(ncases ~ alcgp, data = esoph)
boxplot(ncases ~ tobgp, data = esoph)

# need at least R version 3.3 for ggpubr, and it takes a while to install
# install.packages('ggpubr')
library(ggpubr)
ggboxplot(esoph, x = "agegp", y = "ncases", color = "tobgp")

# let's throw out that outlier of 16 cases

esoph_cln <- as.data.table(esoph)
esoph_cln <- esoph_cln[ncases < 15,]

# check box plot again
ggboxplot(esoph_cln, x = "agegp", y = "ncases", color = "tobgp")


model <- aov(ncases ~ agegp + tobgp + agegp * tobgp, data = esoph_cln)
summary(model)

# the main factors are still included, even if we only specify interactions
fit <- aov(ncases ~ agegp * tobgp, data = esoph_cln)
summary(fit)

par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))

# null is that the distribution is normal, alternative is that it's not normal
shapiro.test(residuals(fit))
hist(residuals(fit))

shapiro.test(esoph_cln$ncases)
hist(esoph_cln$ncases)

interaction.plot(x.factor = esoph_cln$agegp,
                 trace.factor = esoph_cln$tobgp, 
                 response = esoph_cln$ncases,
                 fun = mean, 
                 type = "b",  # shows each point
                 main = "Interaction Plot",
                 legend = TRUE,
                 trace.label = "Tobacco Group",
                 xlab = "Age Group",
                 ylab="Number of cancer cases",
                 #pch=c(1, 2, 3, 4),
                 col = c("#76448a", "#1f618d", "#148f77"))

TukeyHSD(fit, conf.level = 0.05)

# install.packages('car')
library(car)
leveneTest(esoph_cln$ncases ~ esoph_cln$agegp)
leveneTest(esoph_cln$ncases ~ esoph_cln$tobgp)

summary(lm(abs(model$res) ~ esoph_cln$agegp))
summary(lm(abs(model$res) ~ esoph_cln$tobgp))
par(mfrow = c(2,2))
plot(lm(abs(model$res) ~ esoph_cln$agegp))
plot(lm(abs(model$res) ~ esoph_cln$tobgp))
par(mfrow = c(1,1))

