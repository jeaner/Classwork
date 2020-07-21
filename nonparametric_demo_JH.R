library(data.table)
library(BSDA)

# comparing if a sample is different from a median value
c(rep(10, 10), rep(30, 30))  # the sample looks like this
# see if it's significantly different from a median of 20
SIGN.test(x = c(rep(10, 10), rep(30, 30)),  md = 20)
SIGN.test(x = c(rep(1, 10), rep(40, 30)),  md = 40)

# autism medication to reduce repetetive behaviors, like programming in R too much
aut <- fread('C:/MSDS/Meth/Wk7/DATA/autism_medication.csv')
View(aut)

# see if the two groups differ significantly
SIGN.test(x = aut$Before.Treatment, y = aut$After.Treatment)

# what we actually want to see is a reduction in the repetetive behaviors, so we should do a one-sided test
# we want the first group to be greater than the second group
SIGN.test(x = aut$Before.Treatment, y = aut$After.Treatment, alternative = 'greater')



# Wilcoxon signed-rank test
# takes into account magnitude of differences, looks significant now
wilcox.test(x = aut$Before.Treatment, y = aut$After.Treatment, paired = T, alternative = 'greater')


# Mann-Whitney U
# placebo vs drug to treat shortness of breath
# we want to see a decrease in symptoms with the drug
placebo <- fread('C:/MSDS/Meth/Wk7/DATA/placebo_new_drug.csv')
View(placebo)

# two-sided test -- not quite significant
wilcox.test(placebo$Placebo, placebo$New.Drug)

# one-sided -- we want the placebo to be greater than the drug
# looks barely significant
wilcox.test(placebo$Placebo, placebo$New.Drug, alternative = 'greater')


# Kruskal-Wallis
# amount of albumin in the blood with different amounst of protein in diet
# albumin can be used to gauge if enough protein is in the diet
alb <- fread('C:/MSDS/Meth/Wk7/DATA/albumin.csv')
View(alb)
str(alb)
alb[, pct.protein.5:=as.numeric(pct.protein.5)]
str(alb)
# reject the null because p < 0.05
# so some of the groups have different medians
kruskal.test(alb)

# complicated reshaping to work with agricolae
alb$temp <- 0
# temp column to allow reshape
# convert column names into row values
alb <- melt(alb, id.vars = 4, variable.name = 'protein', value.name = 'albumin')
alb <- alb[complete.cases(alb), ] # drop NAs
alb$temp <- NULL  # drop temp column that allowed for reshape
alb
str(alb)
library('agricolae')
krus <- with(alb, kruskal(albumin, trt = protein, console = T))
plot(krus)


# Kolmogorov-Smirnov
ks.test(placebo$Placebo, placebo$New.Drug)
# confusingly, 'greater' for ks.test means the second set of data's median is greater than the second one
# so if we want the new drug less than the placebo, it's written like this:
ks.test(placebo$Placebo, placebo$New.Drug, alternative = 'less')
ks.test(placebo$Placebo, 'pnorm')
hist(placebo$Placebo)

#Anderson-Darling test
library(nortest)
ad.test(placebo$Placebo)
ad.test(aut$Before.Treatment)


# spearman's
cor(aut[, c('Before.Treatment', 'After.Treatment'), with=F], method = 'spearman')
library(pspearman)
# rho is the spearman coefficient.  the p-value is used to check if there is a strong 
# monotonic relationship between the two variables
spearman.test(aut$Before.Treatment, aut$After.Treatment)
