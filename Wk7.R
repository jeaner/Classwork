library(data.table)
library(BSDA)
library('agricolae')
library(nortest)
library(pspearman)
flu <- fread('C:/MSDS/Meth/Wk7/DATA/new_flu_shot.csv')
View(flu)

str(flu)
flu[, new.vaccine:=as.numeric(new.vaccine)]
str(flu)
View(flu)
summary(flu)
hist(flu$new.vaccine)
hist(flu$old.vaccine)

#Mann-Whitney U
# two-sided test -- not quite significant
wilcox.test(flu$old.vaccine, flu$new.vaccine)

# one-sided -- we want the old to be greater than the drug
# looks barely significant
wilcox.test(flu$old.vaccine, flu$new.vaccine, alternative = 'greater')

car <- fread('C:/MSDS/Meth/Wk7/DATA/mpg.tsv')
summary(car)
View(car)
car[car = NA] <- NULL 
#cleans the negative values out
View(car)
summary(car)
hist(car$V1)
hist(car$V2)

ks.test(car$V1, car$V2)
# confusingly, 'greater' for ks.test means the second set of data's median is greater than the second one
# so if we want the jap car less than the US car, it's written like this:
ks.test(car$V1, car$V2, alternative = 'less')
ks.test(car$V1, 'pnorm')
hist(car$V1)
wilcox.test(car$V1, car$V2)


vocab <- fread('C:/MSDS/Meth/Wk7/DATA/vocab.csv')
summary(vocab)
View(vocab)
SIGN.test(x=vocab$before.training, y=vocab$after.training, alternative ='greater')
wilcox.test(x=vocab$before.training, y=vocab$after.training, paired=TRUE, alternative ='greater')
