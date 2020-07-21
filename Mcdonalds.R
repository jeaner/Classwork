library(data.table)
library(BSDA)
library('agricolae')
library(nortest)
library(pspearman)
mcd <- fread('C:/MSDS/Meth/Wk7/DATA/menu.csv')
View(mcd)
summary(mcd)
str(mcd)

#Different categories sodium levels - Kruskal-Wallis test
levels(mcd$Category)
mcd$Category <- factor(mcd$Category)
levels(mcd$Category)
str(mcd)
plot(mcd$Category)
plot(mcd$Sodium~mcd$Category)
sod <- mcd[,.(Category, Sodium)]
View(sod)
str(sod)
plot(sod)
sod[,Category:=as.numeric(Category)]
kruskal.test(sod)
krus<- with(sod, kruskal(Sodium, trt = Category, console = T))
plot(krus)
with(sod, kruskal(Sodium, trt = Category, group=F, console = T))


#Is Breakfast sodium from normal distribution?
bfast <- sod[Category == "Breakfast"]
View(bfast)
summary(bfast)
hist(bfast$Sodium)
bsod <- bfast[, list(Sodium)]
View(bsod)
bsod2<-bsod[, Sodium:=as.numeric(Sodium)]
str(bsod2)
shapiro.test(bsod2$Sodium)
qqnorm(bsod$Sodium)
qqline(bsod$Sodium)

#items correlate to the calories? :
# 'Carbohydrates', 'Sugars', 'Protein', 'Total Fat'
str(mcd)
cal <- mcd[, Calories:=as.numeric(Calories)]
cal <- mcd[, Carbohydrates:=as.numeric(Carbohydrates)]
cal <- mcd[, Sugars:=as.numeric(Sugars)]
  cal <- mcd[, Protein:=as.numeric(Protein)]
    cal <- mcd[, `Total Fat`:=as.numeric(`Total Fat`)]
str(cal)
model <- aov(cal$Calories ~ cal$Carbohydrates + cal$Sugars + cal$Protein + cal$`Total Fat`, data=cal)
summary(model)
plot(model)
           