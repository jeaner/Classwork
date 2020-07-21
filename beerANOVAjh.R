dt <- fread('C:/MSDS/DataAnalytics/Wk4/craft-cans/beers.csv')
#View(dt)
summary(dt)
str(dt) #way to much variety in styles for me to wrap my head around
#Choosing a vaiety of well known styles

beer<- dt[dt$style %in% c('American IPA',
                          'American Amber / Red Ale',
                          'American Porter',
                          'Fruit / Vegetable Beer',
                          'Kölsch',
                          'Witbier',
                          'American Black Ale',
                          'American Pale Ale (APA)')]
summary(beer)
beer<-na.omit(beer) #deleting the N/A's
summary(beer)
beer$style<- as.factor(beer$style) #making the style a factor
summary(beer)
View(beer)
par(mfrow=c(1,2))
boxplot(beer$abv~beer$style,las=2, main = "ABV")
# can also write boxplot(abv ~ style, data=beer)
boxplot(beer$ibu~beer$style,las=2, main = "IBU")
par(mfrow=c(1,1))
fit<- lm(ibu ~ style, data=beer)
summary(fit)
anova(fit)
summary(beer)
library(agricolae)
comp<- with(beer, waerden.test(ibu, style, alpha=0.05, group = T, console = T))
plot(comp)
#p-values from group = F
comp2<- with(beer, waerden.test(ibu, style, alpha=0.05, group = F, console = T))
plot(comp2)
