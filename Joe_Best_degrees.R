library(data.table)
filepath = '/Users/josephseverino/Downloads'
filename = 'degrees-that-pay-back.csv'
# this is how we read in a .rdata file
best_degrees <- paste(filepath, filename, sep='/')
dt_best_degrees <- fread(best_degrees)
dim(dt_best_degrees)
str(dt_best_degrees)
View(dt_best_degrees)

#convert all salaries from characters to numbers
dt_best_degrees$Salary <- as.numeric(gsub('[$,]', '', dt_best_degrees$Salary))
#dt_best_degrees$`Mid-Career 25th Percentile Salary` <- as.numeric(gsub('[$,]', '', dt_best_degrees$`Mid-Career 25th Percentile Salary`))
#dt_best_degrees$`Mid-Career 75th Percentile Salary` <- as.numeric(gsub('[$,]', '', dt_best_degrees$`Mid-Career 75th Percentile Salary`))
#dt_best_degrees$`Mid-Career 90th Percentile Salary` <- as.numeric(gsub('[$,]', '', dt_best_degrees$`Mid-Career 90th Percentile Salary`))

for(i in 1:dim(dt_best_degrees)[1]){
  if(dt_best_degrees$`Undergraduate Major`[i] == 'Architecture' |
     dt_best_degrees$`Undergraduate Major`[i] == 'Art History' |
     dt_best_degrees$`Undergraduate Major`[i] == 'Drama'|
     dt_best_degrees$`Undergraduate Major`[i] == 'Education' |
     dt_best_degrees$`Undergraduate Major`[i] == 'English' |
     dt_best_degrees$`Undergraduate Major`[i] == 'Film' |
     dt_best_degrees$`Undergraduate Major`[i] == 'Graphic Design' |
     dt_best_degrees$`Undergraduate Major`[i] == 'History' |
     dt_best_degrees$`Undergraduate Major`[i] == 'Interior Design' |
     dt_best_degrees$`Undergraduate Major`[i] == 'International Relations' |
     dt_best_degrees$`Undergraduate Major`[i] == 'Journalism' |
     dt_best_degrees$`Undergraduate Major`[i] == 'Music' |
     dt_best_degrees$`Undergraduate Major`[i] == 'Philosophy' |
     dt_best_degrees$`Undergraduate Major`[i] == 'Political Science' |
     dt_best_degrees$`Undergraduate Major`[i] == 'Religion' |
     dt_best_degrees$`Undergraduate Major`[i] == 'Spanish' |
     dt_best_degrees$`Undergraduate Major`[i] == 'Anthropology' |
     dt_best_degrees$`Undergraduate Major`[i] == 'Sociology' |
     dt_best_degrees$`Undergraduate Major`[i] == 'Psychology' |
     dt_best_degrees$`Undergraduate Major`[i] == 'Geography'){
    #Category of "liberal Arts majors
    dt_best_degrees$Major[i] <- "Lib_Arts"
  }else if(dt_best_degrees$`Undergraduate Major`[i] == 'Aerospace Engineering' |
           dt_best_degrees$`Undergraduate Major`[i] == 'Chemical Engineering' |
           dt_best_degrees$`Undergraduate Major`[i] == 'Civil Engineering' |
           dt_best_degrees$`Undergraduate Major`[i] == 'Computer Engineering' |
           dt_best_degrees$`Undergraduate Major`[i] == 'Computer Science' |
           dt_best_degrees$`Undergraduate Major`[i] == 'Electrical Engineering' |
           dt_best_degrees$`Undergraduate Major`[i] == 'Industrial Engineering' |
           dt_best_degrees$`Undergraduate Major`[i] == 'Information Technology' |
           dt_best_degrees$`Undergraduate Major`[i] == 'Mechanical Engineering'){
    #Category of Eng&Tech
    dt_best_degrees$Major[i] <- 'Eng&Tech'
  }else if(dt_best_degrees$`Undergraduate Major`[i] == 'Accounting' |
           dt_best_degrees$`Undergraduate Major`[i] == 'Business Management' |
           dt_best_degrees$`Undergraduate Major`[i] == 'Communications' |
           dt_best_degrees$`Undergraduate Major`[i] == 'Economics' |
           dt_best_degrees$`Undergraduate Major`[i] == 'Finance' |
           dt_best_degrees$`Undergraduate Major`[i] == 'Hospitality & Tourism' |
           dt_best_degrees$`Undergraduate Major`[i] == 'Marketing' |
           dt_best_degrees$`Undergraduate Major`[i] == 'Management Information Systems (MIS)' ){
    #Category of Business
    dt_best_degrees$Major[i] <- 'Biz'
  }else if(dt_best_degrees$`Undergraduate Major`[i] == 'Biology' |
           dt_best_degrees$`Undergraduate Major`[i] == 'Chemistry' |
           dt_best_degrees$`Undergraduate Major`[i] == 'Geology' |
           dt_best_degrees$`Undergraduate Major`[i] == 'Math' |
           dt_best_degrees$`Undergraduate Major`[i] == 'Physics' |
           dt_best_degrees$`Undergraduate Major`[i] == 'Agriculture' ){
    #Science and Math
    dt_best_degrees$Major[i] <- 'Science&Math'
  }else if(dt_best_degrees$`Undergraduate Major`[i] == 'Health Care Administration' |
           dt_best_degrees$`Undergraduate Major`[i] == 'Nursing' |
           dt_best_degrees$`Undergraduate Major`[i] == 'Nutrition' |
           dt_best_degrees$`Undergraduate Major`[i] == 'Physician Assistant'){
    #Healthcare
    dt_best_degrees$Major[i] <- 'Health'
  }else {
    dt_best_degrees$Major[i] <- 'Misc'
  }
}

dt_best_degrees <- dt_best_degrees[,Major:=as.factor(Major)]
#dt_best_degrees$top_pay <- dt_best_degrees[,`Mid-Career 90th Percentile Salary`]
#dt_best_degrees$bottom_pay <- dt_best_degrees[,`Mid-Career 10th Percentile Salary`]
install.packages('ggpubr')
library(ggpubr)
ggboxplot(dt_best_degrees, x = "Performance", y = 'Salary', color ='Major')

model <- aov(Salary ~ Major + Performance + Major * Performance, data = dt_best_degrees)
summary(model)

# the main factors are still included, even if we only specify interactions
fit <- aov(Salary ~ Major * Performance, data = dt_best_degrees)
summary(fit)

par(mfrow = c(2,2))
plot(fit)
par(mfrow = c(1,1))

shapiro.test(residuals(fit))
hist(residuals(fit))
dt_best_degrees <- dt_best_degrees[,Performance:=as.factor(Performance)]
dt_best_degrees <- dt_best_degrees[,Major:=as.factor(Major)]
interaction.plot(x.factor = dt_best_degrees$Performance,
                 trace.factor = dt_best_degrees$Major, 
                 response = dt_best_degrees$Salary,
                 fun = mean, 
                 type = "b",  # shows each point
                 main = "Interaction Plot",
                 legend = TRUE,
                 trace.label = "TV Freq Group",
                 xlab = "Performance Level",
                 ylab="Salary",
                 pch=c(1, 2, 3, 4),
                 col = c("#76448a", "#1f618d", "#148f77"))
TukeyHSD(fit, conf.level = 0.05)
