library(bnlearn)
library(data.table)

set.seed(11)

filename <- '/Users/EliaZ/dataML/wk3/heart.data.clean.csv'
dt <- fread(filename)
str(dt)

dt <- dt[, lapply(.SD, as.numeric)]
dt[, V1:=NULL]
str(dt)
summary(dt)

dt$sex <- sapply(dt$sex, as.factor)
dt$fbs <- sapply(dt$fbs, as.factor)
dt$famhist <- sapply(dt$famhist, as.factor)
dt$exang <- sapply(dt$exang, as.factor)
str(dt)

model <- hc(dt)
plot(model)

model$arcs
model$arcs <- model$arcs[-which((model$arcs[,'to'] == "age")),]
plot(model)

fittedbn <- bn.fit(model, data = dt)

cpquery(fittedbn, event = (num >=1), evidence = (cp >= 3) )
# 0.5792896
cpquery(fittedbn, event = (num >=1), evidence = (age > 40) )
# 0.4794059
cpquery(fittedbn, event = (num >=1), evidence = (age > 40 & cp >=3) )
# 0.5913105
cpquery(fittedbn, event = (num >=1), evidence = (chol >= 200) )
# 0.4677626
cpquery(fittedbn, event = (num >=1), evidence = (sex == 1) )
# 0.688
cpquery(fittedbn, event = (num >=1), evidence = (sex == 1 & age > 40 & cp >=3) )
# 0.7107502
cpquery(fittedbn, event = (num >=1), evidence = (famhist == 1) )
# 0.4502277
cpquery(fittedbn, event = (num >=1), evidence = (restecg >= 1) )
# 0.5027451
cpquery(fittedbn, event = (num >=1), evidence = (thalach < 85) )
# 0.8823529
cpquery(fittedbn, event = (num >=1), evidence = (thalach < 85 & age >= 50) )
# 0.9705882