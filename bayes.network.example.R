# adapted from here: https://www.r-bloggers.com/bayesian-network-in-r-introduction/
library(bnlearn)
library(data.table)
data(coronary)
head(coronary)
bn.dt <- as.data.table(coronary)
head(bn.dt)
str(bn.dt) #structure shows that they are all factors
bn.dt[`M. Work` == "yes" & `P. Work` == "yes"] 
?hc #hill climbing, one change at a time, if it improves if keeps the change, if not it deletes the change
model <- hc(bn.dt)
plot(model)
model
model$arcs
# if a connection doesn't make sense, we can remove it like this
# family history should not be predicted by type of work
#which is built into R, to give index in arcs
model$arcs <- model$arcs[-which((model$arcs[,'from'] == "M. Work" & model$arcs[,'to'] == "Family")),]
plot(model)
model
model$arcs
fittedbn <- bn.fit(model, data = bn.dt)
plot(fittedbn)
fittedbn

# how we can get a prediction from the model
#gives an event we want to predict then multiple pieces of evidence 

cpquery(fittedbn, event = (Smoking=="no"), evidence = (`M. Work`=="no") )
cpquery(fittedbn, event = (Smoking=="no"), evidence = (`M. Work`=="yes") )
cpquery(fittedbn, event = (Smoking=="yes"), evidence = (`M. Work`=="yes") )
cpquery(fittedbn, event = (Smoking=="yes"), evidence = (`M. Work`=="yes" & `P. Work`=="yes") )
cpquery(fittedbn, event = (Smoking=="yes"), evidence = (`P. Work`=="yes") )

#to say if the age is in a range that there will be probability
#will get slightly different numbers every time you run because it uses montycarlo sampling