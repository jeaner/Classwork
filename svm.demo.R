library(data.table)
library(caret)
set.seed(42)  # set the random seed for reproducibility

filename <- '~/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week1/data/heart.disease.data.clean.csv'
heart.dt <- fread(filename)
str(heart.dt)
col.labels <- c('age',
                'sex',
                'chest.pain.type',
                'resting.blood.pressure',
                'cholesterol',
                'num.cigs.per.day',
                'years.as.smoker',
                'fasting.blood.sugar',
                'family.history.heart.disease',
                'resting.ecg.results',
                'max.heart.rate.exercise',
                'exercise.induced.angina',
                'blood.disorder',  # Thalassemia I think
                'heart.disease')

names(heart.dt) <- col.labels
# convert heart disease to binary outcome, 1 is heart disease
heart.dt[heart.disease > 0, heart.disease:=1]
heart.dt[, heart.disease:=as.factor(heart.disease)]
# rename factor levels to more intuitive labels
levels(heart.dt$heart.disease) <- c('no', 'yes')

str(heart.dt)

tr.idxs <- createDataPartition(heart.dt$heart.disease, p = 0.8)$Resample1
train <- heart.dt[tr.idxs]
test <- heart.dt[-tr.idxs]

# default sigma value is found with math, from sigest()
library(kernlab)
kernlab::sigest(heart.disease ~ ., train)

# unfortunately can't give 'automatic' as an option for 
# signa, have to use the result from sigest
radial.model <- train(heart.disease ~ .,
               method = 'svmRadial',
               # first sigma is from sigest
               tuneGrid = expand.grid(sigma = c(0.041, 0.01, 0.1), C = c(0.1, 1, 10)),
               data = train)
radial.model
postResample(predict(radial.model, train), train$heart.disease)
postResample(predict(radial.model, test), test$heart.disease)


radial.scaled.model <- train(heart.disease ~ .,
               method = 'svmRadial',
               preProcess = c('center', 'scale'),
               tuneGrid = expand.grid(sigma = c(0.041, 0.01, 0.1), C = c(0.1, 1, 10)),
               data = train)
radial.scaled.model
postResample(predict(radial.scaled.model, train), train$heart.disease)
postResample(predict(radial.scaled.model, test), test$heart.disease)
# scaling appears to make no difference

poly.model <- train(heart.disease ~ .,
               method = 'svmPoly',
               # scale is not really explained in the docs, guessing it's the same as gamma in e1071
               tuneGrid = expand.grid(degree = c(2, 3, 4), scale=c(0, 1, 2), C = c(0.1, 1, 10)),
               data = train)
poly.model
postResample(predict(poly.model, train), train$heart.disease)
postResample(predict(poly.model, test), test$heart.disease)
# accurcay is not great, poly seems to be overfitting quite a bit
# but does best on test set

# using the e1071 package:

# "As performance measure, the classification error is used for classification, and the mean squared error for regression."
# https://www.rdocumentation.org/packages/e1071/versions/1.6-8/topics/tune
# gamma the same as sigma, just different notation
library(e1071)
model <- tune(svm,
              heart.disease ~ .,
              data = train, 
              ranges = list(gamma = c(0.001, 0.01, 0.1),
                            cost = c(1, 5, 10)),
              tunecontrol = tune.control(sampling = "cross", cross = 3),
              kernel = 'radial'
)
# "performance" is classification error for classification
model
postResample(predict(model$best.model, train), train$heart.disease)
postResample(predict(model$best.model, test), test$heart.disease)


model <- tune(svm,
              heart.disease ~ .,
              data = train, 
              ranges = list(cost = c(1, 5, 10)),
              tunecontrol = tune.control(sampling = "cross", cross = 3),
              kernel = 'linear'
)
model
postResample(predict(model$best.model, train), train$heart.disease)
postResample(predict(model$best.model, test), test$heart.disease)

# sometimes linear works better...or we should try optimizing rbf more, because we are at the limit for gamma

model <- tune(svm,
              heart.disease ~ .,
              data = train, 
              ranges = list(gamma = c(0.1, 0.5, 1),
                            cost = c(1, 5, 10)),
              tunecontrol = tune.control(sampling = "cross", cross = 3),
              kernel = 'sigmoid'
)
model
postResample(predict(model$best.model, train), train$heart.disease)
postResample(predict(model$best.model, test), test$heart.disease)

model <- tune(svm,
              heart.disease ~ .,
              data = train, 
              ranges = list(gamma = c(0.1, 0.5, 1),
                            cost = c(1, 5, 10),
                            degree = c(2, 3, 4)),
              tunecontrol = tune.control(sampling = "cross", cross = 3),
              kernel = 'polynomial'
)
model
postResample(predict(model$best.model, train), train$heart.disease)
postResample(predict(model$best.model, test), test$heart.disease)

# kernlab does not have the gamma parameter for the polynomial kernel
library(kernlab)
model <- ksvm(heart.disease ~ .,
              data = train,
              kernel = 'polydot',  # polynomial kernel
              C = 1,
              kpar = list(degree = 3)
              )
model
postResample(predict(model, train), train$heart.disease)
postResample(predict(model, test), test$heart.disease)

# polynomial seemed to be best out of all of them
# but we may not have explored the hyperparameters enough