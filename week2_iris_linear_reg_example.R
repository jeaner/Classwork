# checking out an overview of the dataset
dim(iris)
str(iris)
summary(iris)

# plots everything vs everything
plot(iris)

# plots x, y
plot(iris$Petal.Length, iris$Petal.Width)


# fits a linear model to the data in the form y ~ x
fit <- lm(iris$Petal.Width ~ iris$Petal.Length)

# adds the regression line to the plot
abline(fit, col='red')

# plots the diagnostics of the fit
par(mfrow=c(2,2)) # Change the panel layout to 2 x 2
plot(fit)
par(mfrow=c(1,1)) # Change back to 1 x 1
