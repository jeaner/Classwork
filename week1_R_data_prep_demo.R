versilibrary(data.table)

fn <- '/home/nate/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week1/auto-mpg.data'

# as.is leaves characters as characters instead of converting to factors, so we can substitute NA for ?
df <- read.table(fn, as.is = T)
auto.dt <- as.data.table(df)
str(auto.dt)

fn <- '/home/nate/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week1/auto-mpg.names'

auto.names <- readLines(fn)
auto.names

# gets the name from the list of names using regular expressions
get_name <- function(x) gsub('\\s+\\d+\\.\\s+(.+):\\s+.+', '\\1', x)
short.names <- unlist(lapply(auto.names, FUN = get_name))
short.names <- unlist(lapply(short.names, FUN = function(x) gsub('\\s', '.', x)))
short.names

names(auto.dt) <- short.names
auto.dt[auto.dt == '?'] <- NA
str(auto.dt)
auto.dt[, horsepower:=as.numeric(horsepower)]
auto.dt[, car.name:=NULL]
str(auto.dt)
# a few na's in horsepower -- we can impute that fairly easily with KNN
summary(auto.dt)

# https://datascienceplus.com/imputing-missing-data-with-r-mice-package/
# library(mice)
library(DMwR)
# vim package also has knn imputation:
# library(VIM)

auto.dt.nona <- knnImputation(auto.dt)
# save for later use
fwrite(auto.dt.nona, '~/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week1/auto.dt.nona.csv')
# if you're curious, see what the nas were replaced with
auto.dt.nona[is.na(auto.dt$horsepower)]$horsepower

library(corrplot)
corrplot(cor(auto.dt.nona))

dt.names <- names(auto.dt.nona)
for (i in seq(dim(auto.dt)[2])) {
  coldata <- auto.dt.nona[, get(dt.names[i])]
  n.levels <- nlevels(as.factor(coldata))
  if (n.levels <= 10) {
    barplot(table(coldata), xlab = dt.names[i])
  } else {
    hist(coldata, main = NULL, xlab = dt.names[i])
  }
}
