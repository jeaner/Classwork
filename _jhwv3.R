
table(wv3[, V8])
hist(wv3[, V8])
hist(wv3[V8 > 0, V8])
# V6 - education level
hist(wv3[, V6])
hist(wv3[V6 > 0, V6])

hist(wv3[V8 > 0 & V6 > 0, V8])

if (!require(ggplot2)) {
  install.packages(ggplot2)
  require(ggplot2)
}

wv3_filtered <- wv3[V8 > 0 & V6 > 0]
str(wv3_filtered[, V6])
wv3_filtered <- wv3_filtered[, V6:=as.factor(V6)]
# I googled 'r histogram groupby' to get the plot working,
# and went to the sublink of the third link:
# http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/#histogram-and-density-plots

# I googled 'ggplot change colormap' to figure out how
# to change the colors
ggplot(data=wv3_filtered, aes(x=V8, colour=V6)) + 
  geom_density() + scale_colour_brewer(palette = "RdYlBu")

wv3_filtered <- wv3[V8 > 0 & V6 > 0] #Filter out missing values
ggplot(data=wv3_filtered, aes(x=V6, fill=V8)) + 
  geom_histogram(aes(y=0.5*..density..), binwidth=0.5) +
  facet_wrap(~V8, nrow=4)

wv3_filtered <- wv3_filtered[, V6:=as.factor(V6)]
wv3_filtered <- wv3_filtered[, V8:=as.factor(V8)]
ggplot(wv3_filtered, aes(V8, V6)) + geom_bin2d(bins=c(4,9))

fwrite(wv3, paste(filepath, 'wave3.csv', sep=''))

                                                                                                               