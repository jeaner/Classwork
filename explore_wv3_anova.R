# highlight a section and press ctrl+enter to run,
# or click run above.  Always better to use
# keyboard shortcuts if they're faster

# if you forgot, this will check if the 
# data.table package is installed
# If not, it'll install it
if (!require(data.table)) {
  install.packages('data.table')
  require(data.table)
}

filepath = 'C:/MSDS/Meth/Wk1/'
filename = 'WV3_Data_x_v_2015-04-18.rdata'
# this is how we read in a .rdata file
load(paste(filepath, filename, sep=''))
# this converts to a data.table, which is easier
# to work with than other formats in R
typeof(`WV3_Data_R_v_2015-04-18`)
# the backticks are necessary because of the - I think
wv3 = as.data.table(`WV3_Data_R_v_2015-04-18`)

# shows number of rows and columns, always good
# to check first
dim(wv3)

# check the .sts file to see column definitions
# duration of interview is V230

# google 'data.table cheat sheet' for a quick
# summary of how to manipulate data.tables
# basically, we do dt[row, columns, groupby]
hist(wv3[, V230])
print(max(wv3[, V230]))
wv3[V230 > 500, V230]  # looks like errors, right?
wv3[V230 < 500, V230]
sort(table(wv3[, V230]), decreasing=T)
dim(wv3)
wv3_duration <- wv3[V230 < 100, ]
wv3_duration <- wv3_duration[V230 > 10, ]
hist(wv3_duration[, V230])
dim(wv3_duration)

# hypothesis: higher education means more open minded,
# so higher educated people more strongly disagree with
# "A university education is more important for a boy than for a girl"
# V103
# 1 = strongly agree; 4 = strongly disagree
table(wv3[, V103])
hist(wv3[, V103])
hist(wv3[V103 > 0, V103])
# V217 - education level
hist(wv3[, V217])
hist(wv3[V217 > 0, V217])

hist(wv3[V103 > 0 & V217 > 0, V103])

if (!require(ggplot2)) {
  install.packages(ggplot2)
  require(ggplot2)
}

wv3_filtered <- wv3[V103 > 0 & V217 > 0]
str(wv3_filtered[, V217])
wv3_filtered <- wv3_filtered[, V217:=as.factor(V217)]
# I googled 'r histogram groupby' to get the plot working,
# and went to the sublink of the third link:
# http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/#histogram-and-density-plots

# I googled 'ggplot change colormap' to figure out how
# to change the colors
ggplot(data=wv3_filtered, aes(x=V103, colour=V217)) + 
  geom_density() + scale_colour_brewer(palette = "RdYlBu")

# here's another idea for plotting, might be more useful for less factors:
# https://stackoverflow.com/questions/22181132/normalizing-y-axis-in-histograms-in-r-ggplot-to-proportion-by-group
# need to reset V217 column so its not a factor
wv3_filtered <- wv3[V103 > 0 & V217 > 0] #Filter out missing values
ggplot(data=wv3_filtered, aes(x=V217, fill=V103)) + 
  geom_histogram(aes(y=0.5*..density..), binwidth=0.5) +
  facet_wrap(~V103, nrow=4)

# how to make a heatmap -- not very useful here
wv3_filtered <- wv3_filtered[, V217:=as.factor(V217)]
wv3_filtered <- wv3_filtered[, V103:=as.factor(V103)]
ggplot(wv3_filtered, aes(V103, V217)) + geom_bin2d(bins=c(4,9))

# save the data so we can read it in Python more easily
fwrite(wv3, paste(filepath, 'wave3.csv', sep=''))

                                                                                                               