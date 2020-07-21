library(tm)
library(data.table)
file_loc <- "~/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week3/spam.csv"
dt <- fread(file_loc)
labels <- dt$v1
dt[, c('v1', 'V3', 'V4', 'V5'):=NULL]
dt[, doc_id:=seq(1:nrow(dt))]
names(dt) <- c('text', 'doc_id')
setcolorder(dt, c('doc_id', 'text'))
dt[, text:=unlist(lapply(text, function(x) {iconv(x, "latin1", "ASCII", sub="")}))]  # convert non-ascii characters which cause problems
str(dt)
corp <- VCorpus(DataframeSource(dt))
dtm <- DocumentTermMatrix(corp, control = list(weighting = weightTfIdf, stopwords = T, stemming = T, wordLengths = c(2, 13)))
inspect(dtm)

# highly unbalanced dataset
barplot(table(labels))
dtm.matrix <- as.matrix(dtm)
filename <- '~/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week3/dtm.csv'
write.csv(dtm.matrix, filename)
filename <- '~/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week3/dtm.data'
save(dtm.matrix, file = filename)
dtm