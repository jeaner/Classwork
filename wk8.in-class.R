pick.site <- function(epsilon) {
  sites <- c('a', 'b', 'c')
  random.number <- runif(1)
  if (random.number < epsilon) {
    return(list(sample(sites, 1), epsilon))
  } else {
    (return(list(sites[2], epsilon)))
  }
}

epsilon <- 1
for (i in 1:1000) {
  epsilon <- epsilon * 0.99
  result <- pick.site(epsilon)
  print(result)
}