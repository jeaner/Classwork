pick.site <- function(epsilon, decay = 0.9) {
  labels <- c('a', 'b', 'c')
  a.prob <- 0.028
  b.prob <- 0.032
  c.prob <- 0.026
  probs <- c(a.prob, b.prob, c.prob)
  if (runif(1) < epsilon) {
    print('random choice')
    idx <- sample(1:3, 1)
  } else {
    idx <- which.max(probs)
  }
  print('choosing')
  print(labels[idx])
  return(epsilon * decay)
}

epsilon = 1
for (i in 1:100) {
  epsilon <- pick.site(epsilon)
  print(epsilon)
}
