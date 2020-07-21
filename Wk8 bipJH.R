#Author: Jeanette & Bip

pick.site <-function(epsilon){
  sites<-c('a','b','c')
  random.number<- runif(1,0,1)
  if (random.number<epsilon) 
  {return(list(sample(sites,1), epsilon))} #returns a random site if the random number is less than epsilon
  else
  {return(list(sites=site[2],   epsilon))} #otherwise returns second site in list, so site b

}
epsilon <- 1
for (i in 1:100) {
  epsilon<-epsilon * 0.99
  result <- pick.site(epsilon)
  print(result)
}


#Given probabilities
#a.pb <- 0.028
#b.pb <- 0.032
#c.pb <- 0.026
#site.prob<- c(a.pb, b.pb, c.pb)

pick.site <-function(epsilon){
  a.pb <- 0.028
  b.pb <- 0.032
  c.pb <- 0.026
  
  sites <-    c('a', 'b',  'c')
  site.prob<- c(a.pb, b.pb, c.pb)
  maxsite<- which.max(site.prob)
  
  random.number<- runif(1,0,1)
  if (random.number<epsilon) 
  {return(list(sample(sites,1), epsilon))} #returns a random site if the random number is less than epsilon
  else
  {return(list(sites[maxsite], epsilon))} #otherwise returns site with max probability
  
}
epsilon <- 1
for (i in 1:100) {
  epsilon<-epsilon * 0.99
  result <- pick.site(epsilon)
  print(result)
}
