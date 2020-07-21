set.seed(1)
x<-matrix(rnorm(200*2), ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y<-c(rep(1,150),rep(2,50))
dat<-data.frame(x=x,y=as.factor(y))
plot(x, col=y)