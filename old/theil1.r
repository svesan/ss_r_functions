theil <- function(dfr,x,y,alpha=0.05) {
n <- NROW(dfr)
q <- matrix(ncol=1, nrow=n*(n-1)/2)
k <- 0
for (i in 1:(n-1)){
  for (j in (i+1):n){
    k <- k+1
    q[k] <- (dfr$y[j]-dfr$y[i])/(dfr$x[j]-dfr$x[i])
  }
}

#-- Confidence interval for beta
z <- qnorm(1-alpha/2)
k <- n*(n-1)/4 - z*sqrt(n*(n-1)*(2*n+5) / 72)
low <- q[k+1]
upp <- q[n*(n-1)/2 - k]
icept<-median(dfr$y-median(q)*dfr$x)
c(icept,median(q),low,upp)
}

x <- 1:20
y <- 1+2*x+rnorm(x)
d<-data.frame(x,y)
e<-theil(d,x,y)

glm(y~x,data=d)
cor.test(d$x,d$y,method="kendall")