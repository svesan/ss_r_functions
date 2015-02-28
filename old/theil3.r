theil <- function(x, y, alpha=0.05, data=NULL) {

#  Name..: theil
#  AUthor: sven.sandin@meb.ki.se
#  Date..: 2001-05-14
#
#  06Jul2004
#  1) Added header and example
#  2) Replaced function parameter DFR with the DATA=
#  3) Added checks for the function parameters
#  
#  
#  Example:
#  --------
#  x <- 1:20
#  y <- 1+2*x+rnorm(x)
#  d<-data.frame(x,y)
#  e<-theil(d,x,y)
#  
#  glm(y~x,data=d)
#  cor.test(d$x,d$y,method="kendall")

#-- Logical checks
if (class(data) != "data.frame") stop('Parameter data not a valid data frame')
if (alpha<=0 | alpha>=1) stop('alpha must be between 0 and 1')


#-- Start the calculations
n <- NROW(data)
q <- matrix(ncol=1, nrow=n*(n-1)/2)
k <- 0
for (i in 1:(n-1)){
  for (j in (i+1):n){
    k <- k+1
    q[k] <- (data$y[j]-data$y[i])/(data$x[j]-data$x[i])
  }
}

#-- Confidence interval for beta
z <- qnorm(1-alpha/2)
k <- n*(n-1)/4 - z*sqrt(n*(n-1)*(2*n+5) / 72)
low <- q[k+1]
upp <- q[n*(n-1)/2 - k]
icept<-median(data$y-median(q)*data$x)
c(icept,median(q),low,upp)
}

