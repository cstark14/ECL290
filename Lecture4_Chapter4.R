library(coda)
library(devtools)
library(loo)
library(mvtnorm)
library(dagitty) 
library(shape)
library(tidyverse)
library(rethinking)


##### 4M1 Simulate
mu <- rnorm(1000,0,10)
sigma <- rexp(1)
y <- rnorm(1000,mu,sigma)
dens(y)

##### 4M2 quap list
m2 <- alist(
  mu ~ dnorm(0,10),
  sigma ~ dexp(1),
  yi ~ dnorm(mu.sigma)
)

##### 4M3 done in Goodnotes

##### 4M4 simulations 
n <- 50
a <- rnorm(n,100,10)
b <- rnorm(n,0,10)
s <- rexp(n,1)
y <- 1:3
ybar <- mean(y)
h <- matrix(NA,nrow=n,ncol=length(y))
for(i in 1:n){
  for(j in y){
    h[i,j] <- rnorm(1,a[i]+b[i]*(y[j]-ybar),s[i])
  }
}

plot(NULL,xlim=c(1,3),ylim=range(h),xlab="year",ylab="height")
for(i in 1:n) lines(y,h[i,])


##### 4M5
#avg growth rate of middle schools is 6cm, log 6
### log-normal = exp(mu+stddev^2/2)
exp(1+1.25^2/2)
b <- rlnorm(n,1,1.25)
h2 <- matrix(NA,nrow=n,ncol=length(y))
for(i in 1:n){
  for(j in y){
    h2[i,j] <- rnorm(1,a[i]+b[i]*(y[j]-ybar),s[i])
  }
}

plot(NULL,xlim=c(1,3),ylim=range(h2),xlab="year",ylab="height")
for(i in 1:n) lines(y,h2[i,])

##### 4M6
s <- runif(n,0,8)
h3 <- matrix(NA,nrow=n,ncol=length(y))
for(i in 1:n){
  for(j in y){
    h3[i,j] <- rnorm(1,a[i]+b[i]*(y[j]-ybar),s[i])
  }
}

plot(NULL,xlim=c(1,3),ylim=range(h3),xlab="year",ylab="height")
for(i in 1:n) lines(y,h3[i,])

##### 4M7
data(Howell1); d <- Howell1; d2 <- d[ d$age >= 18 , ]
######## og model
xbar <- mean(d2$weight)
m4.3 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*( weight - xbar ) ,
    a ~ dnorm( 178 , 20 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d2 )
precis(m4.3)
round(vcov(m4.3),2)
######### modified
m4.3b <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*weight ,
    a ~ dnorm( 178 , 20 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d2 )
precis(m4.3b)
round(vcov(m4.3b),2)
round(cov2cor(vcov(m4.3b)),2)
pairs(m4.3b)

##### 4H1
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]
xbar <- mean(d2$weight)
m <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*( weight - xbar ) ,
    a ~ dnorm( 178 , 20 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d2 )
post <- extract.samples(m)
post

##### 4H2
data(Howell1)
d <- Howell1
d3 <- d[ d$age < 18 , ]
