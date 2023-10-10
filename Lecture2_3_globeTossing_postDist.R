library(coda)
library(devtools)
library(loo)
library(mvtnorm)
library(dagitty) 
library(shape)
library(tidyverse)
library(rethinking)

###sample <- c("W","L","W","W","W","L","W","L","W")
compute_posterior_sampleList <- function(the_sample,poss=c(0,0.25,0.5,0.75,1)){
  W <- sum(sample=="W") # number of W observed
  L <- sum(sample=="L") # number of L observed
  p <- c(0,0.25,0.5,0.75,1) # proportions W
  ways <- sapply( p , function(q) (q*4)^W * ((1-q)*4)^L )
  #prob <- ways/sum(ways)
  post <- ways/sum(ways)
  bars <- sapply(post,function(q) make_bar(q))
  #data.frame(poss,ways,post=round(post,3))
  data.frame(poss,ways,post=round(post,3),bars)
  #cbind( p , ways , prob )
}

compute_posterior <- function(sumW,sumL,poss=c(0,0.25,0.5,0.75,1)){
  ### assuming prior here is a uniform flat line
  ways <- sapply( poss , function(q) (q*4)^sumW * ((1-q)*4)^sumL )
  post <- ways/sum(ways)
  #bars <- sapply(post,function(q) make_bar(q))
  data.frame(poss,ways,post=round(post,3))
  #data.frame(poss,ways,post=round(post,3),bars)
}


# function to toss a globe covered p by water N times
sim_globe <- function( p=0.7 , N=9 ) {
  sample(c("W","L"),size=N,prob=c(p,1-p),replace=TRUE)
}

replicate(sim_globe(p=0.5,N=9),n=10)

post_samples <- rbeta(1e3,6+1,3+1)
dens(post_samples,lwd=4,col=2,xlab="proportion water",adj=0.1)
curve(dbeta(x,6+1,3+1),add=TRUE,lty=2,lwd=3)

pred_post <- sapply(post_samples,function(p) sum(sim_globe(p,10)=="W"))
tab_post <- table(pred_post)
for(i in 0:10) {
  lines(c(i,i),c(0,tab_post[i+1]),lwd=4,col=4)
}

sim_globe2 <- function( p=0.7 , N=9 , x=0.1 ) {
  true_sample <- sample(c("W","L"),size=N,prob=c(p,1-p),replace=TRUE)
  obs_sample <- ifelse( runif(N) < x ,
                        ifelse( true_sample=="W" , "L" , "W" ) , # error
                        true_sample ) # no error
  return(obs_sample)
}



##### assignment questions ####
### My answer for assignment 1
compute_posterior(sumW=4,sumL=11)

####
#posterior is (likelihood fx x prior fx)/normalization which gives us post
### Dan Runcie answer:
##### likelihood (p,w,l): given p, how likely is it w and l. 
# dnorm(4,15,p) ### probability of 4 out of 15 given p globe/die
#likelihood <- function(W,L,p) dnorm(W,L,p)
#prior <- function(p) return(1)
#prior <- function(p) ifelse(p<0.5,0,1)
#candidates <- seq(0,1,length=2000)
#unstandard_posterior = likelihood(4,15,candidates)*prior(candidates)
#plot(candidates,unstandard_posterior)

#### The answer given. Doesn't make sense to me because the question didn't ask to change the number of sides
post <- compute_posterior(sumW=4,sumL=11,poss=seq(from=0,to=1,by=0.1))
plot(post$poss,post$post)

#### Question2:
## becomes a binomial distribution if we "know" prob
dbinom(0,5,prob=0.7)
dbinom(4,5,prob=0.7)
plot(dbinom(0:5,5,prob=0.7))

###
rbinom(1,5,prob=0.6) ### simulates tossing once out of 5 at prob p
### do num simulations based on how likely parameter/prob is

#### vector of poss where each has a prob, get 10000 from this vector at prob relative freq
samples_of_p = sample(post$poss,1e4,replace=T,prob=post$post)
hist(samples_of_p)
sim_5_toss <- rbinom(length(samples_of_p),5,prob=samples_of_p)
head(sim_5_toss)
hist(sim_5_toss,prob=T)
### point estimate
hist(rbinom(1e4,5,prob=mean(samples_of_p)))

#### Question 3 
mean(sim_5_toss >=3)


p_sample <- rbeta(1e4,4+1,11+1)
W_sim <- rbinom(1e4,size=5,p=p_samples)

