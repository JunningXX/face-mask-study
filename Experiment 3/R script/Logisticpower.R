

#estimates power (by simulation) for logistic regression with given sample size, and probability of choosing masked. 
#n.sam =  number of participants per simulation.
#rp = number of simulations power is based on
#eff.pr =  probability of choosing masked in simulations.
power.log=function(eff.pr, n.sam=200, rp=10){
  #generates data from a binomial distribution.
  x=array(rbinom(n.sam*rp, size=1, prob=eff.pr), dim=c(n.sam, rp))
  #passes simulated data to logistic regression function
  round(mean(apply(x, 2, FUN=mask.logistic)),3)
  #does this for every simulated, returning the proportion of simulations where null is rejected.
}



#fits logistc reg to simulated data and returns whether null is rejected (true) or not
mask.logistic=function(x){
  n=length(x)/2
  t.data=data.frame(DV=x, Gender=rep(c(-0.5, 0.5),times=n), Allocation=rep(c(-0.5, 0.5),each=n))
  logist<-summary(glm(DV~Gender*Allocation,family=binomial, data=t.data))$coefficients
  logist[1,4]<0.05
  
}

#try all integer probabilities between 50 and 99% with small simulation number (inaccurate)
sapply(seq(0.5,0.99, 0.01), FUN=power.log, n.sam=200, rp=100)
#try all integer probabilities between 60 nd 70% (close to 90% power) with increased accuracy
sapply(seq(0.6,0.7, 0.01), FUN=power.log, n.sam=200, rp=500)
#try all integer probabilities between 60 and 65% with higher number of sample
sapply(c(0.5,seq(0.6,0.65, 0.01)), FUN=power.log, n.sam=200, rp=20000)
#probabiliter effect size of 0.5 should be a null effect, so should be approximately alpha.
