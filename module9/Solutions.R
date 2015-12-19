# Module 9

# Exercise 1


# (a)

  duration <- c(1,2,3,5,8,10,15)
  probs <- c(0.10,0.15,0.25,0.20,0.15,0.10,0.05)
  N=1000
  
  data=sample(x=duration,size=N,replace=T,prob=probs)

 # Generated durarions:

  table(data)

# "Simulated frequencies:"

  table(data)/N


# (b)

  dur.10 <- matrix(data,nrow=10)   # in columns: durations for 10 calls
                                         
  sums.10 <- apply(dur.10,2,sum)    # joint durations of 10 calls
                    
  over.60 <- sums.10[sums.10 > 60]   # joint durations exceeding 60

 # Probability that joint duration of 10 consecutive calls is longer than 1 hour:

  length(over.60)/length(sums.10)

  hist(sums.10)



# Exercise 2


  N=5000
  ux <- runif(N)
  uy <- runif(N)
  fun <- exp(-ux^2/2)

  integral = mean(fun)
  integral


# In order to find an exact value of the integral, we notice that
# it is closely related with normal distribution.
# If Z~N(0,1) then P(0<Z<1) is an integral between 0 and 1
# of its density which differs from the function exp(-x^2/2) only
# by a factor 1/sqrt(2*pi).

  distr.norm = pnorm(c(0,1))       # cumulative distribution function at 0 and 1
  integral.theor = sqrt(2*pi) * (distr.norm[2] - distr.norm[1])
  integral.theor


  # Standard error of the estimator:

  error=sd(fun)/sqrt(N)
  error


  # confidence interval:

  alpha=0.05

  CI=c(integral-qnorm(1-alpha/2)*error, integral+qnorm(1-alpha/2)*error)
  CI



# Exercise 3


  n=500

  data=rnorm(n,mean=1,sd=1)

  prob=length(data[exp(data)>2])/n   # estimated probability P(exp(X)>2)
  prob  

  p=1-pnorm(log(2),1,1)    # theoretical (exact) value of the probability P(exp(X)>2)
  p

  variance=p*(1-p)/n     # variance of the estimator prob
  sd=sqrt(variance)
  sd


  # Standard error (estimator of sd):

  pom=rep(0,n)
  pom[exp(data)>2]=1
  se=sd(pom)/sqrt(n)
  se



# Exercise 4


  data(rivers)

  rivers


  mr=mean(rivers)
  mr

  # Now, we are looking for an estimator of standard deviation of mr.
  
  n=length(rivers)
  n

  m=3000

  bm=rep(0,m)   # vector that will store bootstrapped means
  
  for(i in 1:m){
      bootstrapped.sample=sample(rivers,size=n,replace=T)
      bm[i]=mean(bootstrapped.sample)
  }

  SE=sd(bm)
  SE


  # 95% Percentile confidence interval:

  alpha=0.05
  q1=quantile(bm-mr,0.025)
  q2=quantile(bm-mr,0.975)

  CI=c(mr-q2, mr-q1)
  CI



# Exercise 5

  n=200
  
  lambda=1.5

  # CDF of exponential distribution: F(x)=1-exp(-lambda*x)
  # Inverse of F:  F^{-1}(y)=-(1/lambda)*log(1-y)

  y=runif(n)

  sample=-(1/lambda)*log(1-y)

  hist(sample,freq=F)

  curve(dexp(x,1.5),add=T)


