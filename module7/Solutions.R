
# Exercise 1.

library(car)

data(USPop)

USPop

attach(USPop)

plot(year, population)


#a)

time=0:21
pop.mod=nls(population~beta1/(1+exp(beta2+beta3*time)),start=list(beta1=350,beta2=4.5,beta3=-0.3),trace=T)

coef(pop.mod)

#       beta1       beta2       beta3 
# 440.8331748   4.0324001  -0.2160592 

plot(year, population)
lines(year, fitted.values(pop.mod), lwd=2)

#b)

predict(pop.mod,list(time=22.5))

# 306.8767
  
detach(USPop)



#________________________________________________________________


# Exercise 2.


library(car)

data(Prestige)

Prestige

attach(Prestige)


# (a)

# Change of scale:

income=income/1000

plot(income,prestige)


# (b)

# Moving average estimator:

l0=loess(prestige~income,degree=0,span=0.75,family="gaussian")

# We draw the fitted curve:

plot(income,prestige)
or=order(income); x=income[or]; y=l0$fitted[or]; lines(x,y,col="purple")


# (c)

# Local linear estimator:

l1=loess(prestige~income,degree=1,span=0.75)

plot(income,prestige); y=l1$fitted[or]; lines(x,y,col="red")


# (d)

# Local quadratic estimator:

l2=loess(prestige~income,degree=2,span=0.75)

plot(income,prestige); y=l2$fitted[or]; lines(x,y,col="blue")


# (e)

# Local polynomial with normal kernel:

library(KernSmooth); plot(income,prestige)

k1=locpoly(income,prestige,degree=1,kernel="normal",bandwidth=5)
plot(income,prestige); lines(k1,col="red")

k1=locpoly(income,prestige,degree=2,kernel="normal",bandwidth=5)
plot(income,prestige); lines(k1,col="red")

k1=locpoly(income,prestige,degree=3,kernel="normal",bandwidth=5)
plot(income,prestige); lines(k1,col="red")


# (f)

# Spline:

s=smooth.spline(income,prestige)
plot(income,prestige)
lines(s,col="orange")




#________________________________________________________________


# Exercise 3.


n=100
x=runif(n,0,1.6)
eps=rnorm(n,0,1/3)
y=sin(4*x)+eps

library(KernSmooth) 

k1=locpoly(x,y,degree=1,kernel="normal",bandwidth=1)
plot(x,y); lines(k1,col="red")

k2=locpoly(x,y,degree=2,kernel="normal",bandwidth=0.2)
plot(x,y); lines(k2,col="red")

k3=locpoly(x,y,degree=3,kernel="normal",bandwidth=2)
plot(x,y); lines(k3,col="red")

# local polynomial of order 2 seems to fit the data well