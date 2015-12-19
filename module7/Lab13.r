### TASK 1:

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
detach(USPop)

#################################################################


### TASK 2:

library(car)
data(Prestige)
Prestige
attach(Prestige)

# Change of scale:

income=income/1000

plot(income,prestige)

# Moving average estimator:

l0=loess(prestige~income,degree=0,span=0.75,family="gaussian")
plot(income,prestige)

or=order(income); x=income[or]; y=l0$fitted[or]; lines(x,y,col="purple")

# Local linear estimator:

l1=loess(prestige~income,degree=1,span=0.75)
plot(income,prestige); y=l1$fitted[or]; lines(x,y,col="red")

# Local quadratic estimator:

l2=loess(prestige~income,degree=2,span=0.75)
plot(income,prestige); y=l2$fitted[or]; lines(x,y,col="blue")

# Local polynomial with normal kernel:

library(KernSmooth); plot(income,prestige)

k1=locpoly(income,prestige,degree=1,kernel="normal",bandwidth=0.25)
plot(income,prestige); lines(k1,col="red")

k1=locpoly(income,prestige,degree=2,kernel="normal",bandwidth=5)
plot(income,prestige); lines(k1,col="red")

k1=locpoly(income,prestige,degree=3,kernel="normal",bandwidth=5)
plot(income,prestige); lines(k1,col="red")


# Spline:

s=smooth.spline(income,prestige)
plot(income,prestige)
lines(s,col="orange")

#################################################################

### TASK 3:
curve1 = function(x) {

4.26*(exp(-x)-4*exp(-2*x)+3*exp(-3*x))

}

n=100
x=runif(n,min=0,max=3.5)
x=sort(x)
e=rnorm(n,0,0.1)
y=curve1(x)+e

yteor=curve1(x)

plot(x,y)
lines(x,yteor)

#NADARAYA-WATSON
est.ksmooth = ksmooth(x,y)


#SPLINE:
est.spline=smooth.spline(x,y)

#LOESS:
est.loess1=loess(y~x)


par(mfrow=c(2,2))

plot(x,y,main="Nadaraya-Watson")
lines(x,yteor)
lines(est.ksmooth,col="red")

plot(x,y,main="Spline")
lines(x,yteor)
lines(est.spline,col="orange")


plot(x,y,main="loess")
lines(x,yteor)
lines(x,est.loess1$fitted,lty=1,col="blue")

#b)
ISE=function(x,y) mean((x-y)^2)

ISE(y,est.loess1$fitted)
ISE(y,est.ksmooth$y)
ISE(y,est.spline$y)

#c)
est.loess01=loess(y~x,span=0.1)
est.loess04=loess(y~x,span=0.4)
est.loess075=loess(y~x,span=0.75)

plot(x,y,main="loess")
lines(x,yteor)
lines(x,est.loess01$fitted,lty=1,col="blue")
lines(x,est.loess04$fitted,lty=1,col="green")
lines(x,est.loess075$fitted,lty=1,col="red")
legend("bottomright",c("span=0.1","span=0.4","span=0.75"),col=c("blue","green","red"),lty=c(1,1,1))




