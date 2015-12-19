# Exercise 1.


# Correct model:

f1=function(n)
{
	x=seq(0,1,length=n)
	y=2+15*x+rnorm(n,0,sqrt(2))
	l=lm(y~x)
	return(l)
}


# Gamma distribution of errors:

f2=function(n)
{
	x=seq(0,1,length=n)
	y=2+15*x+(rgamma(n,2,2)-1)
	l=lm(y~x)
	return(l)
}


# Cauchy distribution of errors:

f3=function(n)
{
	x=seq(0,1,length=n)
	y=2+15*x+rcauchy(n,0,1)
	l=lm(y~x)
	return(l)
}


# Quadratic model:

f4=function(n)
{
	x=seq(0,1,length=n)
	y=2+15*x^2+rnorm(n,0,2)
	l=lm(y~x)
	return(l)
}


# Plots:

resplot=function(f,n)
{
	r=f(n)$res
	plot(r)
}

qqres=function(f,n)
{
	r=f(n)$res
	qqnorm(r)
}


# Plots for correct model:

resplot(f1,30)
resplot(f1,100)
resplot(f1,300)

qqres(f1,30)
qqres(f1,100)
qqres(f1,300)

# Plots for gamma model:

resplot(f2,30)
resplot(f2,100)
resplot(f2,300)

qqres(f2,30)
qqres(f2,100)
qqres(f2,300)

# Plots for Cauchy model:

resplot(f3,30)
resplot(f3,100)
resplot(f3,300)

qqres(f3,30)
qqres(f3,100)
qqres(f3,300)

# Plots for quadratic model:

resplot(f4,30)
resplot(f4,100)
resplot(f4,300)

qqres(f4,30)
qqres(f4,100)
qqres(f4,300)



#_________________________________________________________________________


# Exercise 2.


attach(trees)

l1=lm(Volume~Girth)
l2=lm(Volume~Height)



# Residual plots for model l1:

res=l1$res

par(mfrow=c(2,2))
plot(res)
plot(Girth,res)
plot(l1$fitted,res)

# We observe that residuals tend to be positive for small and large
# values of Girth and negative for medium values of Girth.
# Thus the functional relationship between Girth and Volume is probably
# misspecified.


# Residual plots for model l2:

res=l2$res

par(mfrow=c(2,2))
plot(res)
plot(Height,res)
plot(l2$fitted,res)

# We observe non-constant variance in plots 2 and 3 and a clear tendency in plot 1. 


# As to the model l1, we look again at residuals:

par(mfrow=c(2,2))

plot(l1,which=1:4)

# plot of residuals versus fitted values
# suggests nonlinear relationship between variables.
# Thus linear model is inadequate.


# We propose a quadratic model:

l1_2=lm(Volume~Girth + I(Girth^2))

plot(l1_2,which=1:2)

# Residual plot looks much better now. 

summary(l1)

# R-squared = 0.9353

summary(l1_2)

# R_squared=0.9616 larger than for model l1.


# We draw the fitted curve:

par(mfrow=c(1,1))

plot(Volume~Girth, main="Quadratic model")
x=seq(min(Girth),max(Girth),length=100)
y=coef(l1_2)[1] + coef(l1_2)[2]*x + coef(l1_2)[3]*x^2
lines(x,y)


summary(l1)$sigma^2		# ->  18.079
summary(l1_2)$sigma^2		# ->  11.121

# Estimated variance is much smaller than in model l1.

detach(trees)



#______________________________________________________________________


# Exercise 3.



rel=read.table("D:\\Dokumenty\\STUDIA\\MATEMATYKA dr\\Dydaktyka\\SM2\\Zbiory danych\\realest.txt",header=T)
attach(rel)


# (a)

l=lm(Price~.,data=rel)
summary(l)  

par(mfrow=c(2,2))
plot(l,which=1:4)



# (b)

# We look for outliers in the data by analysing the studentized residuals.
# For this we use function rstudent():

rs=rstudent(l)
rs[abs(rs)>2]

# Observations number 8, 16 and 20 have large studentized residuals.



# (c)

# Influential observations (influence is large if h_ii > 2p/n).
# Here: p=9, n=26

hat=hatvalues(l)
hat[hat>2*9/length(Price)]

# Observation number 8 has a large influence on the fitted hyperplane.


# Cook's distances:

cook=cooks.distance(l)
cook[cook==max(cook)]

# Observation number 8 has the largest Cook's distance.

# Cook's diagram:

plot(l,which=4)


detach(rel)




#______________________________________________________________________

# Exercise 4.


a=read.table("D:\\Dokumenty\\STUDIA\\MATEMATYKA dr\\Dydaktyka\\SM2\\Zbiory danych\\activity.txt",header=T)

attach(a)


# (a)

g=lm(Y~X1+X2)
summary(g)
par(mfrow=c(2,2))
plot(g,which=1:2)

# Estimated variance: (RSE)^2, i.e. (216.7)^2,
# R-Squared = 0.9468


# Partial regression plot for X1:

x=lm(X1~X2)$res
y=lm(Y~X2)$res
plot(y~x,main="Partial regression plot")

lm(y~x)$coef   # the intercept is always 0 because the average value of residuals is zero.
g$coef

# The slopes related with X1 are the same!


abline(0,g$coef['X1'])



# Partial residual plot for X1:

library(faraway)
prplot(g,1)


# How will the square root of X1 fit the data?

y2=g$res+coef(g)[2]*X1
ll=lm(y2~sqrt(X1))
curve(ll$coef[1]+ll$coef[2]*sqrt(x), xlim=range(X1),add=T)


# Both plots suggest that there is nonlinear influence of X1 on Y.



# Partial regression plot and partial residual plot for X2:

x=lm(X2~X1)$res
y=lm(Y~X1)$res
plot(y~x,main="Partial regression for X2")
prplot(g,2)

# We cannot notice any clear nonlinear behaviour.



# We fit a new model with X1 transformed:


g2=lm(Y~sqrt(X1)+X2)
summary(g2)
plot(g2,which=1:2)

# R_Squared increased from 0.947 to 0.975.
# RSE decreased from 219.7^2 to 147.5^2.
# (badly fitted model always overestimates the variance)
# But the QQ-plot is still not good!

prplot(g2,2)

# -> Now we see that X2^2 could be a better solution.

g3=lm(Y~I(X1^0.5)+I(X2^2))

plot(g3,which=1:2)

# -> now diagnostic plots are fine.

summary(g3)

# RSE = 137.1, R^2 = 0.9787

# R-squared increased even more and RSE (sigma) decreased.


prplot(g3,1)
prplot(g3,2)

detach(a)




#______________________________________________________________________

# Exercise 5.


st=read.table("http://www.mini.pw.edu.pl/~mwojtys/sar/dane/strongx.txt",header=T)
st
attach(st)

l1=lm(crossx~energy)
l2=lm(crossx~energy,weights=sd^-2)
summary(l1)

# -> R_Squared = 0.9548, RSE = 12.69 

summary(l2)

# -> R_Squared = 0.9397, RSE = 1.657

# R_Squared decreases a bit, but RSE decreases significantly.


plot(crossx~energy)
abline(l1,col="green")
abline(l2,col="red")

# Weighted Least Squares line is better fitted for smaller energies
# where sd is smaller:

plot(sd~energy)


# (b)

par(mfrow=c(2,2))
plot(l2,which=1:2)

# Large R_Squared doesn't mean that a better model can't be fitted.
# Residual plot suggests quadratic dependence.

l3=lm(crossx~energy+I(energy^2),weights=sd^-2)
summary(l3)
# -> R_Squared = 0.9911, RSE = 0.6788
par(mfrow=c(2,2))
plot(l3,which=1:2)

# Diagnosis plots suggest a good fit of this model.

par(mfrow=c(1,1))
plot(crossx~energy)
abline(l2)
curve(l3$coef[1]+l3$coef[2]*x+l3$coef[3]*x^2,xlim=range(energy),add=T)

We can also remove the variable energy as it is insignificant.

l4=lm(crossx~I(energy^2),weights=sd^-2)
summary(l4)

# Note smaller RSE than for model l3 and larger Adjusted R-Squared.

detach(st)



#______________________________________________________________________

# Exercise 6.


us=read.table("http://www.mini.pw.edu.pl/~mwojtys/sar/dane/uscrime.txt",header=T)
us
attach(us)


# (a)

l1=lm(R~.,data=us)
summary(l1)

# (b)

pairs(us)

# -> Ex0 i Ex1 are highly correlated (collinear);
# one of them should be removed from the model.

cor(us)

vif(us)


l2=lm(R~.-Ex0,data=us)
summary(l2)

# Note that in model l1 both variables (Ex0 and Ex1) were insignificant.
# After removing Ex0, Ex1 became significant!

detach(us)

