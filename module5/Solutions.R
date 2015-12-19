# Exercise 1.


cig=read.table("http://www.mini.pw.edu.pl/~mwojtys/sar/dane/cigconsumption.txt",header=T)
cig
attach(cig)



# a)

l=lm(Sales~.-State,data=cig)
summary(l)

par(mfrow=c(2,2))

plot(l,which=1:4)

# Outliers:

rstudent(l)[abs(rstudent(l))>2]  # -> 29, 30

# Influential observations:

cd=cooks.distance(l)
cd[cd==max(cd)]

h=hatvalues(l)
h[h==max(h)]


# -> 9, 29, 30


# We exclude observations no 29 and 30 from further analysis (as outliers)

l=lm(Sales~.-State,data=cig,subset=-c(29,30))
summary(l)

# RSE decreased, R^2 increased significantly

# Based on F test, we do not reject hypothesis that all variables are insignificant.

# Based on t tests, variables: Female, Black, HS, Age are insignificant if all the 
other predictors are incorporetd in the model.

# Can we remove them all?

l1=lm(Sales~Income+Price,subset=-c(29,30)) 

anova(l1,l)

# No.

# Can we remove Female and Black and HS from the model?

l2=lm(Sales~Income+Price+Age,data=cig,subset=-c(29,30))

anova(l,l2)

# No.


# Choose best subset of predictors

# Based on t-tests stepwise procedure

m1=lm(Sales~.-State-Female,data=cig,subset=-c(29,30))
summary(m1)

m2=lm(Sales~.-State-Female-HS,data=cig,subset=-c(29,30))
summary(m2)

anova(l,m2)

# ok



# Based on AIC criterion

step(l) # direction="both"

#Coefficients:
#(Intercept)          Age       Income        Black        Price  
#   59.58875      3.76837      0.01527      0.56558     -2.86162  

# same model



# Based on BIC criterion

step(l,k=log(length(Sales)-2))

#Coefficients:
#(Intercept)          Age       Income        Black        Price  
#   59.58875      3.76837      0.01527      0.56558     -2.86162  

# the same model


# Exhaustive search

library(leaps)

# The best model for each size:

b=regsubsets(Sales~.-State,data=cig[-c(29,30),])

rs=summary(b)
rs

# For a model of a given size all the methods: Adj-R2, AIC, BIC
# select the model with the smallest residual sum of squares.

rs$adjr2

plot(rs$adjr2)

# The best model based on Adj-R2 includes: Age, HS, Income, Black, Price.


plot(rs$cp)

# Based on Cp: Age, Income, Black, Price


plot(rs$bic)

# Based on BIC: Age, Income, Black, Price



summary(l)$r.squared	# 0.59	
summary(m2)$r.squared	# 0.58	

summary(l)$adj.r.squared  #  0.54
summary(m2)$adj.r.squared # 0.54	

# R-squared coefficient are practically the same.

anova(l,m2)  # comparison of models using F test.



# How did the standard errors of estimated coefficients change?

# Their variability is smaller now:

summary(l)


m2=lm(Sales~Age+Income+Black+Price,data=cig,subset=-c(29,30))




#_________________________________________________________________________________

# Exercise 2.



us=read.table("http://www.mini.pw.edu.pl/~mwojtys/sar/dane/uscrime.txt",header=T)
us
attach(us)


l1=lm(R~.,data=us)
summary(l1)

pairs(us)

# -> Ex0 i Ex1 sa highly correlated (collinear);
# one of them should be removed from the model.

l2=lm(R~.-Ex0,data=us)
summary(l2)

# Note that in model l1 both variables (Ex0 and Ex1) were insignificant.
# After removing Ex0, Ex1 became significant!


# Elimination method based on t test

Before variable selection we remove outliers: 11, 19

rs=rstandard(l2)
rs[abs(rs)>2]


summary(lm(R~.-Ex0,data=us,subset=-c(11,19)))
summary(lm(R~.-Ex0-S,data=us,subset=-c(11,19)))
summary(lm(R~.-Ex0-S-M,data=us,subset=-c(11,19)))
summary(lm(R~.-Ex0-S-M-LF,data=us,subset=-c(11,19)))
summary(lm(R~.-Ex0-S-M-LF-W,data=us,subset=-c(11,19)))
summary(lm(R~.-Ex0-S-M-LF-W-N,data=us,subset=-c(11,19)))
summary(lm(R~.-Ex0-S-M-LF-W-N-NW,data=us,subset=-c(11,19)))
summary(lm(R~.-Ex0-S-M-LF-W-N-NW-U1,data=us,subset=-c(11,19)))

# ->  Age, Ed Ex1, U2, X


# Variable selection based on AIC:

# Elimination:

l=lm(R~.-Ex0,data=us,subset=-c(11,19))
step(l,direction="backward")

# -> Age, Ed, Ex1, NW, U1, U2, X


# Stepwise regression:

step(l,direction="both")

# -> Age, Ed, Ex1, NW, U1, U2, X

# Forward selection:

l0=lm(R~1,subset=-c(11,19))
step(l0,direction="forward",scope=list(upper=l))

# -> Age, Ed, Ex1, NW, U1, U2, X


# Another possibility:

library(leaps)
s=summary(regsubsets(R~.-Ex0,data=us[-c(11,19),],method="backward"))

s$cp            # -> Age, Ed, Ex1, U1, U2, X
s$adjr2	        # -> Age, Ed, Ex1, N, NW,  U1, U2, X 	
s$bic           # -> Age, Ed, Ex1, U2, X

detach(us)



#_________________________________________________________________________________

# Exercise 3.


library(MASS)
attach(longley)

L0 = lm(Employed~.,data=longley)
summary(L0)

longley

X=as.matrix(longley[,-7])

pairs(X)

round(cor(X),3)


# Wspolczynniki determinacji wielokrotnej:

R = numeric(ncol(X))

for(i in 1:ncol(X)){
 L = lm(X[,i]~X[,-i],data=as.data.frame(X))
 R[i] = summary(L)$r.squared
} 
R
plot(1:ncol(X),R)
  

# Variance inflation factors (wspolczynniki podbicia wariancji):

VIF = numeric(ncol(X))

for(i in 1:ncol(X)){
 VIF[i] = 1/(1-R[i])
} 
VIF


library(faraway)
vif(X)


# Ridge regression:

rr <- lm.ridge(Employed~ .,data=longley,lambda = seq(0,0.2,0.001))
  
rr
summary(rr)

plot(rr)


# Optimal lambda by crossvalidation:  

select(rr)


# Estimators of regression coefficients for lambda=0.03:

gr1 <- lm.ridge(Employed~ .,data=longley,lambda = 0.03)
gr1$coef

 
# Porownac wspolczynnik przy zmiennej GNP (Produkt narodowy brutto) 
# dla modelu z punku a) oraz dla modelu dopasowanego przy pomocy regresji 
# grzbietowej z parametrem lamda=0.03.

#  GNP = -3.582e-02 
#  GNP = 0.7693585 (zgodnie z intuicja!).


detach(longley)




#_________________________________________________________________________________

# Exercise 4.


prostate=read.table("http://www.mini.pw.edu.pl/~mwojtys/sar/dane/prostate.data",header=T)

attach(prostate)


# First, we try regular regression:

M0 = lm(lpsa~. -train,data=prostate)
summary(M0)

step(M0,direction="backward")

# Selected: lcavol, lweight, age, lbph, svi. 


# LASSO:


library(lars)

lasso = lars(as.matrix(prostate[,-c(9,10)]),lpsa,type="lasso")


# As lambda decreases variables are added to the model:

lasso


plot(lasso,breaks=FALSE)


plot(lasso,breaks=FALSE,plottype="Cp")
lasso$Cp


# Selected variables: lcavol, svi, lweight, pgg45, lbph, age


# Crossvalidation:

d=cv.lars(as.matrix(prostate[,-c(9,10)]),lpsa,mode="step")
d$cv

d=cv.lars(as.matrix(prostate[,-c(9,10)]),lpsa)



# Estimators of regresssion coefficients:

lasso$beta[6+1,]





#_________________________________________________________________________________

# Exercise 5.



cities= read.table("http://www.mini.pw.edu.pl/~mwojtys/sar/dane/cities.txt",h=T)
attach(cities)
cities


# a) Standardization:

cities_std=scale(cities)

cities_std=as.data.frame(cities_std)


# A different way to do that:

m.mean = apply(cities,2,mean)
m.std = apply(cities,2,sd)
cities_std = sweep(cities,2,FUN="-",m.mean)
cities_std = sweep(cities_std,2,FUN="/",m.std)


# b)

pc1=princomp(~., cor=FALSE, data = cities_std[,1:2])

pc1
summary(pc1)

plot(pc1)

# Directions:

pc1$loadings

plot(cities_std$Work,cities_std$Price)
lines(cities_std$Work,-1*cities_std$Work,col="red")
lines(cities_std$Work,1*cities_std$Work,col="blue")


# c)

# PCA for all three variables:

pc = princomp(~., cor=FALSE, data = cities_std)


# Percentage of variability explained by principal components:

summary(pc)
plot(pc)


# Principal directions:

pc$loadings

# Principal components:

pc$scores


# e)

which.max(pc$scores[,1])
cities_std["Manila",]

# a1= c(0.485,-0.618,-0.619) 
# Relatively long work hours and relatively low salaries
# and low cost of living.






#_________________________________________________________________________________

# Exercise 6.


library(pls)
data(yarn)
attach(yarn)

yarn$density
yarn$NIR
yarn$train


#a), b)

yarn.pcr <- pcr(density ~ NIR, data = yarn[yarn$train,],validation="CV",scale=T)
summary(yarn.pcr)

# -> crossvalidation selects all components. 

plot(yarn.pcr,plottype="validation")

# Closer look:

plot(yarn.pcr,plottype="validation",ylim=c(0,2))


# By the way, we can also plot loadings (in order to interpret the components):

plot(yarn.pcr,plottype="loadings",1)     
plot(yarn.pcr,plottype="loadings",2)    
plot(yarn.pcr,plottype="loadings",1:3)
# and so on...



#c), d)

yarn.pls <- plsr(density ~ NIR, data = yarn[yarn$train,], validation = "CV")
summary(yarn.pls)

# -> crossvalidation selects first 13 components

plot(yarn.pls,plottype="validation")


yarn.pls <- plsr(density ~ NIR, data = yarn[yarn$train,], validation = "LOO")
# leave-one-out crossvalidation

summary(yarn.pls)

# e)

plot(yarn.pls, plottype="loadings", comps = 1:2)


# f)

pred1=predict(yarn.pcr, comps = 1:17, newdata = yarn[!yarn$train,])
pred2=predict(yarn.pls, comps = 1:14, newdata = yarn[!yarn$train,])




