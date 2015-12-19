
# Exercise 1.


library(rpart)
data(kyphosis)
kyphosis
attach(kyphosis)


# (a)

g=glm(Kyphosis~Age+Number+Start,"binomial")

summary(g)


# (b)

# In R null deviance is defined as deviance of null model from saturated model.
# However, null deviance is also often defined as the deviance of a null model
# from a given model (in this case our fitted model):

g$null.dev - g$dev	   # -> 21.854

# Residual deviance:

g$dev		   # -> 61.37993


# (c)

# Likelihood Ratio Test:

g0=glm(Kyphosis~1,"binomial")
anova(g0,g, test="Chi")     #  ->  p-value = 6.994e-05

# A different way:

1-pchisq(g$null.dev-g$dev,g$df.null-g$df.res)      # -> p-value=6.993833e-05

# We reject null hypothesis that a null model is adequate. Thus some
# of the variables are significant.


# (d)

# We cannot perform test for adequacy of the model since the test statistic
# g$dev has asymptotic distriubution equal to chi-square with n-p degrees of
# freedom when n_i tends to infinity (n_i - number of replications at each value
# of x, i=1,...,n). Here we have n_i=1 for all i.


# (e)

1-g$dev/g$null.dev     # ->  0.2625661


# (f)

g2=glm(Kyphosis~Age+Number+Start+I(Age^2)+I(Number^2)+I(Start^2),"binomial")

# AIC = - 2logL + 2p = dev + 2p

g$aic		# -> 69.37993
g2$aic		# -> 61.83235

# other way:
g$dev+2*4	# -> 69.37993
g2$dev+2*7	# -> 61.83235

# Smaller value of AIC proves a better fit of model g2

# test for comparison of models:
anova(g,g2,test="Chi")     # -> p-value = 0.004 => g2 is better fitted


# (g)

step(g2)

step(g2,test="Chisq")

# Resulting model:  Kyphosis ~ Age + Start + I(Age^2) + I(Start^2) 

g3=glm(Kyphosis~Age+Start+I(Age^2)+I(Start^2),"binomial")


# (h)

# "Absent" is treated as failure (0) and "Present" is taken as a success (1)
# (alphabetically first value of response variable is taken as a failure
# and all the other values are taken as a success).
# Thus we model the probability of presence of the sickness.

predict(g3,newdata=data.frame(Age=20,Start=10),type="response")    # -> 0.1280725




#____________________________________________________________________________

# Exercise 2.


mydata=read.table("http://www.mini.pw.edu.pl/~mwojtys/sar/dane/Brands.txt",header=T)
attach(mydata)
names(mydata)

# (a)

mydata$brand<-as.factor(mydata$brand)
library(nnet)
m=multinom(brand~female+age)
summary(m)

# Coefficients:
#   (Intercept)    female       age
# 2   -11.77469 0.5238197 0.3682075
# 3   -22.72141 0.4659488 0.6859087

# Brand 1 is our reference level.

#    log(P(brand=2)/P(brand=1)) = b_10 + b_11*female + b_12*age
#    log(P(brand=3)/P(brand=1)) = b_20 + b_21*female + b_22*age, 
# 
# Equivalently (we use the fact that P(brand=1)+P(brand=2)+P(brand=3)=1):
#
#    P(brand=1)=1/(1+exp(b_10 + b_11*female + b_12*age)+exp(b_20 + b_21*female + b_22*age))
#    P(brand=2)=exp(b_10 + b_11*female + b_12*age)/(1+exp(b_10 + b_11*female + b_12*age)+exp(b_20 + b_21*female + b_22*age))
#    P(brand=3)=exp(b_20 + b_21*female + b_22*age)/(1+exp(b_10 + b_11*female + b_12*age)+exp(b_20 + b_21*female + b_22*age))


# (b)

beta=coef(m)


# For example, we can say that for one unit change in the variable age, the log
# of the ratio of the two probabilities, P(brand=2)/P(brand=1), will be increased
# by 0.368, and the log of the ratio of the two probabilities P(brand=3)/P(brand=1)
# will be increased by 0.686. Therefore, we can say that, in general, the older
# a person is, the more he/she will prefer brand 2 or 3 to brand 1.


# We can also present the regression results in terms of probabilities.
# For example, we can look at a range of age values and calculate the probabilities
# of choosing each brand for both females and males.


# (c)

X=cbind(rep(1,30),c(rep(0,15),rep(1,15)),rep(24:38,2)) # intercept, female, age

beta=coef(m)

Xb1=X %*% beta[1,]
Xb2=X %*% beta[2,]

p1=1/(1+exp(Xb1)+exp(Xb2))
p2=exp(Xb1)/(1+exp(Xb1)+exp(Xb2))
p3=exp(Xb2)/(1+exp(Xb1)+exp(Xb2))


# Graphs of fitted probabilites for three brands separately (divided by gender):

plot(p1[1:15]~X[1:15,3],
	type="l",col="blue",lwd=1,ylab="Predicted Probability for Brand 1",xlab="Age")
lines(p1[16:30]~X[16:30,3],col="red",lwd=1)
legend(33.5,.93,c("Males","Females"),col=c("blue","red"),lwd=c(1,1))


plot(p2[1:15]~X[1:15,3],
	type="l",col="blue",lwd=1,ylab="Predicted Probability for Brand 2",xlab="Age")
lines(p2[16:30]~X[16:30,3],col="red",lwd=1)
legend(33.5,.93,c("Males","Females"),col=c("blue","red"),lwd=c(1,1))


plot(p3[1:15]~X[1:15,3],
	type="l",col="blue",lwd=1,ylab="Predicted Probability for Brand 3",xlab="Age")
lines(p3[16:30]~X[16:30,3],col="red",lwd=1)
legend(33.5,.93,c("Males","Females"),col=c("blue","red"),lwd=c(1,1))



# Another possibility: function mlogit() in library mlogit.





#____________________________________________________________________________

# Exercise 3.


discoveries

plot(discoveries)

g=glm(discoveries~1,poisson)

summary(g)

mean(discoveries)    # 3.1


# Goodness-of-fit test:

p=1-pchisq(164.68,99)    # p-value
p

p=1-pchisq(g$dev,g$df.res)
p

# Small p-value (3.79455e-05)
# Conclusion: the model is not well fitted.
# So the number of discoveries is not constant in time.


year=seq(1860,1959,step=1)
plot(year,residuals(g))


# Larger model:

g1=glm(discoveries~year+I(year^2),poisson)
summary(g1)
step(g1,test="Chi")
#p1=1-pchisq(164.68-132.84,2)

anova(g,g1)

roznica=g1$null.deviance-g1$deviance
p1=1-pchisq(roznica,g$df.null-g$df.res)
p1

# Conclusion: model g1 is better fitted than model g. 

# Thus the mean number of discoveries within one year is not constant
# in time.

1-pchisq(g1$dev,g1$df.res)   # small p-value

# The larger model is still not well fitted at 5% significance level.

# Explained deviance:

1-g1$dev/g1$null.dev 

# 0.19




#____________________________________________________________________________

# Exercise 4.



gator=read.table("D:/Dokumenty/Zbiory danych/gator.data",header=T)

# (a)

gator1=aggregate(gator$count,list(food=gator$food,lake=gator$lake),FUN=sum)
gator1


# (b)

g1=glm(x~as.factor(food)+as.factor(lake),poisson,gator1)
summary(g1)

# We want to test hypothesis pij=p_i*p_j, i.e.
# log(expected count in cell (i,j))=log n + log p_i +log p_j (*)

# Formal test of adequacy:

p1=1-pchisq(g1$deviance,g1$df.res)
p1

# We reject the null hypothesis. The model is not adequate, i.e. variables 'count'
# and 'food' are not independent.


# (c)

# fraction for lake 1

attach(gator1)

f1=sum(x[lake==1])/sum(x)
f1

# Estimated probability that an aligator comes from lake 1:

f1=sum(g1$fitted[lake==1])/sum(g1$fitted)
f1

s=exp(0)/(1+exp(g1$coeff[6])+exp(g1$coeff[7])+exp(g1$coeff[8]))
s






