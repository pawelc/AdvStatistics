# Task 1.

p=read.table("http://www.mini.pw.edu.pl/~mwojtys/sar/dane/pszen.txt",header=T)
p
attach(p)



# (a) Checking the assumptions


# Normal QQ-plots in groups:

par(mfrow=c(2,2))
tapply(plon,azot,qqnorm)


# Box plots:

par(mfrow=c(1,1))
plot(plon~azot)

# Shapiro-Wilk test of normality:

shapiro.test(plon[azot=="dawka1"])    # -> p-value = 0.7553
shapiro.test(plon[azot=="dawka2"])    # -> p-value = 0.3522
shapiro.test(plon[azot=="dawka3"])    # -> p-value = 0.1159
shapiro.test(plon[azot=="dawka4"])    # -> p-value = 0.4606

# Kolmogorov-Smirnov test of normality:

m=mean(plon[azot=="dawka1"])
s=sd(plon[azot=="dawka1"])
ks.test(plon[azot=="dawka1"],"pnorm",m,s)    # -> p-value = 0.9333


# Bartlett test of equality of variances:

bartlett.test(plon,azot)    # -> p-value = 0.7787




# (b) anova model:

a=aov(plon~azot)
a

#                     azot Residuals
# Sum of Squares   86.8325  435.6075
# Deg. of Freedom        3        28

# Residual standard error: 3.944288 


summary(a)

#             Df Sum Sq Mean Sq F value Pr(>F)
# azot         3  86.83   28.94  1.8605 0.1592
# Residuals   28 435.61   15.56


# We conclude that all mean values are equal.


# Fitted values:

l=lm(plon~azot)
summary(l)


#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  70.1750     1.3945  50.322   <2e-16 ***
#azotdawka2    0.1625     1.9721   0.082   0.9349    
#azotdawka3    1.8500     1.9721   0.938   0.3562    
#azotdawka4    4.0875     1.9721   2.073   0.0475 *  

#Residual standard error: 3.944 on 28 degrees of freedom
#Multiple R-Squared: 0.1662,     Adjusted R-squared: 0.07687 
#F-statistic:  1.86 on 3 and 28 DF,  p-value: 0.1592 


anova(l)


#          Df Sum Sq Mean Sq F value Pr(>F)
#azot       3  86.83   28.94  1.8605 0.1592
#Residuals 28 435.61   15.56


# We have identical results as before.


# Plot of mean values in groups:

m=tapply(plon,azot,mean)
stripchart(plon~azot,vertical=T)
lines(1:4,m)

# Diagnostic plots:

par(mfrow=c(2,2))
plot(l,which=1:4)


# observation number 7 seems to be outlying.




#_______________________________________________________________________

# Task 2.



tr=read.table("http://www.mini.pw.edu.pl/~mwojtys/sar/dane/trucizny.txt",header=T)
tr
attach(tr)


#(a)


interaction.plot(trucizna,kuracja,wyczas)
interaction.plot(kuracja,trucizna,wyczas)

# Does interaction seem to be significant?


a=aov(wyczas~trucizna*kuracja)
summary(a)

# Test result: interaction not significant.


#(b)

par(mfrow=c(3,1))
tapply(wyczas,trucizna,qqnorm)

par(mfrow=c(2,2))
tapply(wyczas,kuracja,qqnorm)

par(mfrow=c(3,4))
tapply(wyczas,kuracja:trucizna,qqnorm)

par(mfrow=c(1,1))
pom=kuracja:trucizna
plot(wyczas~pom)

# Variance depends on the level of factor.
# Also, some plots reveal skewed distribution.


# Distribution of residuals:

l=lm(wyczas~trucizna*kuracja)
summary(l)
plot(l,which=1:2)

# Unconstant variance very visible.


# Another model:

l2=lm(log(wyczas)~trucizna*kuracja)
summary(l2)

plot(l2,which=1:2)

# This model is a bit better but it could be improved.


library(MASS)
boxcox(l,plotit=T)

l3=lm((wyczas)^(-1)~trucizna*kuracja)
summary(l3)

plot(l3,which=1:2)

# This model is much better.



#(c)

anova(l3)

# Interaction not significant.

l3_b=lm((wyczas)^(-1)~trucizna+kuracja)
summary(l3_b)
 
interaction.plot(trucizna,kuracja,wyczas^(-1))
interaction.plot(kuracja,trucizna,wyczas^(-1))



# Multiple comparisons:


# Bonferroni method:

pairwise.t.test(wyczas,trucizna,p.adjust.method="bonferroni")


#   A      B     
# B 0.9853 -     
# C 1e-04  0.0022



# Tukey's method:


TukeyHSD(aov(l3_b),conf.level=0.95)


# $trucizna
#          diff       lwr       upr     p adj
# B-A 0.4686413 0.4154231 0.5218594 0.0271587
# C-A 1.9964249 1.9432067 2.0496431 0.0000000
# C-B 1.5277837 1.4745655 1.5810018 0.0000000
# 
# $kuracja
#              diff        lwr        upr     p adj
# II-I   -1.6574024 -1.7651237 -1.5496810 0.0000000
# III-I  -0.5721354 -0.6798568 -0.4644141 0.0335202
# IV-I   -1.3583383 -1.4660596 -1.2506169 0.0000002
# III-II  1.0852669  0.9775456  1.1929883 0.0000172
# IV-II   0.2990641  0.1913428  0.4067854 0.4550931
# IV-III -0.7862029 -0.8939242 -0.6784815 0.0018399

detach(tr)





#_______________________________________________________________________

# Task 3.


f=read.table("http://www.mini.pw.edu.pl/~mwojtys/sar/dane/fuelprices.txt",header=T)
f
attach(f)



# (a) ANOVA


# plot of group means:

m=tapply(price,city,mean)
stripchart(price~city,vertical=T)
lines(1:6,m)


# quantile plots:


par(mfrow=c(3,2))
tapply(price,city,qqnorm)


# boxplots:


par(mfrow=c(1,1))
plot(price~city)


# test for equality of variances:


bartlett.test(price,city)


# ANOVA model:


l=lm(price~city,data=f)
anova(l)


# We conclude that all the means are the same.

# a different way:
# a=aov(price~city)
# summary(a)



# (b) On the plots we can observe a large dispersion
# of the data. This means that there might be some hidden covariable.


# (c) ANCOVA


interaction.plot(month,city,price,lty=1,col=1:6)

l=lm(price~month*city)
anova(l)

# interactions are present.


par(mfrow=c(2,2))
plot(l,which=1:2)


# There are some outliers.


# We exclude city Gold Coast from the analysis 
# since the relation between Price and Month
# is not linear in this case.


l=lm(price~month*city,data=f[1:30,])
anova(l)
plot(l,which=1:2)


# Now interactions are not significant.


l=lm(price~month+city,data=f[1:30,])
anova(l)



# The fitted lines:


summary(l)


# Plots:

months=1:6
plot(price[city=="Sunshine.Coast"]~months,col="pink",pch=19,ylim=range(price))
points(price[city=="Toowoomba"]~months,col="green",pch=19)
points(price[city=="Townsville"]~months,col="black",pch=19)
points(price[city=="Brisbane"]~months,col="red",pch=19)
points(price[city=="Cairns"]~months,col="blue",pch=19)

abline(79.08,3.186,col="red")
abline(79.08+2.72,3.186,col="blue")
abline(79.08+0.53,3.186,col="pink")
abline(79.08+1.67,3.186,col="green")
abline(79.08+1.33,3.186,col="black")


# Conclusion: the most expensive petrol in in Cairns,
# the cheapest - in Brisbane.
# The price grows 3.186 per month.





#_______________________________________________________________________

# Task 4.


twins=read.table("http://www.mini.pw.edu.pl/~mwojtys/sar/dane/twins.txt",header=T)
twins
attach(twins)


# (a)


plot(BiolIQ,FosterIQ,xlab="rodzice naturalni",ylab="rodzice przybrani",type="n")

status=substr(Social,1,1)
status

kolor=rep("red",length(Social))
kolor[Social=="low"]="blue"
kolor[Social=="middle"]="orange"
text(BiolIQ,FosterIQ,status,col=kolor)

# or:

interaction.plot(BiolIQ,Social,FosterIQ,type="p",col=1:3,pch=c("h","l","m"))


# (b)

g=lm(FosterIQ~BiolIQ*Social)
summary(g)
anova(g)

plot(g,which=1:2)


# (c)

g2=lm(FosterIQ~BiolIQ+Social)
# anova(g2,g)

anova(g2)

# We can also drop the factor Social  0.2383 > 0.05

g2=lm(FosterIQ~BiolIQ)
abline(g2)

par(mfrow=c(2,2))
plot(g2,which=1:4)

summary(g2)



