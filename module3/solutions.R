# Solutions to Module 3 Exercises (Lab 5-6)


# Exercise 1.

trees

(a) 

lmVH=lm(Volume~Height,data=trees)
lmVG=lm(Volume~Girth,data=trees)

lmVH

lmVG

names(lmVH)

lmVH$residuals


(b)

summary(lmVH)

summary(lmVG)

names(summary(lmVH))


summary(lmVH)$df


(c)

attach(trees)

par(mfrow=c(1,2))

plot(Volume~Girth)
abline(lmVG)

plot(Volume~Height)
abline(lmVH)


(d)

summary(lmVH)$r.squared
summary(lmVG)$r.squared


(e)

# p-value for Volume~Height: 0.000378
# p-value for Volume~Girth: < 2e-16
# Both relationships are significant.


(f)

# CI for slope coefficient for Volume~Girth.

#              Estimate Std. Error 
#(Intercept) -36.943459   3.365145 
#Girth         5.065856   0.247377 

q=qt(0.975,29)

L=5.065856-q*0.247377    # 4.559913
U=5.065856+q*0.247377    # 5.571799

# 95% confidence interval: (L,U) = (4.56, 5.57)


(g)

# Estimated variance of tree Volume in model Volume~Girth:

summary(lmVG)$sigma^2    # 18.0794


(h)

# Predicted value of Volume for Girth=15:

 -36.943459 + 15 * 5.065856     # 39.04438

predict(lmVG,newdata=list(Girth=15))    # 39.04439



# -------------------------------------------------------------------------

# Exercise 2.


# -------------------------------------------------------------------------

# Exercise 3.


# -------------------------------------------------------------------------


# Exercise 4.


cheese=read.table('D:/Dokumenty/STUDIA/MATEMATYKA dr/Dydaktyka/SM2/Zbiory danych/cheese.txt',header=T)

attach(cheese)

l1=lm(taste~Acetic)

plot(taste~Acetic)
abline(l1)

l2=lm(taste~Acetic+Lactic+H2S)

anova(l1,l2)

# Conclusion: larger model l2 is better fitted, smaller model is not good enough..



detach(cheese)
