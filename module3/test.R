an<-anova(vol_girth_lm)
an
summ<-summary(vol_girth_lm)
summ
confint(vol_girth_lm)
#SSE/(n-2) is estymator warrianci bledow, var(b1)= var(ei)/sum(xi-mean(xi))
sqrt(sum(vol_girth_lm$residuals^2)/(nrow(trees)-2)/sum((trees$Girth-mean(trees$Girth))^2))
#Residual standard error:
sqrt(sum(vol_girth_lm$residuals^2)/(nrow(trees)-2))
names(an)
an[1,2]+an[2,2]
sum((trees$Volume-mean(trees$Volume))^2)
sum((fitted(vol_girth_lm)-mean(fitted(vol_girth_lm)))^2)

an[1,2]/(an[1,2]+an[2,2])

X<-matrix(c(rep(1,nrow(trees)),trees$Girth),nrow=nrow(trees))
H<-X%*%solve(t(X)%*%X)%*%t(X)
Y<-trees$Volume
res<-(diag(nrow(trees))-H)%*%Y
sum(res)
sum(res-vol_girth_lm$residuals)
any((res-vol_girth_lm$residuals)>0.001)
sqrt(t(vol_girth_lm$residuals)%*%vol_girth_lm$residuals/(nrow(trees)-2))

d<-seq(-10,10,length.out=100)
plot(d,dnorm(d,0,1),type="l")
lines(d,dcauchy(d,0,1),col="red")

require(graphics)
lm.SR <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)

op <- par(mar = par("mar")/2)
par(mfrow = c(2,1))  # same oma as above
plot(lm.SR, which = 1:2, sub.caption = "Saving Rates, n=50, p=5")
par(op)

crPlots(price_all.model)# partial residuals
avPlots(price_all.model)# partial regression

library(MASS)
price_all.model.studres <- studres(price_all.model)
plot(price_all.model.studres)
outliers <- which(abs(price_all.model.studres)>=2)
points(outliers, price_all.model.studres[outliers],col="red",pch=16)
text(outliers+1,price_all.model.studres[outliers],labels=outliers)
legend(x="topleft",legend=c("outlier"),pch=c(16),col=c("red"))

which(hatvalues(price_all.model)>= length(price_all.model$coefficients)/nrow(realest.data))

as.vector(which(cooks.distance(price_all.model)>1))
avPlots(activity.model)
crPlots(activity.model)

curve(sin, -2*pi, 2*pi, xname = "t",add=T)
plot(cos, -pi,  3*pi)