# TODO: Add comment
# 
# Author: pawelc
###############################################################################


library(faraway)
data(bliss)
g <- glm(cbind(dead,alive) ~ conc, family=binomial, data=bliss)
g$fit

g$fit/(1+g$fit)

ilogit(g$coef[1]+g$coef[2]*bliss$conc)

qchisq(0.01,1,lower.tail = F)

head(kyphosis.lr.model$fitted.values)
head(kyphosis)