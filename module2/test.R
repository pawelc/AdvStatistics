# TODO: Add comment
# 
# Author: pawelc
###############################################################################


d<-data.frame(a=1:2,b=3:4)
attach(d)
a
detach(d)
a

curve(sin,xlim=c(0,2*pi))

x=rlnorm(50) 
y=rlnorm(50)+0.5*x 
pval=cor.test(x,y)$p.value
for (i in 1:1000) { 
  x=rlnorm(50)
  y=rlnorm(50)+0.5*x 
  pval=c(pval,cor.test(x,y)$p.value)
}
reject=length(pval[pval<0.05])/1001 
reject

x=rnorm(50) 
y=rnorm(50)+0.5*x 
pval=cor.test(x,y)$p.value
for (i in 1:1000) { 
  x=rnorm(50)
  y=rnorm(50)+0.5*x 
  pval=c(pval,cor.test(x,y)$p.value)
}
reject=length(pval[pval<0.05])/1001 
reject

# test of independence based on Spearman rho
x=rlnorm(50) 
y=rlnorm(50)+0.5*x 
pval=cor.test(x,y, method="spearman")$p.value
for (i in 1:1000){ 
  x=rlnorm(50)
  y=rlnorm(50)+0.5*x 
  pval=c(pval,cor.test(x,y,method="spearman",exact=TRUE)$p.value)
}
reject=length(pval[pval<0.05])/1001 
reject

r=cor(trees$Girth,trees$Height) 
r
rPerm=rep(10000,0)
for (i in 1:10000) {
  GirthPerm=sample(trees$Girth) # function sample(x) yields a permutation of a vector x 
  rPerm[i]=cor(GirthPerm,trees$Height)
}
length(rPerm[rPerm>r])

melan



m=matrix(c(22,16,19,11,2,54,33,17,10,115,73,28),ncol=3)
col.props=apply(m,2,sum)/400 
row.props=apply(m,1,sum)/400 # Calculate matrix of products *400 
em=outer(row.props,col.props)*400 
pearson=sum((m-em)^2/em)
pearson
1-pchisq(122.99,6)