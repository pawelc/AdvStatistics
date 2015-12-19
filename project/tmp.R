x=(1:100)  
y=exp(x)                    
cor(x,y,method="spearman")  
cor(x,y,method="pearson")  

m1<-matrix(c("a","b","c","d"),nrow=2)
m2<-matrix(c("1","2","3","4"),nrow=2)
matrix(paste(m1,m2),nrow=2)

apply(rbind(pearson,spearman),MARGIN=2,FUN=function(row){sprintf("%+1.2f %+1.2f",row[1],row[2])})
length(pearson)

class(kaggle.data[[variable.names[1]]])
shapiro.test(kaggle.data[[variable.names[10]]])

apply(kaggle.data[variable.names],2,function(x){shapiro.test(x)})

for(names in combn(variable.names,m=2)){
	print(names[2])
}

#pearson correlation test
sum(apply(combn(variable.names,m=2),2,function(names){
			res<-cor.test(kaggle.data[[names[1]]],kaggle.data[[names[2]]])
			if(res$p.value < 0.05){
				print(sprintf("cor test between: %s and %s",names[1],names[2]))
				print(res)
				return(T)
			}
			return(F)
		}))

ct<-cor.test(kaggle.data[[variable.names[1]]],kaggle.data[[variable.names[2]]])
names(ct)
ct$estimate
ct$p.value

v1<-"AG.LND.ARBL.HA.PC"
v2<-"Corruption.Index"
cor.test(kaggle.data[[v1]],kaggle.data[[v2]])
cor.test(kaggle.data[[v1]],kaggle.data[[v2]],method="spearman")

model.full<-lm(Corruption.Index~.,kaggle.data[variable.names])
model<-lm(Corruption.Index~.,kaggle.data[variable.names[variable.names!="BX.KLT.DINV.WD.GD.ZS"]])
plot(sort(kaggle.data[["AG.LND.AGRI.K2"]]))
hist(kaggle.data[["AG.LND.AGRI.K2"]][kaggle.data[["AG.LND.AGRI.K2"]]<=500000])

model.full	
summary(model.full)
plot(model.full$fitted.values,model.full$residuals)
summary(model)

library(Matrix)
length(kaggle.data[variable.names[variable.names!="Corruption.Index"]])
rankMatrix(kaggle.data[variable.names[variable.names!="Corruption.Index"]])

lm(formula = Corruption.Index ~ FS.AST.PRVT.GD.ZS + IC.EXP.DURS +
				EG.USE.ELEC.KH.PC + IC.CRD.PRVT.ZS + NE.TRD.GNFS.ZS + ER.H2O.INTR.PC,
		data = kaggle.data[variable.names])

lm(formula = Corruption.Index ~ AG.LND.ARBL.HA.PC + AG.LND.ARBL.ZS +
				AG.YLD.CREL.KG + BM.GSR.TRVL.ZS + BX.KLT.DINV.WD.GD.ZS +
				EG.GDP.PUSE.KO.PP + EG.USE.ELEC.KH.PC + EN.ATM.CO2E.PC +
				FM.LBL.MQMY.GD.ZS + IC.CRD.PRVT.ZS + NE.TRD.GNFS.ZS, data = kaggle.data[variable.names])

summary(rlm(Corruption.Index ~ ., kaggle.data[variable.names]))

model.full.sum

fstats<-numeric(4000)
for(i in 1:4000){
	g<-model.full<-lm(sample(Corruption.Index)~.,kaggle.data[variable.names])
	fstats[i]<-summary(g)$fstat[1]
}
length(fstats[fstats > 5.688])/4000

library(WDI)
WDIcache()


summary(lm(abs(residuals(model.full)) ~ fitted(model.full)))


median(kaggle.data$Corruption.Index)
sum(kaggle.data$Corruption.Index>50)

jack <- rstudent(model.full)
sorted_idx<-sort(abs(jack),decreasing = T,index.return=T)$ix
max<-sorted_idx[1]
sec_max<-sorted_idx[2]
jack[max]
jack[sec_max]
qt(.05/(nrow(kaggle.data)*2),nrow(kaggle.data)-length(model.full$coefficients)-1)

ginf<-influence(model.full)
plot(ginf$coef[,2],ylab="Change in AG.LND.AGRI.K2 coef")
identify(1:nrow(kaggle.data),ginf$coef[,2],kaggle.data$country)

model.full.sum$coefficients["EN.ATM.CO2E.KD.GD",]
form<-as.formula(paste("Corruption.Index~",paste(all.predictor.names[all.predictor.names!="EN.ATM.CO2E.KD.GD"],collapse = "+"),"+log(EN.ATM.CO2E.KD.GD)"))
model.log<-lm(form,kaggle.data[variable.names])
model.log.sum<-summary(model.log)
model.log.sum$coefficients["log(EN.ATM.CO2E.KD.GD)",]
model.full<-lm(Corruption.Index~.,kaggle.data[variable.names])


model.full.sum$coefficients["AG.YLD.CREL.KG",]
scaled<-as.formula(paste("Corruption.Index~",paste(all.predictor.names[all.predictor.names!="AG.YLD.CREL.KG"],collapse = "+"),"+I(AG.YLD.CREL.KG/1000)"))
model.scaled<-lm(scaled,kaggle.data[variable.names])
model.scaled.sum<-summary(model.scaled)
model.scaled.sum$coefficients["I(AG.YLD.CREL.KG/1000)",]


model.scaled<-lm(Corruption.Index~.,data.frame(scale(kaggle.data[variable.names])))

lm(formula = Corruption.Index ~ AG.LND.AGRI.K2 + AG.LND.ARBL.HA.PC +
				AG.LND.ARBL.ZS + AG.LND.CROP.ZS + AG.PRD.CROP.XD + AG.PRD.LVSK.XD +
				AG.YLD.CREL.KG + BM.GSR.INSF.ZS + BM.GSR.TRVL.ZS + BX.GSR.CMCP.ZS +
				BX.KLT.DINV.WD.GD.ZS + EG.GDP.PUSE.KO.PP + EG.USE.COMM.KT.OE +
				EG.USE.ELEC.KH.PC + EN.ATM.CO2E.KD.GD + EN.ATM.CO2E.PC +
				EN.ATM.PM10.MC.M3 + ER.H2O.INTR.K3 + ER.H2O.INTR.PC + FS.AST.PRVT.GD.ZS +
				IC.CRD.PRVT.ZS + IC.EXP.DURS + IC.LGL.CRED.XQ + NE.RSB.GNFS.ZS +
				NE.TRD.GNFS.ZS, data = kaggle.data[variable.names])

sd(kaggle.data$BX.KLT.DINV.WD.GD.ZS)

sd(apply(kaggle.data[all.predictor.names],1, function(case){sqrt(mahalanobis(case,colMeans(kaggle.data[all.predictor.names]),cov(kaggle.data[all.predictor.names])))}))

form<-as.formula(paste("Corruption.Index~",paste( names(model.backward.t.test$coefficients)[-1],collapse = "+")))

model.vif.wls<-lm(form,kaggle.data[variable.names],weights = 1/model.vif$residuals^2)
summary(model.vif.wls)
summary(model.vif)
model.backward.t.test.sum
predict(model.vif.wls,GB.2007.predictors,interval=c("prediction"))

vif.formula<-as.formula(paste("Corruption.Index~",paste(names(model.vif$coefficients)[-1],collapse = "+"),sep=""))
cbind(model.vif$coefficients,rlm(vif.formula, kaggle.data[variable.names])$coef,lqs(vif.formula, kaggle.data[variable.names])$coef)	

step(model.full, direction = c("backward"), k=2, scope=list(lower=.~1),trace=1)
par(mfrow=c(2,1))


h <- lm.influence(model.adjr2)$hat
names(h) <- kaggle.data$country
rev(sort(h))
2*length(model.adjr2$coefficients)/nrow(kaggle.data)

get_formula_log_skew<-function(){
	log.pred<-sapply(all.predictor.names,function(pred){
				if(description[pred,"skew"]>5){
					paste("log(",pred,")",sep="")
				}else{
					pred	
				}
			})
	as.formula(paste("Corruption.Index~",paste(log.pred,collapse="+"),sep=""))
}

model.full.log.pred<-lm(get_formula_log_skew(),kaggle.data[variable.names])
model.full.log.pred.sum<-summary(model.full.log.pred)
model.full.sum

subsets <- regsubsets(get_formula_log_skew(),kaggle.data[variable.names],nvmax=31)


model3 <- lm(fat ~ meatpca$x[,1:4] ,meatspec[1:172,"fat",drop=F] )
svb <- meatpca$rot[,1:4] %*% model3$coef[-1]
plot(svb,ylab="Coefficient")


kaggle.plsr$loadings[,1:11] %*% model3$coef[-1]

kaggle.pcr$loadings[,15]

par(mfrow=c(ceiling(length(variable.names)/5),5),mar = par("mar")/2)
for(var.name in variable.names){
	hist(kaggle.data[[var.name]],main="",ylab="",xlab=var.name)
}
for(var.name in variable.names){
	stripchart(kaggle.data[[var.name]],main=var.name,vertical=TRUE,method="jitter")
}

model.cp.quad.i <- influence(model.cp.quad)

qqnorml(model.cp.quad.i$coef[,1])
halfnorm(cooks.distance(model.cp.quad))
range(rstudent(model.cp.quad))

boxcox(model.cp.quad,plotit=T, lambda=seq(0.4,1.6,by=0.1))	

