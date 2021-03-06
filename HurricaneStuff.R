library(gdata)
library(MASS)

Data=read.xls("http://www.pnas.org/content/suppl/2014/05/30/1402786111.DCSupplemental/pnas.1402786111.sd01.xlsx", nrows=92, as.is=TRUE)
Data$Minpressure.2014.sc=scale(Data$Minpressure_Updated.2014)
Data$NDAM.sc=scale(Data$NDAM)
Data$MasFem.sc=scale(Data$MasFem)

mod3=glm.nb(alldeaths~MasFem*(Minpressure_Updated.2014+I(NDAM^3)), data=Data)
mod2=glm.nb(alldeaths~MasFem*(Minpressure_Updated.2014+I(NDAM^2)), data=Data, init.theta=0.7)
mod1=glm.nb(alldeaths~MasFem*(Minpressure_Updated.2014+NDAM), data=Data)
mod02=glm.nb(alldeaths~MasFem*(Minpressure_Updated.2014+sqrt(NDAM)), data=Data)
modlog=glm.nb(alldeaths~MasFem*(Minpressure_Updated.2014+log(NDAM)), data=Data, init.theta=0.9)
mod02Inv=glm.nb(alldeaths~MasFem*(Minpressure_Updated.2014+I(NDAM^-0.5)), data=Data, init.theta=0.9)
mod2Inv=glm.nb(alldeaths~MasFem*(Minpressure_Updated.2014+I(NDAM^-2)), data=Data)
mod3Inv=glm.nb(alldeaths~MasFem*(Minpressure_Updated.2014+I(NDAM^-3)), data=Data)



cat("Original: ", round(mod1$aic,1), 
    "\nCube: ", round(mod3$aic,1),
    "\nSquare: ", round(mod2$aic,1),
    "\nSquare root: ", round(mod02$aic,1),
    "\nLog: ", round(modlog$aic,1),
    "\nInverse Square root: ", round(mod02Inv$aic,1),
    "\nInverse Square: ", round(mod2Inv$aic,1),
    "\nInverse Cube: ", round(mod3Inv$aic,1), sep="", "\n")


modJSVH=glm.nb(alldeaths~MasFem*(Minpressure_Updated.2014+NDAM), data=Data)
summary(modJSVH)

SimAllDeaths=function(masfem, GLM, dat) {
#ndam=0; GLM=modJSVH; dat=Data
  dat$MasFem=rep(masfem,nrow(dat))
  pred=predict(GLM, newdata=dat, se.fit=TRUE, type="link")
  Mean=exp(rnorm(length(pred$fit), pred$fit, pred$se.fit))
  Sim=rnegbin(Mean, theta=GLM$theta)
  sum(Sim)
}

SimAllDeaths(masfem=mean(Data$MasFem), GLM=modJSVH, dat=Data)

PredMasFem=seq(min(Data$MasFem), max(Data$MasFem), length=30)
# Use if parallel pacakge isn't installed
PredDeaths=as.data.frame(t(sapply(PredMasFem, function(masfem, GLM,dat, nsim) {
  sims=replicate(nsim, SimAllDeaths(masfem, GLM=GLM, dat=dat))
  c(Mean=mean(sims), LCI=quantile(sims, 0.025), UCI=quantile(sims,0.975))
}, GLM=modJSVH, dat=Data, nsim=1e4)))

library(parallel)
PredDeaths.lst=mclapply(PredMasFem, function(masfem, GLM,dat, nsim) {
  sims=replicate(nsim, SimAllDeaths(masfem, GLM=GLM, dat=dat))
  c(Mean=mean(sims), LCI=quantile(sims, 0.025), UCI=quantile(sims,0.975))
}, GLM=modJSVH, dat=Data, nsim=1e4)

PredDeaths=data.frame(
  Mean=unlist(lapply(PredDeaths.lst, function(lst) lst["Mean"])),
  LCI=unlist(lapply(PredDeaths.lst, function(lst) lst["LCI.2.5%"])),
  UCI=unlist(lapply(PredDeaths.lst, function(lst) lst["UCI.97.5%"]))
)

plot(PredMasFem, log(PredDeaths$Mean), type="l", ylim=range(log(PredDeaths)),
     xlab="Feminity Index", ylab="log(Deaths)")
polygon(c(PredMasFem,rev(PredMasFem)), log(c(PredDeaths$LCI, rev(PredDeaths$UCI))), col="grey70", border=NA)
lines(PredMasFem, log(PredDeaths$Mean))
lines(PredMasFem, log(ExPredDeaths), col=2)



plot(PredMasFem, PredDeaths$Mean/1e6, type="l", ylim=c(0,max(PredDeaths$UCI/1e6)),
     xlab="Feminity Index", ylab="Deaths, millions")
polygon(c(PredMasFem,rev(PredMasFem)), c(PredDeaths$LCI, rev(PredDeaths$UCI))/1e6, col="grey70", border=NA)
lines(PredMasFem, PredDeaths$Mean/1e6)

#######################
# Look at joint effects of NDAM & min. pressure

modJSVH.sc=glm.nb(alldeaths~MasFem.sc*(Minpressure.2014.sc+NDAM.sc), data=Data)
summary(modJSVH.sc)
MasFemEff=coef(modJSVH.sc)["MasFem.sc"]+as.matrix(Data[,c("Minpressure.2014.sc", "NDAM.sc")])%*%
  as.matrix(coef(modJSVH.sc)[c("MasFem.sc:Minpressure.2014.sc", "MasFem.sc:NDAM.sc")])


MasFemEff=coef(modJSVH)["MasFem"]+as.matrix(Data[,c("Minpressure_Updated.2014", "NDAM")])%*%
  as.matrix(coef(modJSVH)[c("MasFem:Minpressure_Updated.2014", "MasFem:NDAM")])

par(mfrow=c(1,2), mar=c(4.1,2,3,1), oma=c(0,2,0,0))
plot(Data$Minpressure_Updated.2014, MasFemEff, xlab="Minimum pressure", ylab="", main="Pressure")
abline(lm(MasFemEff~Data$Minpressure_Updated.2014))
plot(Data$NDAM[Data$NDAM<4e4], MasFemEff[Data$NDAM<4e4], xlab="Normalised damage", ylab="", main="Damage")
abline(lm(MasFemEff[Data$NDAM<4e4]~Data$NDAM[Data$NDAM<4e4]))
mtext("Femininity index", 2, outer=TRUE, line=0.5)


