# This code is for a manuscript.

library(gdata)
#library(mgcv)
library(MASS)

# Read in the data
Data=read.xls("http://www.pnas.org/content/suppl/2014/05/30/1402786111.DCSupplemental/pnas.1402786111.sd01.xlsx", nrows=92, as.is=TRUE)
Data$Category=factor(Data$Category)
Data$Gender_MF=factor(Data$Gender_MF)
Data$ColourMF=c("blue", "red")[as.numeric(Data$Gender_MF)] # the stereotyped colours are being used ironically, of course
BigH=which(Data$alldeaths>100) # Select hurricanes with > 100 deaths
Data$MasFem.sc=scale(Data$MasFem)
Data$Minpressure.2014.sc=scale(Data$Minpressure_Updated.2014)
Data$NDAM.sc=scale(Data$NDAM)

# Plot the numer of deaths against time
plot(Data$Year, Data$alldeaths, col=Data$ColourMF, type="p", pch=15,
     xlab="Year", ylab="Number of Deaths")
text(Data$Year[BigH], Data$alldeaths[BigH], Data$Name[BigH], adj=c(0.8,1.5))
legend(1984, 200, c("Male", "Female"), fill=c("blue", "red"))


# Fit the model used in paper
modJSVH=glm.nb(alldeaths~MasFem.sc*(Minpressure.2014.sc+NDAM.sc), data=Data)
summary(modJSVH)

# Fit a similar model, but simply using gender as a factor
modJSVH.gender=glm.nb(alldeaths~Gender_MF*(Minpressure.2014.sc+NDAM.sc), data=Data)
summary(modJSVH.gender)

# Plot the residuals 
par(mfrow=c(1,1), mar=c(4.1,4.1,1,1))
plot(log(fitted(modJSVH)), resid(modJSVH), col=Data$ColourMF, ylim=c(min(resid(modJSVH)), 0.5+max(resid(modJSVH))), pch=15,
     xlab="Fitted values", ylab="Residuals", main="Residual plot against fitted values")
text(log(fitted(modJSVH)[BigH]), resid(modJSVH)[BigH], Data$Name[BigH], adj=c(0.7,-0.7))
legend(1984, 200, c("Male", "Female"), fill=c("blue", "red"))

par(mfrow=c(2,1), mar=c(4.1,4.1,1,1))
plot(Data$Minpressure_Updated.2014, resid(modJSVH), col=Data$ColourMF, ylim=c(min(resid(modJSVH)), 0.5+max(resid(modJSVH))), pch=15,
     xlab="Minimum pressure", ylab="Residuals")
text(Data$Minpressure_Updated.2014[BigH], resid(modJSVH)[BigH], Data$Name[BigH], adj=c(0.2,-0.7))
legend(910, 2.8, c("Male", "Female"), fill=c("blue", "red"))

plot((Data$NDAM), resid(modJSVH), col=Data$ColourMF, ylim=c(min(resid(modJSVH)), 0.5+max(resid(modJSVH))), pch=15,
     xlab="Normalized Damage", ylab="Residuals")
text((Data$NDAM[BigH]), resid(modJSVH)[BigH], Data$Name[BigH], adj=c(0.8,-0.7))
legend(4e4, 2.8, c("Male", "Female"), fill=c("blue", "red"))

par(mfrow=c(1,1), mar=c(4.1,4.1,1,1))
plot(sqrt(Data$NDAM), resid(modJSVH), col=Data$ColourMF, ylim=c(min(resid(modJSVH)), 0.5+max(resid(modJSVH))), pch=15,
     xlab="Normalized Damage", ylab="Residuals")
text(sqrt(Data$NDAM[BigH]), resid(modJSVH)[BigH], Data$Name[BigH], adj=c(0.8,-0.7))
legend(2e2, 2.8, c("Male", "Female"), fill=c("blue", "red"))


plot(gam(resid(modJSVH)~s(sqrt(Data$NDAM)), data=Data), ylim=c(min(resid(modJSVH)), 0.5+max(resid(modJSVH))), 
     xlab="Normalized Damage", ylab="Residuals", rug=FALSE, shade=TRUE)
points(sqrt(Data$NDAM), resid(modJSVH), col=Data$ColourMF, pch=15)
text(sqrt(Data$NDAM[BigH]), resid(modJSVH)[BigH], Data$Name[BigH], adj=c(0.8,-0.7))
legend(2e2, 2.8, c("Male", "Female"), fill=c("blue", "red"))

# Add a squared term
# Fit the model used in paper
modJSVH.sq=glm.nb(alldeaths~MasFem.sc*(Minpressure.2014.sc+NDAM.sc+I(NDAM.sc^2)), data=Data)
summary(modJSVH.sq)
modJSVH.gender.sq=glm.nb(alldeaths~Gender_MF*(Minpressure.2014.sc+NDAM.sc+I(NDAM.sc^2)), data=Data)
summary(modJSVH.gender.sq)

# Use sqrt(NDAM)
Data$NDAMsqrt=sqrt(Data$NDAM)
Data$NDAMsqrt.sc=scale(Data$NDAMsqrt)

# Original model, but with sqrt(NDAM)
modJSVH.sqrt=glm.nb(alldeaths~MasFem.sc*(Minpressure.2014.sc+NDAMsqrt.sc), data=Data)
summary(modJSVH.sqrt)
# Add quadratic term (-> abs(NDAM))
modJSVH.sqrt.sq=glm.nb(alldeaths~MasFem.sc*(Minpressure.2014.sc+NDAMsqrt.sc+NDAM.sc), data=Data)
summary(modJSVH.sqrt.sq)

# Fit a similar model, but simply using gender as a factor
modJSVH.gender.sqrt=glm.nb(alldeaths~Gender_MF*(Minpressure.2014.sc+NDAMsqrt.sc), data=Data)
summary(modJSVH.gender.sqrt)
modJSVH.gender.sqrt.sq=glm.nb(alldeaths~Gender_MF*(Minpressure.2014.sc+NDAMsqrt.sc+NDAM.sc), data=Data)
summary(modJSVH.gender.sqrt.sq)

# remove gender terms
modJSVH.sqrt.smaller=glm.nb(alldeaths~(Minpressure.2014.sc+NDAMsqrt.sc+I(NDAMsqrt.sc^2)), data=Data)
summary(modJSVH.sqrt.smaller)

# AICs
cat("AICs\nOriginal Model:  ", modJSVH$aic, "\nModel with squared term:  ", modJSVH.sq$aic, 
    "\nModel with sqrt(NDAM):  ", modJSVH.sqrt$aic, "\nModel with sqrt(NDAM)+NDAM:  ", modJSVH.sqrt.sq$aic, 
    "\nModel without gender effects:  ", modJSVH.sqrt.smaller$aic, sep="")
# Models with just Gender
cat("AICs for Models with just Gender\nOriginal Model:  ", modJSVH.gender$aic, "\nModel with squared term:  ", modJSVH.gender.sq$aic, 
    "\nModel with sqrt(NDAM):  ", modJSVH.gender.sqrt$aic, "\nModel with sqrt(NDAM)+NDAM:  ", modJSVH.gender.sqrt.sq$aic, 
    "\nModel without gender effects:  ", modJSVH.sqrt.smaller$aic, sep="")

