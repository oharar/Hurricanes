# This code is what I used to develop hte code in the markdown document, so it's a bit messy. But here it is...

library(gdata)
library(mgcv)

# Read in the data
Data=read.xls("http://www.pnas.org/content/suppl/2014/05/30/1402786111.DCSupplemental/pnas.1402786111.sd01.xlsx", nrows=92, as.is=TRUE)
Data$Category=factor(Data$Category)
Data$Gender_MF=factor(Data$Gender_MF)
Data$ColourMF=c("lightblue", "pink")[as.numeric(Data$Gender_MF)] # the stereotyped colours are being used ironically, of course
BigH=which(Data$alldeaths>100) # Select hurricanes with > 100 deaths

# Plot the numer of deaths against time
plot(Data$Year, Data$alldeaths, col=Data$ColourMF, type="p", pch=15,
     xlab="Year", ylab="Number of Deaths")
text(Data$Year[BigH], Data$alldeaths[BigH], Data$Name[BigH], adj=c(0.8,1.5))
legend(1984, 200, c("Male", "Female"), fill=c("lightblue", "pink"))




# Fit the model used in paper
modJSVH=gam(alldeaths~scale(MasFem)*(scale(Minpressure_Updated.2014)+scale(NDAM)), data=Data, family=negbin(theta=c(0.2,10)))
summary(modJSVH)

# Fit a similar model, but simply using gender as a factor
modJSVH.gender=gam(alldeaths~Gender_MF*(scale(Minpressure_Updated.2014)+scale(NDAM)), data=Data, family=negbin(theta=c(0.2,10)))
summary(modJSVH.gender)

# Plot the residuals 
par(mfrow=c(1,1), mar=c(4.1,4.1,1,1))
plot(log(fitted(modJSVH)), resid(modJSVH), col=Data$ColourMF, ylim=c(min(resid(modJSVH)), 0.5+max(resid(modJSVH))), pch=15,
     xlab="Fitted values", ylab="Residuals", main="Residual plot against fitted values")
text(log(fitted(modJSVH)[BigH]), resid(modJSVH)[BigH], Data$Name[BigH], adj=c(0.7,-0.7))
legend(1984, 200, c("Male", "Female"), fill=c("lightblue", "pink"))

par(mfrow=c(2,1), mar=c(4.1,4.1,1,1))
plot(Data$Minpressure_Updated.2014, resid(modJSVH), col=Data$ColourMF, ylim=c(min(resid(modJSVH)), 0.5+max(resid(modJSVH))), pch=15,
     xlab="Minimum pressure", ylab="Residuals")
text(Data$Minpressure_Updated.2014[BigH], resid(modJSVH)[BigH], Data$Name[BigH], adj=c(0.2,-0.7))
legend(910, 2.8, c("Male", "Female"), fill=c("lightblue", "pink"))

plot((Data$NDAM), resid(modJSVH), col=Data$ColourMF, ylim=c(min(resid(modJSVH)), 0.5+max(resid(modJSVH))), pch=15,
     xlab="Normalized Damage", ylab="Residuals")
text((Data$NDAM[BigH]), resid(modJSVH)[BigH], Data$Name[BigH], adj=c(0.8,-0.7))
legend(4e4, 2.8, c("Male", "Female"), fill=c("lightblue", "pink"))

par(mfrow=c(1,1), mar=c(4.1,4.1,1,1))
plot(sqrt(Data$NDAM), resid(modJSVH), col=Data$ColourMF, ylim=c(min(resid(modJSVH)), 0.5+max(resid(modJSVH))), pch=15,
     xlab="Normalized Damage", ylab="Residuals")
text(sqrt(Data$NDAM[BigH]), resid(modJSVH)[BigH], Data$Name[BigH], adj=c(0.8,-0.7))
legend(2e2, 2.8, c("Male", "Female"), fill=c("lightblue", "pink"))

plot(gam(resid(modJSVH)~s(sqrt(Data$NDAM)), data=Data), ylim=c(min(resid(modJSVH)), 0.5+max(resid(modJSVH))), 
     xlab="Normalized Damage", ylab="Residuals", rug=FALSE, shade=TRUE)
points(sqrt(Data$NDAM), resid(modJSVH), col=Data$ColourMF, pch=15)

# Add a squared term
modJSVH.sq=gam(alldeaths~scale(MasFem)*(scale(Minpressure_Updated.2014)+scale(sqrt(NDAM))+scale(I(sqrt(NDAM)^2))), data=Data, family=negbin(theta=c(0.2,10)))
summary(modJSVH.sq)

plot(sqrt(Data$NDAM), resid(modJSVH.sq), col=Data$ColourMF, ylim=c(min(resid(modJSVH.sq)), 0.5+max(resid(modJSVH.sq))), pch=15)
text(sqrt(Data$NDAM[BigH]), resid(modJSVH.sq)[BigH], Data$Name[BigH], adj=c(0.7,-0.7))
legend(2e2, 1.8, c("Male", "Female"), fill=c("lightblue", "pink"))

# Add year to the model: doen't help
modJSVH.sq.year=gam(alldeaths~s(Year)+scale(MasFem)*(scale(Minpressure_Updated.2014)+scale(sqrt(NDAM))+scale(I(sqrt(NDAM)^2))), data=Data, family=negbin(theta=c(0.2,10)))
summary(modJSVH.sq.year)

Data$Minpressure.2014.sc=scale(Data$Minpressure_Updated.2014)
Data$NDAM.sc=scale(Data$NDAM)
Data$MasFem.sc=scale(Data$MasFem)

Data$NDAM.sqrt=sqrt(Data$NDAM)
Data$NDAM.abs=Data$NDAM.sqrt^2
modJSVH.sqrt=gam(alldeaths~MasFem.sc*(Minpressure.2014.sc+NDAM.sqrt +NDAM.abs), data=Data, family=negbin(theta=c(0.2,10)))
summary(modJSVH.sqrt)

modJSVH.sq.year=gam(alldeaths~s(Year)+MasFem.sc*(Minpressure.2014.sc+NDAM.sqrt +NDAM.abs), data=Data, family=negbin(theta=c(0.2,10)))
summary(modJSVH.sq.year)
