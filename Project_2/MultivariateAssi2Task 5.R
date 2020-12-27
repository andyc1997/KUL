rm(list = ls())
setwd("D:/Download/KUL/Multivariate/1-assignment/2")

#install.packages("smacof")
library(smacof)
load("confusion.Rdata")
load("dissim.Rdata")

sim<-(confusion+t(confusion))/2
dissim<-100-sim
for (i in 1:36){dissim[i,i]<-0} #why do this?

#conduct MDS disparities

##ratio
m1<-smacofSym(delta=dissim, ndim=2, type="ratio", init="torgerson")
##interval
m2<-smacofSym(delta=dissim, ndim=2, type="interval", init="torgerson")
##ordinal
m3<-smacofSym(delta=dissim, ndim=2, type="ordinal", init="torgerson")
##Monotone spline 
m4<-smacofSym(delta=dissim, ndim=2 ,type="mspline",spline.degree =4 ,
              spline.intKnots = 4, init="torgerson") #why degree and intKnots equal to 4

#stress-1 values
round(c(m1$stress,m2$stress,m3$stress,m4$stress),3) 

##residual plot and Shepard plot for ordinal MDS
par(mfrow=c(1,2))
plot(m3,plot.type="resplot",main="residual plot ordinal MDS")
plot(m3,plot.type="Shepard",main="Shepard diagram ordinal MDS")

#configuration ordinal MDS
plot(m3,plot.type="conf") 

#computing stress norms

##stress for random data
set.seed(202012)
rstress<-randomstress(n=36,ndim=2,nrep=500,type="ordinal")
mean(rstress)-2*sd(rstress)

##permutation test
set.seed(202012)
perm.dissim<-permtest(m3,nrep=500) 

###plot distribution stress
par(mfrow=c(1,2),pty="s")
hist(rstress,main="stress random data")
hist(perm.dissim$stressvec,main="stress permuted data") 

###stability of solution using jackknife
jack.dissim<-jackmds(m3)
jack.dissim
plot(jack.dissim)

#conduct ordinal MDS analysis
fitdissim <- mds(dissim, type = "ordinal")

#compute MDS biplot
dissimScale<-scale(dissim,center = TRUE,scale=TRUE)
bidissim <- biplotmds(fitdissim, dissimScale) 

# project external variables in the MDS solution
plot(bidissim, main = "Biplot Vector Representation", vecscale = 0.8,
     vec.conf = list(col = "brown"), pch = 20, cex = 0.5)

cor(fitdissim$conf,dissimScale)
