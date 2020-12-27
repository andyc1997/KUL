rm(list = ls())
setwd("D:/Download/KUL/Multivariate/1-assignment/2")

#install.packages("smacof")
library(smacof)
load("confusion.Rdata")
load("dissim.Rdata")

sim<-(confusion+t(confusion))/2
dissim<-100-sim
for (i in 1:36){dissim[i,i]<-0} #why do this?
# Wai Chun: It is because MDS takes distance matrix (representing dissimilarity) in a high dimension as input and return a set of coordinates in a low dimension as output.
# It is also called 'low dimensional embedding'. So, the distance with itself (located in the diagonal of dist matrix) should be 0.

# 1. MDS with 2 dim different transformations --------------------------------------------
transformations.code <- c('ratio', 'interval', 'ordinal', 'mspline')

##ratio
m1<-smacofSym(delta=dissim, ndim=2, type=transformations.code[1], init="torgerson")
##interval
m2<-smacofSym(delta=dissim, ndim=2, type=transformations.code[2], init="torgerson")
##ordinal
m3<-smacofSym(delta=dissim, ndim=2, type=transformations.code[3], init="torgerson")
##Monotone spline 
m4<-smacofSym(delta=dissim, ndim=2 ,type=transformations.code[4],spline.degree =4 ,
              spline.intKnots = 4, init="torgerson") #why degree and intKnots equal to 4
# Wai Chun: Just convention. It is because if the spline has too high degree -> Runge phenonmen. Too less --> Bad fit.

#stress-1 values
signal_stress <- c(m1$stress, m2$stress, m3$stress, m4$stress)
round(signal_stress,3) # Output to Task 5 - Question 1

##residual plot and Shepard plot for ordinal MDS
par(mfrow=c(1,2))
plot(m3,plot.type="resplot",main="residual plot ordinal MDS")
plot(m3,plot.type="Shepard",main="Shepard diagram ordinal MDS")

##residual plot and Shepard plot for spline MDS
par(mfrow=c(1,2))
plot(m4,plot.type="resplot",main="residual plot ordinal MDS")
plot(m4,plot.type="Shepard",main="Shepard diagram ordinal MDS")


# 2. Goodness-of-fit ------------------------------------------------------

getStress <- function(model, code){
  # Stress norm
  set.seed(202012)
  rstress <- randomstress(n = 36, ndim = 2, nrep = 500, type = code)
  threshold <- mean(rstress) - 2*sd(rstress)
  
  # Permutation test
  set.seed(202012)
  perm <- permtest(model, nrep = 500)
  
  # Return all outputs
  return(list(rstress = rstress, threshold = threshold, perm = perm))
}

m1.stability <- getStress(m1, transformations.code[1])
m2.stability <- getStress(m2, transformations.code[2])
m3.stability <- getStress(m3, transformations.code[3])
m4.stability <- getStress(m4, transformations.code[4])

stability <- list(m1.stability, m2.stability, m3.stability, m4.stability)

# Criterion 1 - Stress-1 < mean(rstress) - 2*sd(rstress)?
for (i in 1:4){
  cat('\n', transformations.code[i], ' MDS - Threshold: ', stability[[i]]$threshold, 
      ' MDS - Stress-1: ', signal_stress[i])
} # All pass the test

# Criterion 2 - Stress distribution
for (i in 1:4){
  par(mfrow = c(1, 2))
  hist(stability[[i]]$rstress, 
       main = paste('Random stress: ', transformations.code[i]), 
       xlab = 'Random stress')
  hist(stability[[i]]$perm$stressvec, 
       main = paste('Stress permuted data: ', transformations.code[i]), 
       xlab = 'Permuted data')
}

###stability of solution using jackknife
jack.dissim<-jackmds(m3)
jack.dissim
plot(jack.dissim)

# 3. External variables ---------------------------------------------------
signal <- row.names(confusion); sb <- substr(row.names(confusion)[1], 1, 1) # Extract for the character representing short beeps
signal_length <- sapply(signal, nchar) # Length of signal
signal_prop_shortb <- sapply(signal, function(x) lengths(regmatches(x, gregexpr(sb, x))))
signal_prop_shortb <- signal_prop_shortb / signal_length
signal_begin_shortb <- sapply(signal, function(x) substring(x, 1, 1) == sb) # If signal begin with short beeps

# Dataframe for external variables
exter_var <- data.frame(signal_length = signal_length,
                        signal_prop_shortb = signal_prop_shortb,
                        signal_begin_shortb = signal_begin_shortb)

# Create a bitplot for external variables
signal_biplot <- biplotmds(m3, exter_var)
plot(signal_biplot, main = 'Biplot of MDS model',
     xlim = c(-1.2, 1.2), vec.conf = list(col = 'blue'), vecscale = 0.6, 
     col = 'red', cex = 0.5, pch = 2, label.conf = list(label = TRUE, cex = 1.5)) # Output to Task 5 - Question 3
