# Multivariate Statistics - Assignment 2
# Task 4

# Library ----------------------------------------------------------------------
library(ca)

# Import data ------------------------------------------------------------------
setwd('C:\\Users\\user\\Desktop\\KUL - Mstat\\Multivariate Statistics\\Assignment-2')
load('datacar.Rdata')


# 1. Correspondence analysis ----------------------------------------------
chisq.test(datacar) # Chi-squared test for row & col independence
ca_car <- ca(datacar) # Correspondence analysis
summary(ca_car)

# 2. Biplot ---------------------------------------------------------------
# Plot for row principal coordinates
plot(ca_car, map = 'rowprincipal', arrows = c(FALSE, TRUE), 
     main = 'Biplot for Row-principal coordinates')

# Plot for column principal coordinates
plot(ca_car, map = 'colprincipal', arrows = c(FALSE, TRUE), 
     main = 'Biplot for Column-principal coordinates')

# Plot for row + column principal coordinates 
par(cex = 1.2)
plot(ca_car, map = 'symmetric', arrows = c(FALSE, TRUE), 
     main = 'Biplot for Row- and Column-principal coordinates') # Better visualization for report

# Plot for Symbiplot
par(cex = 1.2)
plot(ca_car, map = 'symbiplot', arrows = c(FALSE, TRUE), 
     main = 'Biplot for similarly scaled coordinates')
