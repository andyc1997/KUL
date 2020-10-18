# Confirmatory factor analysis (CFA)
rm(list=ls())
library(dplyr)
library(lavaan)

# Working directory -------------------------------------------------------
course.path <- 'C:\\Users\\user\\Desktop\\KUL - Mstat\\Multivariate Statistics\\Assignment-1' # Modify if necessary
setwd(course.path)

# Load data ---------------------------------------------------------------
file.name <- 'wvs(1).Rdata'
load(file.name)

# Select data ---------------------------------------------
data_cfa <- wvs %>% filter(country %in% c('Netherlands', 'Malaysia')) %>% 
  select(starts_with('J_')) %>% as.data.frame

# Confirmatory Factor Analysis ----------------------------------------
n <- nrow(data_cfa)
cov_mat <- cov(data_cfa)
cfa_relation <- 'fraudulent =~ NA*J_claiming_benefits + J_avoiding_fare +
                J_stealing_property + J_cheating_taxes + J_accept_bribe
                marriage =~ NA*J_homosexuality + J_prostitution + J_abortion + 
                J_divorce + J_sex_before_marriage + J_suicide
                violent =~ NA*J_beat_wife + J_parents_beating_children + J_violence
                fraudulent ~~ 1*fraudulent
                marriage ~~ 1*marriage
                violent ~~ 1*violent
                fraudulent ~~ violent
                marriage ~~ violent'

fit_cfa <- cfa(cfa_relation, sample.cov = cov_mat, sample.nobs = n)
summary(fit_cfa, fit.measures = TRUE)
standardizedSolution(fit_cfa)
modificationIndices(fit_cfa)
#######################################
# To be continued                     #
#######################################

