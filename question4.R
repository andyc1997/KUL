# Confirmatory factor analysis (CFA)
rm(list=ls())
library(dplyr)
library(lavaan)

# Working directory -------------------------------------------------------
setwd("~/multivariate")

# Load data ---------------------------------------------------------------
load("~/multivariate/wvs(3).Rdata")
# Select data ---------------------------------------------
data_sem <- wvs %>% filter(country %in% c('Netherlands', 'Malaysia')) %>% 
    select(starts_with('R_')| starts_with('J_') ) %>% as.data.frame
# Structural equation model  ----------------------------------------

# latent variable definitions

sem1 <- '

 religion =~ R_attend_religious_services +  R_pray  + R_importance_God
 
 fraudulent =~ J_claiming_benefits + J_avoiding_fare +
                J_stealing_property + J_cheating_taxes + J_accept_bribe
                
 marriage =~ J_homosexuality + J_prostitution + J_abortion + 
                J_divorce + J_sex_before_marriage + J_suicide
 violent =~ J_beat_wife + J_parents_beating_children + J_violence + J_suicide
 

 religion =~ fraudulent
 religion =~ marriage
 religion =~ violent'

 fitsem1 <- sem(sem1, data = data_sem)

fitmeasures(fitsem1,c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))

summary(fitsem1, fit.measures = TRUE)


standardizedSolution(fitsem1) # Standardized solution & Reliability

fit_sem1.mi <- modificationIndices(fitsem1) # modification indices (mi)

fit_sem1.mi[order(fit_sem1.mi$mi, decreasing = TRUE),] # rank relationship by decreasing mi        
                
                
                
                
                
                
                
                
