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

# latent variable definitions

religion =~ R_attend_religious_services +  R_pray  + R_importance_God

fraudulent =~ NA*J_claiming_benefits + J_avoiding_fare +
  J_stealing_property + J_cheating_taxes + J_accept_bribe
  
marriage =~ NA*J_homosexuality + J_prostitution + J_abortion + 
  J_divorce + J_sex_before_marriage + J_suicide
  
violent =~ NA*J_beat_wife + J_parents_beating_children + J_violence + J_suicide

fraudulent ~~ 1*fraudulent
marriage ~~ 1*marriage
violent ~~ 1*violent
fraudulent ~~ violent
marriage ~~ violent
J_homosexuality ~~ J_sex_before_marriage


# regressions 

 fraudulent ~ religion
 marriage ~ religion
 violent ~ religion'

fitsem1 <- sem(sem1, data = data_sem)

fitmeasures(fitsem1,c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))

summary(fitsem1, fit.measures = TRUE)


standardizedSolution(fitsem1) # Standardized solution & Reliability

fit_sem1.mi <- modificationIndices(fitsem1) # modification indices (mi)

fit_sem1.mi[order(fit_sem1.mi$mi, decreasing = TRUE),] # rank relationship by decreasing mi



fscores<-lavPredict(fitsem1,method="regression") 

reliability.religion <- (0.817^2 + 0.911^2 + 0.888^2)^2
reliability.religion <- reliability.religion/(reliability.religion + 0.332 + 0.169 + 0.212) 

reliability.fraudulent <- (0.789^2 + 0.771^2 + 0.899^2 + 0.769^2 + 0.838^2)^2
reliability.fraudulent <- reliability.fraudulent/(reliability.fraudulent + 0.377 + 0.406 + 0.192 + 0.409 + 0.298)

reliability.marriage <- (0.859^2 + 0.790^2 + 0.926^2 + 0.910^2 + 0.908^2 + 0.693^2)
reliability.marriage <- reliability.marriage/(reliability.marriage + 0.262 + 0.376 + 0.143 + 0.171 + 0.176 + 0.452)

reliability.violent <- (0.897^2 + 0.724^2 + 0.879^2 + 0.208^2)
reliability.violent <- reliability.violent/(reliability.violent + 0.196 + 0.475 + 0.228 + 0.475)

cat('\nReliability Fraudulent:', reliability.fraudulent,
    '\nReliability Marriage:', reliability.marriage,
    '\nReliability Violent:', reliability.violent,
    '\nReliability Religion:', reliability.religion)


# Multigroup analysis -----------------------------------------------------
data_cfa_multigroup <- wvs %>% filter(country %in% c('Netherlands', 'Malaysia')) %>% 
  select(starts_with(c('R_', 'J_')), 'country') %>% as.data.frame
cfa_multigroup_relation <- 'religion =~ R_attend_religious_services +  R_pray  + R_importance_God
                fraudulent =~ 1*J_claiming_benefits + J_avoiding_fare +
                J_stealing_property + J_cheating_taxes + J_accept_bribe
                marriage =~ 1*J_homosexuality + J_prostitution + J_abortion + 
                J_divorce + J_sex_before_marriage + J_suicide
                violent =~ 1*J_beat_wife + J_parents_beating_children + J_violence + J_suicide
                
                
                fraudulent ~~ fraudulent
                marriage ~~ marriage
                violent ~~ violent
                fraudulent ~~ violent
                marriage ~~ violent
                J_homosexuality ~~ J_sex_before_marriage
                
                
                # regressions 

              fraudulent ~ religion
              marriage ~ religion
              violent ~ religion'

#Differences in two countries
fit_sem_config <- sem(cfa_multigroup_relation, data = data_cfa_multigroup, 
                      group = 'country')
summary(fit_sem_config, fit.measures = TRUE)
std_sol <- standardizedSolution(fit_sem_config)
std_sol[std_sol$group==1,] # Malaysia
std_sol[std_sol$group==2,] # The Netherlands

                
                
                
                
                
                
                
                
