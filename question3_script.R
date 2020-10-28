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
# Structure specified from EFA
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
                marriage ~~ violent
                '

fit_cfa <- cfa(cfa_relation, sample.cov = cov_mat, sample.nobs = n)
fitmeasures(fit_cfa, c('cfi', 'tli', 'srmr', 'rmsea', 'chisq', 'df', 'pvalue'))
summary(fit_cfa) # loadings
standardizedSolution(fit_cfa) # Standardized solution & Reliability
fit_cfa.mi <- modificationIndices(fit_cfa) # modification indices (mi)
fit_cfa.mi[order(fit_cfa.mi$mi, decreasing = TRUE),] # rank relationship by decreasing mi

# Update model structure by modification indices
cfa_relation <- 'fraudulent =~ NA*J_claiming_benefits + J_avoiding_fare +
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
                '

fit_cfa <- cfa(cfa_relation, sample.cov = cov_mat, sample.nobs = n)
fitmeasures(fit_cfa, c('cfi', 'tli', 'srmr', 'rmsea', 'chisq', 'df', 'pvalue'))
summary(fit_cfa) # loadings
standardizedSolution(fit_cfa) # Standardized solution & Reliability
fit_cfa.mi <- modificationIndices(fit_cfa) # modification indices (mi)
fit_cfa.mi[order(fit_cfa.mi$mi, decreasing = TRUE),] # rank relationship by decreasing mi

# Reliability of factors
reliability.fraudulent <- (0.789^2 + 0.771^2 + 0.899^2 + 0.769^2 + 0.838^2)^2
reliability.fraudulent <- reliability.fraudulent/(reliability.fraudulent + 0.377 + 0.406 + 0.192 + 0.409 + 0.298)

reliability.marriage <- (0.857^2 + 0.792^2 + 0.926^2 + 0.914^2 + 0.901^2 + 0.693^2)
reliability.marriage <- reliability.marriage/(reliability.marriage + 0.265 + 0.373 + 0.142 + 0.164 + 0.189 + 0.451)

reliability.violent <- (0.894^2 + 0.723^2 + 0.882^2 + 0.207^2)
reliability.violent <- reliability.violent/(reliability.violent + 0.201 + 0.478 + 0.222 + 0.451)

cat('\nReliability Fraudulent:', reliability.fraudulent,
    '\nReliability Marriage:', reliability.marriage,
    '\nReliability Violent:', reliability.violent)

# Discriminant validity
standardizedSolution(fit_cfa)[c(19, 20, 36),]

# Multigroup analysis -----------------------------------------------------
data_cfa_multigroup <- wvs %>% filter(country %in% c('Netherlands', 'Malaysia')) %>% 
  select(starts_with('J_'), 'country') %>% as.data.frame
cfa_multigroup_relation <- 'fraudulent =~ 1*J_claiming_benefits + J_avoiding_fare +
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
                '
# Configural measurement invariance model
fit_cfa_config <- cfa(cfa_multigroup_relation, data = data_cfa_multigroup, 
                      group = 'country')
summary(fit_cfa_config, fit.measures = TRUE)
standardizedSolution(fit_cfa_config)

# Metric measurement invariance model
fit_cfa_metric <- cfa(cfa_multigroup_relation, data = data_cfa_multigroup, 
                      group = 'country', group.equal = 'loadings')
summary(fit_cfa_metric, fit.measures = TRUE)
standardizedSolution(fit_cfa_metric)

# Strong measurement invariance model
fit_cfa_strong<- cfa(cfa_multigroup_relation, data = data_cfa_multigroup, 
                      group = 'country', group.equal = c('loadings', 'intercepts'))
summary(fit_cfa_strong, fit.measures = TRUE)
standardizedSolution(fit_cfa_strong)

# Likelihood ratio tests
anova(fit_cfa_config, fit_cfa_metric)
anova(fit_cfa_config, fit_cfa_strong)