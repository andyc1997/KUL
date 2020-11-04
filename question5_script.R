# Question 5: Canonical correlation analysis
rm(list=ls())
library(dplyr)
library(candisc)

# Working directory -------------------------------------------------------
course.path <- 'C:\\Users\\user\\Desktop\\KUL - Mstat\\Multivariate Statistics\\Assignment-1' # Modify if necessary
setwd(course.path)

# Load data ---------------------------------------------------------------
file.name <- 'wvs(1).Rdata'
load(file.name)

# Select and standardize data ---------------------------------------------
f.std <- function(x) (x - mean(x))/sd(x)
v_name <- wvs %>% select(starts_with(c('R_', 'J_', 'CR_'))) %>% colnames
data_value <- wvs %>% filter(country %in% c('Netherlands', 'Malaysia')) %>%
  select(all_of(v_name)) %>% apply(MARGIN = 2, FUN = f.std) %>% as.data.frame
country_lbl <- wvs %>% filter(country %in% c('Netherlands', 'Malaysia')) %>%
  select(country)
# Conduct CCA -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
x <- data_value %>% select(starts_with(c('R_', 'CR_')))
y <- data_value %>% select(starts_with('J_'))

fit_cca <- cancor(x, y)
summary(fit_cca) # Significance test of canonical correlations

n <- 3 # First n canonical variates' loading want to observe
round(fit_cca$structure$X.xscores[, 1:n], digits = 4)
round(fit_cca$structure$Y.yscores[, 1:n], digits = 4)

# Redundancy analysis -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# % of variance in Y being explained by U
par(mfrow = c(1, 2))

redu <- redundancy(fit_cca)
round(redu$Ycan.redun, digits = 4)
barplot(redu$Ycan.redun, ylim = c(0, 0.2), las = 3, col = 'white', border = 'blue',
        main = 'Redundancy of Dependent Variables')

# Cumulative % of variance in Y being explained by U
round(redu$Ycan.redun %>% cumsum, digits = 4)
plot(redu$Ycan.redun %>% cumsum, las = 1, col = 'green',
     main = 'Cumulative Redundancy of \nDependent Variables', ylab = '', xlab = '')
lines(redu$Ycan.redun %>% cumsum, col = 'green')

# Visualization of CCA ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow = c(1, 1))
i <- 2 # Which canonical variates pair want to visualize, Modify here

# Plot command, don't modify this
plot(fit_cca$scores$X[country_lbl == 'Malaysia', i], fit_cca$scores$Y[country_lbl == 'Malaysia', i], 
     las = 1, col = 'blue',
     xlab = paste('u', i), ylab = paste('t', i), main = paste('Canonical Plot - ', i))
points(fit_cca$scores$X[country_lbl == 'Netherlands', i], fit_cca$scores$Y[country_lbl == 'Netherlands', i], 
       las = 1, col = 'red')
legend('bottomright', legend = c('Malaysia', 'Netherlands'), col = c('blue', 'red'),
       pch = 1)
