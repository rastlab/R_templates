######################################################################
#################### Confirmatory Factor Analysis ####################
######################################################################

# Date: TODAY'S DATE
# By: *INSERT NAME HERE*
# Description: PROJECT DISCRIPTION
# Version of R used: CURRENT VERSION OF R

#####################################
### Import data & load libraries ####
#####################################

## update packages then install these packages if not yet installed
# rm(list = ls())
update.packages(ask = FALSE, checkBuilt = TRUE)
if(!require(pacman)){install.packages("pacman")}
pacman::p_load(rio, tidyverse, lavaan)

## load data
dat <- import(file.choose())

# check to see that you loaded the correct dataset
View(dat)

# list variables in dataset
glimpse(dat)

# NB: Alt + Shift + K --> will bring up keyboard shortcuts

###############################
########## Data Prep ##########
###############################

## CFA instructions for lavaan can be found: http://lavaan.ugent.be/tutorial/cfa.html
## cheatsheet here too: https://github.com/jeromyanglim/lavaan-examples/blob/master/cheat-sheet-lavaan/cheat-sheet-lavaan.md


# create new dataframes for just the focal CFA
dat1 = dat %>% 
              select(item1, item2, item3, item4, item5)

# create more dataframes if multiple CFAs

dat2 = dat %>% 
              select(item1, item2, item3, item4, item5, item6, item7, item8, item9, item10)


# check dataframes 
glimpse(dat1)
glimpse(dat2)


### lavaan ----

## cereate models

# specify model
five_item_model <-  ' factor1  =~ item1 + item2 + item3 + item4 + item5 '

two_factor_model <- ' factor1  =~ item1 + item2 + item3 + item4 + item5  
                      factor2  =~ item6 + item7 + item8 + item9 + item10 '


saturated_model <- ' factor1  =~ item1 + item2 + item3 + item4 + item5 +
                                 item6 + item7 + item8 + item9 + item10 '



## fit model

# might take a minute or two to run depending on amount of resamples

fit_five_item        <- cfa(five_item_model,  data = dat1, mimic = 'Mplus')
fit_two_factor_model <- cfa(two_factor_model, data = dat2, mimic = 'Mplus')
fit_saturated_model  <- cfa(saturated_model,  data = dat2, mimic = 'Mplus')



## fit measures ----

# fit summary for 1 factor, 5 item model
summary(fit_five_item, fit.measures=TRUE)
round(fitMeasures(fit_five_item)[c('chisq', 'df', 'pvalue', 'cfi', 'ifi', 'tli', 'rmsea', 'srmr')], 3)
# fitMeasures(fit_five_item) # could also run this

# fit summary for 2 factor model
summary(fit_two_factor_model, fit.measures=TRUE)
round(fitMeasures(fit_two_factor_model)[c('chisq', 'df', 'pvalue', 'cfi', 'ifi', 'tli', 'rmsea', 'srmr')], 3)

# fit summary for fully saturated, 1 factor model
summary(fit_saturated_model, fit.measures=TRUE)
round(fitMeasures(fit_saturated_model)[c('chisq', 'df', 'pvalue', 'cfi', 'ifi', 'tli', 'rmsea', 'srmr')], 3)


## compare models, if needed
# produces Chi-square difference test
# can only compare models with the same items but different df

# compare 2 factor (10 item) model vs fully saturated model
anova(fit_two_factor_model, fit_saturated_model)