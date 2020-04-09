####################################################################################
##################### Hierarchical Regression Power Analysis #######################
####################################################################################

#####
# Date: TODAY'S DATE
# By: *INSERT NAME HERE*
# Description: PROJECT DISCRIPTION
# Version of R used: CURRENT VERSION OF R


#####################################
### Import data & load libraries ####
#####################################

## Install the required script packages if not yet installed

# Install pacman, jmv, & reghelper package if necessary
if(!"pacman" %in% rownames(installed.packages())) install.packages("pacman")

pacman::p_load(pwr2ppl)

# NB: Alt + Shift + K --> will bring up keyboard shortcuts

######################################################
###### Complete with information for your study ######
######################################################

# Model info to test power:

model  <- .2379  # Estimated full model adj R2
change <- .0466  # Estimated change in adj R2 added by interaction
pred   <- 2      # Number of predictors, for a 2x2 study, this number = 2
c.pred <- 1      # Number of predictors added with interaction. For 2x2, this number = 1
low    <- 150    # Min sample size to test, change to be appropriate for your design
high   <- 300    # Max sample size to test, change to be appropriate for your design
incre  <- 10     # Interval increase in sample size to test, e.g., 280, 285, 290, 295

#############################################
###### RUN BUT DO NOT MODIFY!! ##############
#############################################

regintR2(R2Mod = model, 
         R2Ch = change, 
         mod_pred = pred, 
         ch_pred = c.pred, 
         alpha = 0.05,
         nlow = low, 
         nhigh = high, 
         by = incre)

