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
if (!requireNamespace("pacman", quietly = TRUE)) {install.packages("pacman")}
pacman::p_load(pwr2ppl)

# NB: Alt + Shift + K --> will bring up keyboard shortcuts

######################################################
###### Complete with information for your study ######
######################################################

# Model info to test power:

# Model parameters
r2_full      <- 0.2379  # R2Mod: Estimated full model R^2 (main effects + interaction)
r2_change    <- 0.0466  # R2Ch:  Estimated R^2 change contributed by interaction term
n_full_preds <- 3       # mod_pred: Total predictors in full model (2 main effects + 1 interaction = 3)
n_ch_preds   <- 1       # ch_pred: Predictors added in interaction step (typically 1)
alpha_level  <- 0.05    # alpha: Significance threshold (Type I error rate)

# Sample size evaluation range
sample_low   <- 150     # nlow:  Minimum sample size to evaluate
sample_high  <- 300     # nhigh: Maximum sample size to evaluate
sample_step  <- 10      # by:    Increment step size (e.g., 150, 160, 170...)

#########################################################################
###### 3. Execute Power Analysis (Run but do not modify) ################
#########################################################################

power_results <- pwr2ppl::regintR2(
                    R2Mod    = r2_full,
                    R2Ch     = r2_change,
                    mod_pred = n_full_preds,
                    ch_pred  = n_ch_preds,
                    nlow     = sample_low,
                    nhigh    = sample_high,
                    by       = sample_step,
                    alpha    = alpha_level)

