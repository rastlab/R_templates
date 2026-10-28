#######################################################################################
######## Moderated Mediation Analysis (Model 7 from Hayes' SPSS Process Macro) ########
#######################################################################################

# Date: TODAY'S DATE
# By: *INSERT NAME HERE*
# Description: PROJECT DISCRIPTION
# Version of R used: CURRENT VERSION OF R

#####################################
### Import data & load libraries ####
#####################################

## Install the required script packages if not yet installed

# Install pacman package if necessary
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(rio, tidyverse, effectsize, lavaan, janitor, here)

## load data

# RData/RDS files work the best in R. 
# Try to only open RData/RDS files to avoid any issues.
# CSV works the next best in R. 
# Try to only save and open CSV files to avoid any issue if you cannot load RData/RDS files.
# SPSS files can be buggy to import, especially factors and labels

# the following command will open a dialog box and allow you to select the file you wish to laod
dat <- import(file.choose())

# check to see that you loaded the correct dataset
View(dat)

# list variables in dataset
glimpse(dat1)

# NB: Alt + Shift + K --> will bring up keyboard shortcuts

###############################
########## Data Prep ##########
###############################

# Hayes' template with all models can be found here:
# http://afhayes.com/public/templates.pdf
# this script is modified from: https://nickmichalak.blogspot.ca/2016/07/reproducing-hayess-process-models.html

# going to run Hayes's PROCESS Model 7: moderated mediation
# x*w -> m -> y

# I've tried to automate this process
# Need to specify your X, W, M, and Y variables, the script will do the rest
# note: x = IV, w = Moderator, m = Mediator, y = DV

# one you change iv1, iv2, iv3, and dv to your variables of interest, then run the subsequent code in order
# we'll also remove NA values to make this simpler

(dat1 = dat %>% 
                 select(iv1, iv2, iv3, dv) %>% 
                 rename(x = iv1, # relabel whatever you want your variables to be named in the manuscript, cannot contain spaces though
                        w = iv2, 
                        m = iv3, 
                        y = dv) %>%
                 drop_na())

####### center and scale IVs
dat1$cen_x <- standardize(dat1$x, two_sd = FALSE, force = TRUE) # main predictor
dat1$cen_w <- standardize(dat1$w, two_sd = FALSE, force = TRUE) # moderator 1
dat1$cen_m <- standardize(dat1$m, two_sd = FALSE, force = TRUE) # moderator 2

# need to manually add interaction term for IV and Moderator
dat1$cen_xw <- dat1$cen_x * dat1$cen_w

# Calculate sample SD of centered moderator W for conditional probing
w_sd   <- sd(dat1$cen_w)
w_low  <- -w_sd
w_high <-  w_sd

# Inspect clean dataset
glimpse(dat1)


#####################################
### 3. Moderated Mediation Syntax ###
#####################################

# Model 7 Syntax: Dynamic insertion of sample SD values for conditional indirect effects
hayes_model7 <- paste0('
  # Stage 1: Mediator model (X, W, and X*W predict M)
  m ~ a1*cen_x + a2*cen_w + a3*cen_xw

  # Stage 2: Outcome model (M and X predict Y)
  y ~ b*m + c_prime*cen_x

  # Index of Moderated Mediation (a3 * b)
  index_mod_med := a3 * b

  # Conditional Indirect Effects at -1 SD, Mean (0), and +1 SD of Moderator W
  ind_low  := (a1 + a3 * (', w_low,  ')) * b
  ind_mean := (a1 + a3 * 0) * b
  ind_high := (a1 + a3 * (', w_high, ')) * b

  # Direct Effect (c\')
  direct := c_prime

  # Conditional Total Effects
  total_low  := direct + ind_low
  total_mean := direct + ind_mean
  total_high := direct + ind_high
')


#####################################
### 4. Model Estimation & Bootstrap #
#####################################

# Fit SEM model with 5,000 percentile bootstrap resamples (Hayes standard)
set.seed(12345) # Seed for reproducible bootstrap CIs
sem_fit <- sem(
              model     = hayes_model7,
              data      = dat1,
              se        = "bootstrap",
              bootstrap = 5000)


#####################################
### 5. Output & Parameter Summaries #
#####################################

# Complete model output with standardized parameters
summary(sem_fit,
          fit.measures = TRUE,
          standardize  = TRUE,
          rsquare      = TRUE)

# Extract 95% Percentile Bootstrapped Confidence Intervals
boot_results <- parameterEstimates(
                    sem_fit,
                    boot.ci.type = "perc", # Recommended for PROCESS/SEM indirect effects
                    level        = 0.95,
                    ci           = TRUE)

# Clean summary table isolating defined parameters (Index & Conditional Effects)
modmed_summary <- boot_results %>%
                        filter(op == ":=") %>%
                        select(lhs, label, est, se, z, pvalue, ci.lower, ci.upper)

# View defined effects
print(modmed_summary)

# Export clean results to project tables folder
rio::export(modmed_summary, here::here("tables", "model7_bootstrapped_results.csv"))