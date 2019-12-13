#############################################################################
######## Mediation Analysis (Model 4 from Hayes' SPSS Process Macro) ########
#############################################################################

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

pacman::p_load(rio, dplyr, effectsize, lavaan)

## load data

# RData files work the best in R. 
# Try to only open RData files to avoid any issues.
# CSV works the next best in R. 
# Try to only save and open CSV files to avoid any issue if you cannot load RData files.
# SPSS files can be buggy to import, especially factors and labels

# the following command will open a dialog box and allow you to select the file you wish to laod
dat <- import(file.choose())

# check to see that you loaded the correct dataset
View(dat)

# list variables in dataset
glimpse(dat)

# NB: Alt + Shift + K --> will bring up keyboard shortcuts

###############################
########## Data Prep ##########
###############################

# Hayes' template with all models can be found here:
# http://afhayes.com/public/templates.pdf
# this script is modified from: https://nickmichalak.blogspot.ca/2016/07/reproducing-hayess-process-models.html

# going to run Hayes's PROCESS Model 4: "classic" mediation
# x -> m -> y

# I've tried to automate this process
# Need to specify your X, M, and Y variables, the script will do the rest
# note: x = IV, m = Mediator, y = DV

# one you change iv1, iv2, and dv to your variables of interest, then run the subsequent code in order
# we'll also remove NA values to make this simpler

(dat1 = na.omit(dat %>% 
                 select(iv1, iv2, dv) %>% 
                 rename(x = iv1, # relabel whatever you want your variables to be named in the manuscript, cannot contain spaces though
                        m = iv2, 
                        y = dv)))

####### center and scale IVs
dat1$cen_x <- standardize(dat1$x, two_sd = FALSE, force = TRUE) # main predictor
dat1$cen_m <- standardize(dat1$m, two_sd = FALSE, force = TRUE) # moderator

# check dataset1 
glimpse(dat1)

# run mediation analysis ----

# parameters
hayes4 <- ' # direct effect
              y ~ c*cen_x
              direct := c

            # regressions
              cen_m ~ a*cen_x
              y ~ b*cen_m

            # indirect effect (a*b)
              indirect := a*b

            # total effect
              total := c + (a*b)'

# fit model
# might take a minute or two to run depending on amount of resamples
sem <- sem(model = hayes4,
            data = dat1,
            se = "bootstrap",
            bootstrap = 1000)

# fit measures
# want to look at the 'Defined Parameters:' section
summary(sem,
        fit.measures = TRUE,
        standardize = TRUE,
        rsquare = TRUE)

# if you want bootstrapped parameter estimates
# pay attention to lines 7, 8, 9 (direct, indirect, total)

parameterEstimates(sem,
                   boot.ci.type = "bca.simple",
                   level = .95,
                   ci = TRUE,
                   standardized = FALSE)
