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

## update packages then install these packages if not yet installed
# rm(list = ls())
update.packages(ask = FALSE, checkBuilt = TRUE)
if(!require(pacman)){install.packages("pacman")}
pacman::p_load(rio, tidyverse, lavaan)

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

(dat1 = na.omit(dat %>% 
                 select(iv1, iv2, iv3, dv) %>% 
                 rename(x = iv1, # relabel whatever you want your variables to be named in the manuscript, cannot contain spaces though
                        w = iv2, 
                        m = iv3, 
                        y = dv)))

####### center IVs
dat1$cen_x <- std(dat1$x, robust = c("sd"))
dat1$cen_w <- std(dat1$w, robust = c("sd"))
dat1$cen_m <- std(dat1$m, robust = c("sd"))

# need to manually add interaction term for IV and Moderator
dat1$cen_xw <- dat1$cen_x * dat1$cen_w

# check dataset1 
glimpse(dat1)

# run modmed analysis ----

# specify model
hayes7 <- ' # regressions
              cen_m ~ a1*cen_x
              y ~ b1*cen_m
              cen_m ~ a2*cen_w
              cen_m ~ a3*cen_xw
              y ~ cdash*cen_x

            # index of moderated mediation
              index.mod.med := a3*b1

            # mean of centered moderator (for use in simple slopes)
              cen_w ~ w.mean*1

            # variance of centered moderator (for use in simple slopes)
              cen_w ~~ w.var*cen_w

            # indirect effects conditional on moderator (a1 + a3*a2.value)*b1
              indirect.SDbelow := a1*b1 + a3*-sqrt(w.var)*b1
              indirect.mean := a1*b1 + a3*w.mean*b1
              indirect.SDabove := a1*b1 + a3*sqrt(w.var)*b1'

# fit model
# might take a minute or two to run depending on amount of resamples
sem <- sem(model = hayes7,
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
# pay attention to lines 17, 18, 19, and 20 for the indirect effects and 'index of moderated mediation'
parameterEstimates(sem,
                   boot.ci.type = "bca.simple",
                   level = .95, 
                   ci = TRUE,
                   standardized = FALSE)
