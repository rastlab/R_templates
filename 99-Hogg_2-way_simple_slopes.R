#########################################################################
##################### 2-way Simple Slopes Testing #######################
#########################################################################

#####
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
pacman::p_load(rio, sjstats) # this row is needed for plotting

## load regression data

# the following command will open a dialog box and allow you to select the file you wish to laod
dat <- import(file.choose())


###############################################################################################################
######## Prepare data for simple slopes of the 3-way interaction (see Mike's simples procedure sheet) #########
###############################################################################################################

## Step 3 of Mike's sheet

dat$c_iv1A <- dat$c_iv1 - sd(dat$c_iv1, na.rm=T)
dat$c_iv1B <- dat$c_iv1 + sd(dat$c_iv1, na.rm=T)
dat$c_iv2A <- dat$c_iv2 - sd(dat$c_iv2, na.rm=T)
dat$c_iv2B <- dat$c_iv2 + sd(dat$c_iv2, na.rm=T)

## Step 4 of Mike's sheet is not needed in R

## Step 5 & 6 of Mike's sheet

# simple slopes for iv1
iv1.b <- lm(dv ~ c_iv1 * c_iv2B, data=dat)
iv1.a <- lm(dv ~ c_iv1 * c_iv2A, data=dat)

summary(iv1.b)
sjstats::std_beta(iv1.b)
summary(iv1.a)
sjstats::std_beta(iv1.a)

# simple slopes for iv2
iv2.b <- lm(dv ~ c_iv2 * c_iv1B, data=dat)
iv2.a <- lm(dv ~ c_iv2 * c_iv1A, data=dat)

summary(iv2.b)
sjstats::std_beta(iv2.b)
summary(iv2.a)
sjstats::std_beta(iv2.a)


