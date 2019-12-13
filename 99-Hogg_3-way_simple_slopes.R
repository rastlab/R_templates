#########################################################################
##################### 3-way Simple Slopes Testing #######################
#########################################################################

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

pacman::p_load(rio, sjstats)

## load regression data

# the following command will open a dialog box and allow you to select the file you wish to laod
dat <- import(file.choose())

###############################################################################################################
######## Prepare data for simple slopes of the 3-way interaction (see Mike's simples procedure sheet) #########
###############################################################################################################

### Step 3 of Mike's sheet

dat$c_iv1A <- dat$c_iv1 - sd(dat$c_iv1, na.rm=T)
dat$c_iv1B <- dat$c_iv1 + sd(dat$c_iv1, na.rm=T)
dat$c_iv2A <- dat$c_iv2 - sd(dat$c_iv2, na.rm=T)
dat$c_iv2B <- dat$c_iv2 + sd(dat$c_iv2, na.rm=T)
dat$c_iv3A <- dat$c_iv3 - sd(dat$c_iv2, na.rm=T)
dat$c_iv3B <- dat$c_iv3 + sd(dat$c_iv2, na.rm=T)

### Step 4 of Mike's sheet is not needed in R

### Step 5 & 6 of Mike's sheet

## simple slopes for iv1
iv1.bb <- lm(dv ~ c_iv1 * c_iv2B * c_iv3B, data=dat)
iv1.ab <- lm(dv ~ c_iv1 * c_iv2A * c_iv3B, data=dat)
iv1.ba <- lm(dv ~ c_iv1 * c_iv2B * c_iv3A, data=dat)
iv1.aa <- lm(dv ~ c_iv1 * c_iv2A * c_iv3A, data=dat)

# get simple slope regression info
summary(iv1.bb)
sjstats::std_beta(iv1.bb)
summary(iv1.ab)
sjstats::std_beta(iv1.ab)
summary(iv1.ba)
sjstats::std_beta(iv1.ba)
summary(iv1.aa)
sjstats::std_beta(iv1.aa)

## simple slopes for iv2
iv2.bb <- lm(dv ~ c_iv2 * c_iv1B * c_iv3B, data=dat)
iv2.ab <- lm(dv ~ c_iv2 * c_iv1A * c_iv3B, data=dat)
iv2.ba <- lm(dv ~ c_iv2 * c_iv1B * c_iv3A, data=dat)
iv2.aa <- lm(dv ~ c_iv2 * c_iv1A * c_iv3A, data=dat)

# get simple slope regression info
summary(iv2.bb)
sjstats::std_beta(iv2.bb)
summary(iv2.ab)
sjstats::std_beta(iv2.ab)
summary(iv2.ba)
sjstats::std_beta(iv2.ba)
summary(iv2.aa)
sjstats::std_beta(iv2.aa)

## simple slopes for iv3
iv3.bb <- lm(dv ~ c_iv3 * c_iv1B * c_iv2B, data=dat)
iv3.ab <- lm(dv ~ c_iv3 * c_iv1A * c_iv2B, data=dat)
iv3.ba <- lm(dv ~ c_iv3 * c_iv1B * c_iv2A, data=dat)
iv3.aa <- lm(dv ~ c_iv3 * c_iv1A * c_iv2A, data=dat)

# get simple slope regression info
summary(iv3.bb)
sjstats::std_beta(iv3.bb)
summary(iv3.ab)
sjstats::std_beta(iv2.ab)
summary(iv3.ba)
sjstats::std_beta(iv2.ba)
summary(iv3.aa)
sjstats::std_beta(iv2.aa)


