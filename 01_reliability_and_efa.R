#########################################################################
############### Data Factor, Structure, and Reliabilities ###############
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
update.packages(ask=FALSE, checkBuilt = TRUE)
if(!require(pacman)){install.packages("pacman")}
pacman::p_load(parallel, rio, tidyverse, car, psych, jmv)


## load data

# RData files work the best in R. 
# Try to only open RData files to avoid any issues.
# CSV works the next best in R. 
# Try to only save and open CSV files to avoid any issue if you cannot load RData files.
# SPSS files can be buggy to import, especially factors and labels

# the following command will open a dialog box and allow you to select the file you wish to laod
dat <- import(file.choose())

setwd("./PROJECT_NAME/")       # change PROJECT_NAME to your project's name

# check to see that you loaded the correct dataset
View(dat)

# list variables in dataset
glimpse(dat)

# NB: Alt + Shift + K --> will bring up keyboard shortcuts

###################################################
###### Prepare Data/Scales for Alpha and EFA ######
###################################################

# Create dataframes that has only the relevant items for each scale
dv1 = na.omit(select(dat, c(dv1_1, dv1_2, dv1_3, dv1_4)))
dv2 = na.omit(select(dat, c(dv2_1, dv2_2, dv2_3, dv2_4)))

################################
###### Scale Descriptives ######
################################

### Descriptive stats on key variables

describe(dv1)
describe(dv2)

## summary table
describe(select(dat, c(avg_dv1, avg_dv2)))

#########################################
###### Inter-variable Correlations ######
#########################################

# Interscale correlations
round(cor(select(dat, c(avg_dv1, avg_dv2)), 
          use = "complete.obs", 
          method = "pearson"), 2)


##############################
###### Cronbach's Alpha ######
##############################

# Calculate Cronbach's Alpha

# alpha for scale, with item if deleted summary also
jmv::reliability(
  data = dat,
  vars = c("dv1_1", "dv1_2", "dv1_3", "dv1_4"),
  meanScale = TRUE,
  sdScale = TRUE,
  alphaItems = TRUE,
  meanItems = TRUE,
  sdItems = TRUE)


##############################
###### Factor Analysis #######
##############################

# Note: Rotate can "none", "varimax", "quatimax", "promax", "oblimin", "simplimax", or "cluster" .
# Note: retaining 3 components but can be any number

#### DV1 - PRINCIPAL COMPONENTS ANALYSIS (PCA) ####
pca_dv1 <- princomp(dv1)
summary(pca_dv1)
loadings(pca_dv1)
plot(pca_dv1, type="lines")
biplot(pca_dv1)

#### DV1 - EXPLORATORY FACTOR ANALYSIS (EFA) ####
#First make a Scree Plot
plot((eigen(cor(dv1)))$values, type="both")

#Next, do an exploratory factor analysis
(dv1.efa <- factanal(dv1, 3, rotation="promax"))

#### DV2 - PRINCIPAL COMPONENTS ANALYSIS (PCA) ####
pca_dv2 <- princomp(dv2)
summary(pca_dv2)
loadings(pca_dv2)
plot(pca_dv2, type="lines")
biplot(pca_dv2)

#### DV2 - EXPLORATORY FACTOR ANALYSIS (EFA) ####
#First make a Scree Plot
plot((eigen(cor(dv2)))$values, type="both")

#Next, do an exploratory factor analysis
(dv2.efa <- factanal(dv2, 3, rotation="promax"))

# remove EFAs/CFAs from temp/working memory if not needed
rm(dv1.efa)
rm(dv2.efa)
rm(pca_dv1)
rm(pca_dv2)