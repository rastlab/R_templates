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
update.packages(ask = FALSE, checkBuilt = TRUE)
if(!require(pacman)){install.packages("pacman")}
devtools::install_github("NumbersInternational/flipDimensionReduction")
pacman::p_load(parallel, rio, tidyverse, car, psych, flipDimensionReduction, jmv)

# if you want reproducible analysis, use the checkpoint() command using YYYY-MM-DD format
# e.g., checkpoint("YYYY-MM-DD") or checkpoint("2017-09-25")

# checkpoint("YYYY-MM-DD")

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

###################################################
###### Prepare Data/Scales for Alpha and EFA ######
###################################################

# Create dataframes that has only the relevant items for each scale

dv1 <- dat %>% select(dv1_1, dv1_2, dv1_3, dv1_4) %>% na.omit()

dv2 <- dat %>% select(dv2_1, dv2_2, dv2_3, dv2_4) %>% na.omit()

## could also use the following commands to simplify if appropriate
# dv1 <- dat %>% select(starts_with("dv1_") %>% na.omit()
# dv2 <- dat %>% select(starts_with("dv2_") %>% na.omit()

################################
###### Scale Descriptives ######
################################

### Descriptive stats on key variables

describe(dv1)
describe(dv2)

## summary table
dat %>% 
       select(avg_dv1, avg_dv2) %>% 
       describe()

# could also shorten by writing:
# dat %>% 
#        select(starts_with("avg_")) %>% 
#        describe()

#########################################
###### Inter-variable Correlations ######
#########################################

# Interscale correlations
dat %>%
       select(avg_dv1, avg_dv2) %>%
       cor(use = "complete.obs", method = "pearson") %>%
       round(2)

# could also shorten by writing:
# dat %>%
#       select(starts_with("avg_")) %>%
#       cor(use = "complete.obs", method = "pearson") %>%
#       round(2)


##############################
###### Cronbach's Alpha ######
##############################

# Calculate Cronbach's Alpha

# alpha for scale, with item if deleted summary also
alpha(dv1)

alpha(dv2)


##############################
###### Factor Analysis #######
##############################


##%######################################################%##
#                                                          #
####       Using 'flipDimensionReduction' package       ####
#                                                          #
##%######################################################%##


# Note that the rotations used by SPSS will sometimes use the “Kaiser Normalization”. 
# By default, the rotations used in 'psych' do not normalize.
# We will use another package that builds off 'psych' but does normalize the EFA/PCA by default to match SPSS' output
# this package also gives a nice, visually appealing table in the "Viewer" window

# Note: Rotate can "none", "varimax", "quatimax", "promax", "oblimin", "simplimax", or "cluster" .

# analysis without a rotation
PrincipalComponentsAnalysis(dv1, rotation = "none", select.n.rule="Kaiser rule")

# analysis with a rotation, as an example
PrincipalComponentsAnalysis(dv1, rotation = "equamax", select.n.rule="Kaiser rule")

## add {print.type=""} to get different types of output
# "details" gives a lot more EFA/PCA info and in a plain text output in the R console
PrincipalComponentsAnalysis(dv1, rotation = "equamax", select.n.rule="Kaiser rule", print.type="details")

# "scree" provides a simple, yet elegant looking scree plot in the "Viewer" pane
PrincipalComponentsAnalysis(dv1, rotation = "equamax", select.n.rule="Kaiser rule", print.type= "scree")



##%######################################################%##
#                                                          #
####               Using 'psych' package                ####
#                                                          #
##%######################################################%##


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