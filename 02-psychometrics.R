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

## Install the required script packages if not yet installed

# Install pacman package if necessary
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here, rio, tidyverse, psych, corrr, correlation, parameters, performance, jmv)

## load data

# RDS files work the best in R. 
# Try to only open RData/RDS files to avoid any issues.
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

dv1 <- dat %>% 
  dplyr::select(dv1_1, dv1_2, dv1_3, dv1_4) %>% 
  drop_na()

dv2 <- dat %>% 
  dplyr::select(dv2_1, dv2_2, dv2_3, dv2_4) %>% 
  drop_na()

## could also use the following commands to simplify if appropriate
# dv1 <- dat %>% select(starts_with("dv1_")) %>% drop_na()
# dv2 <- dat %>% select(starts_with("dv2_")) %>% drop_na()

################################
###### Scale Descriptives ######
################################

### Descriptive stats on key variables

dv1 %>% psych::describe()
dv2 %>% psych::describe()

## summary table
dat %>% 
       dplyr::select(avg_dv1, avg_dv2) %>% 
       psych::describe()

# could also shorten by writing:
# dat %>% 
#        dplyr::select(starts_with("avg_")) %>% 
#        psych::describe()

#########################################
###### Inter-variable Correlations ######
#########################################

# Interscale correlation total
dat %>%
  dplyr::select(avg_dv1, avg_dv2) %>%                    # Selects only variables/items for correlation table
  corrr::correlate(use = "pairwise.complete.obs") %>%    # Create correlation data frame (cor_df)
  corrr::shave() %>%                                     # only show bottom 'triangle' of output
  corrr::fashion()                                       # formats output to be more readable


# could also shorten by writing:
# dat %>%
#       dplyr::select(starts_with("avg_")) %>%
#       corrr::correlate(use = "pairwise.complete.obs")%>%
#       corrr::shave() %>%
#       corrr::fashion()

# Correlation testing table for ALL DV correlations
dat %>%
       dplyr::select(avg_dv1, avg_dv2) %>%    # Selects only variables/items for correlation table
       correlation::correlation()

# could also shorten by writing:
# dat %>%
#       dplyr::select(starts_with("avg_")) %>%
#       correlation::correlation()


##############################
###### Cronbach's Alpha ######
##############################

# Calculate Cronbach's Alpha

# alpha for scale, with item if deleted summary also
jmv::reliability(dv1, alphaItems = TRUE)

jmv::reliability(dv2, alphaItems = TRUE)

##############################
###### Factor Analysis #######
##############################

# Check factor structure
parameters::check_factorstructure(dv1)

parameters::check_factorstructure(dv2)

### EFA 

## check number of factors

# no rotation 
parameters::n_factors(dv1, type = "FA", rotation = "none")

# varimax rotation
parameters::n_factors(dv1, type = "FA", rotation = "varimax")

## conduct EFA

# Note: Rotate can be:
# orthogonal: "none", "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT" and "bifactor"
# oblique: Promax", "promax", "oblimin", "simplimax", "bentlerQ, "geominQ" and "biquartimin" and "cluster"

# no rotation
psych::fa(dv1, rotation = "none") %>% 
  parameters::model_parameters(sort = TRUE, threshold = "max")


### PCA

## check number of factors

# no rotation 
parameters::n_factors(dv1, type = "PCA", rotation = "none")

# varimax rotation
parameters::n_factors(dv1, type = "PCA", rotation = "varimax")

## conduct EFA

# Note that SPSS conducts a PCA, not EFA by default for Dimension Reduction
# Note that the rotations used by SPSS will sometimes use the “Kaiser Normalization”. 
# Note: Rotate can be 'none', 'varimax' (default), 'quartimax', 'promax', 'oblimin', or 'simplimax'

# no rotation 
jmv::pca(dv1,
         rotation = "none",
         nFactorMethod = "eigen",
         screePlot = TRUE,
         eigen = TRUE,
         factorCor = TRUE,
         factorSummary = TRUE)

# varimax rotation
jmv::pca(dv1,
         rotation = "varimax",
         nFactorMethod = "eigen",
         screePlot = TRUE,
         eigen = TRUE,
         factorCor = TRUE,
         factorSummary = TRUE)


# can also conduct PCA another way to get different tables
dv1_pca <- parameters::principal_components(dv1, rotation = "varimax", threshold = "max")

dv1_pca
summary(dv1_pca)

