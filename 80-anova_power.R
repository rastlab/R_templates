#########################################################################
##################### ANOVA Power Analyses ##############################
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
if(!"remotes" %in% rownames(installed.packages())) install.packages("remotes")

remotes::install_github("Lakens/ANOVApower")
pacman::p_load(rio, ANOVApower)

# NB: Alt + Shift + K --> will bring up keyboard shortcuts

#############################################
###### Variables specific to your data ######
#############################################

model <- "2b*2b" # This should match the design of your study, "2b*2b*2b" is a 2x2x2,
                 # 2b*2b specifically for 2x2 between-subjects design 2w*2w for within subjects

N   <- 100   # Replace with desired sample size of entire study
M1  <- 3.54  # Replace with estimated mean of group 1
SD1 <- 1.67  # Replace with estimated SD of group 1
M2  <- 5.19  # Replace with estimated mean of group 2
SD2 <- 1.19  # Replace with estimated SD of group 2
M3  <- 4.34  # Replace with estimated mean of group 3
SD3 <- 1.55  # Replace with estimated SD of group 3
M4  <- 4.21  # Replace with estimated  mean of group 4
SD4 <- 1.79  # Replace with estimated SD of group 4

##### Use groups such as these if more than a 2 x 2 design

# M5  <- 4.21 # Replace with estimated mean of group 5  
# SD5 <- 1.79 # Replace with estimated SD of group 5
# M6  <- 4.21 # Replace with estimated mean of group 6
# SD6 <- 1.79 # Replace with estimated SD of group 6

simul <- 500 # Replace with desired number of simulations. More simulations, more time to calculate
             # 500 is a reasonable amount and analysis time

#############################################
###### RUN BUT DO NOT MODIFY!! ##############
#############################################

# the following code with provide power analysis output
# based on parameter estimates provided above 

design_result <- ANOVA_design(design = model,
                              n = N, 
                              mu = c(M1, M2, M3, M4), 
                              sd = c(SD1, SD2, SD3, SD4))


# simulate power based on obtained data
ANOVA_exact(design_result, alpha_level = 0.05)

# same as above but, lets us specify simulation info
ANOVA_power(design_result, alpha_level = 0.05,
            p_adjust = "none", 
            seed = 2019, nsims = simul) 

