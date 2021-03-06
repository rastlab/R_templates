#########################################################################
##################### 2-way Simple Effects Testing ######################
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

# Install pacman & jmv package if necessary
if(!"pacman" %in% rownames(installed.packages())) install.packages("pacman")
if(!"jmv" %in% rownames(installed.packages())) install.packages("jmv")

pacman::p_load(parallel, rio, psych, car, lsr, phia, tidyverse, parameters, apaTables)


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

###############################################
###### significant IV1 x IV2 interaction ######
###############################################

# We need to change the default contrast for unordered factors from “cont.treatment” to “contr.helmert”. 
options(contrasts = c("contr.helmert", "contr.poly"))

## R way of Anova using 'car' package

## ANOVA function 
# simplified code to run ANOVAs on all DVs at once

aov_models <- dat %>% 
                     dplyr::select(starts_with("avg_")) %>%   # this line tells the map() only use your DVs (all start "avg_" in my datasets)
                     map(~Anova(lm(. ~ iv1 * iv2, data = dat), type = 3))

aov_models 


# linear model to dive into any significant 2-ways
model1 <- lm(dv ~ iv1 * iv2, data=dat)

# create ANOVA
model_1 <- Anova(model1, type = 3)

# run ANOVA
model_parameters(model_1, eta_squared = "partial")


## descriptives based on condition 
# note: na.omit() removes any NAs contained within each of the IVs

(summarydat <- na.omit(dat %>%
                         group_by(iv1, iv2) %>%
                         summarise(N    = sum(!is.na(dv)),
                                   mean = round(mean(dv, na.rm=TRUE), 2),
                                   sd   = round(sd(dv, na.rm=TRUE), 2))))

# new way of Anova using 'jmv' package, closer to SPSS output
# gives partial eta-squared and omega effect sizes in a nice table

jmv::ANOVA(
           formula = dv ~ iv1 * iv2,
           data = dat,
           effectSize = "partEta",
           emMeans = ~ iv1:iv2)

############################################################################
############# Simple effects of IV1 at different IV2 levels ################
############################################################################

# quickly examine simple effects
# see https://cran.r-project.org/web/packages/phia/vignettes/phia.pdf

# create interaction model of interest
modinter <- na.omit(model1)

# examine simple effects
(moderation1 <- testInteractions(modinter, fixed = c("iv2"), across="iv1", adjustment="none"))


# eta-squared for different levels of iv2
(etasqiv1.l <- (moderation1[1,3]) / ((moderation1[1,3]) + (moderation1[3,3]))) # eta-squared iv2-l, across iv1
(etasqiv1.h <- (moderation1[2,3]) / ((moderation1[2,3]) + (moderation1[3,3]))) # eta-squared iv2-h, across iv1

moderation1
etasqiv1.l
etasqiv1.h

############################################################################
############# Simple effects of IV2 at different IV1 levels ################
############################################################################

# examine simple effects
(moderation2 <- testInteractions(modinter, fixed = c("iv1"), across="iv2", adjustment="none"))

# eta-squared for different levels of iv2
(etasqiv2.l <- (moderation2[1,3]) / ((moderation2[1,3]) + (moderation2[3,3]))) # eta-squared iv1-l, across iv2
(etasqiv2.h <- (moderation2[2,3]) / ((moderation2[2,3]) + (moderation2[3,3]))) # eta-squared iv1-h, across iv2

moderation2
etasqiv2.l
etasqiv2.h

###############################################
######### Plotting 2-way interaction ##########
###############################################

# the follow commands will create APA-style, MS ready figures
# will produce 2 figures:
# Figure_1 will plot IV1 as X-axis
# Figure_2 will plot IV2 as X-axis

### create x, z, and y columns by renaming IVs
dat$iv1 <- dat$YOUR_IV1_NAME_HERE       # x-axis variable here
dat$iv2 <- dat$YOUR_IV2_NAME_HERE       # moderator_1 variable here
dat$dv  <- dat$YOUR_DV_NAME_HERE        # outcome variable here


## create labels for figure
# must use quotes for labels
# change labels in quotes to be what you want them to be

y_label       <- "dv_name"
y_axis_high   <- 9.0                    # high descrete numeric value displayed on y-axis
y_axis_low    <- 1.0                    # low descrete numeric value displayed on y-axis
y_increment   <- 1.0                    # increments for y-axis
x_label       <- "x-axis_label"         # x-axis variable (non-moderator)
x_values      <- c("low", "high")       # x-axis values
mod_label     <- "x_moderator_name"     # figure legend (moderator)
mod_values    <- c("low", "high")       # moderator values


# change legend location if needed
# in the format of (x,y), can be any number between 0 and 1
# (0.8, 0.8) is upper-right corner, (0.2, 0.8) is the top left of the figure
legend_loc    <- c(0.8, 0.8)

# run next line and figure will be automatically created
source("https://raw.githubusercontent.com/rastlab/R_templates/master/99990-ggplot2_2way_SE.R")



#####################################
####### Here are the figures ########
#####################################

# verify figures are consistent with condition means
figure_1
figure_2

# if you are happy with figures, save them
# can change dimensions, file type, and dpi as per journal requirement specifications
ggsave('./figures/figure_1.png', figure_1, width = 8, height = 6, unit = 'in', dpi = 320)
ggsave('./figures/figure_2.png', figure_2, width = 8, height = 6, unit = 'in', dpi = 320)



#######################################
###### Create descriptives table ######
#######################################

# the below script is set up so that once YOU update the variable names in your analysis, 
# then the below commands will automatically create a descriptives table that can be placed
# directly into your MS, presentation, or poster
# we'll also remove NA values to make this simpler

dat2 <- dat %>% dplyr::select(iv1, iv2, dv) %>% na.omit()

# can make multiple tables by changing 'table.number = X
apa.2way.table(iv1, iv2, dv, data = dat2, landscape = TRUE, table.number = 1, filename="./tables/anova_table.doc")

apa.aov.table(modinter, conf.level = 0.9, type = 3,  table.number = 2, filename="./tables/anovasummary_table.doc")

# correlation matrix

dat3 = na.omit(dat %>% 
                 dplyr::select(iv1, iv2, dv) %>% 
                 rename(NEW_NAME_IV1 = iv1, # relabel whatever you want your variables to be named in the manuscript, cannot contain spaces though
                        NEW_NAME_IV2 = iv2, 
                        NEW_NAME_DV = dv))

dat3$NEW_NAME_IV1 <- as.numeric(dat3$NEW_NAME_IV1) # must change experimental variables (factors) into numeric values (intergers)
dat3$NEW_NAME_IV2 <- as.numeric(dat3$NEW_NAME_IV2) # must change experimental variables (factors) into numeric values (intergers)

apa.cor.table(dat3, show.conf.interval = FALSE, landscape = TRUE, table.number = 3, 
              filename = "./tables/correlation_table.doc")

#######################################
###### Saving Data and Workspace ######
#######################################

# Save current workspace: 
export(dat, "./data/11_two_way_anova.RData")

# save R data file as CSV
export(dat, "./data/11_two_way_anova.csv")

# save R data file as SAV SPSS file
export(dat, "./data/11_two_way_anova.sav")

