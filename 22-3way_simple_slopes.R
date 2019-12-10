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

## update packages then install these packages if not yet installed
# rm(list = ls())
update.packages(ask = FALSE, checkBuilt = TRUE)
if(!require(pacman)){install.packages("pacman")}
if(!require(jmv)){install.packages("jmv")}
if(!require(reghelper)){install.packages("reghelper")}
pacman::p_load(rio, pequod, jtools, interactions, 
               apaTables, stargazer, psych, tidyverse, 
               parameters, performance, effectsize,
               janitor, ggstance, patchwork) # this row is needed for plotting

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

################################
###### Summary Statistics ######
################################

## descriptive summary table 
# note: na.omit() removes any NAs contained within each of the IVs

dat %>%
  dplyr::select(iv1, iv2, iv3, dv) %>%
  describe()

####### center and scale IVs
dat$c_iv1 <- standardize(dat$iv1, two_sd = FALSE, force = TRUE)
dat$c_iv2 <- standardize(dat$iv2, two_sd = FALSE, force = TRUE)
dat$c_iv3 <- standardize(dat$iv3, two_sd = FALSE, force = TRUE)

# verify centering
dat %>%
  dplyr::select(starts_with("c_")) %>%
  describe()

##############################
###### 3-way Regression ######
##############################

####### test 3-way regression interaction

## regression function 
# simplified code to run regressions on all DVs at once

reg_models <- dat %>% 
  dplyr::select(starts_with("avg_")) %>%   # this line tells the map() only use your DVs (all start "avg_" in my datasets)
  map(~summ(lm(. ~ c_iv1 * c_iv2 * c_iv3, data = dat))) 

reg_models 

### linear regression to dive into any significant 3-ways
step1.1 <- lm(dv ~ c_iv1 + c_iv2 + c_iv3, data=dat)
step2.1 <- lm(dv ~ c_iv1 * c_iv2 + c_iv1 * c_iv3 + c_iv2 * c_iv3, data=dat)
step3.1 <- lm(dv ~ c_iv1 * c_iv2 * c_iv3, data=dat)

##  check GLM assumptions for:
#heteroskedastic (error variance)
check_heteroscedasticity(step3.1)

# autocorrelation (independence of errors)
check_autocorrelation(step3.1)

# normality (normality of residuals)
check_normality(step3.1)

# multicollinearity (predictor independence)
check_collinearity(step3.1)

# tests for outliers in model then iteratively removes outliers and re-runs the model
# consider add "mcd" method to detect outliers (Mahalanobis et al., 2018)
check_outliers(step3.1, method = c("cook", "zscore", "mahalanobis"))

# can also plot GLM assumptions
plot(gvlma::gvlma(step3.1))


## SPSS-like regression summary
jmv::linReg(data = dat,
            dep = dv,
            covs = vars(c_iv1, c_iv2, c_iv3),
            blocks = list(
              list("c_iv1", "c_iv2", "c_iv3"),
              list(c("c_iv1", "c_iv2"),
                   c("c_iv1", "c_iv3"),
                   c("c_iv2", "c_iv3")),
              list(c("c_iv1", "c_iv2", "c_iv3"))),
            r2 = FALSE, r2Adj = TRUE, ci = TRUE, stdEst = TRUE,
            ciEmm = FALSE, emmPlots = FALSE, emmWeights = FALSE)

##############################################################
######## could also achive this differently by doing: ########
##############################################################

# hierarchical linear regression
reghelper::build_model(dv, c(c_iv1 + c_iv2), 
                       c(c_iv1 * c_iv2, 
                         c_iv1 * c_iv3, 
                         c_iv2 * c_iv3),
                       c(c_iv1 * c_iv2 * c_iv3),
                       data=dat, model='lm') %>% summary()

# step 1 betas
sjstats::std_beta(step1.1)

# step 2 betas
sjstats::std_beta(step2.1)

# step 3 betas
sjstats::std_beta(step3.1)


######################################################################
############### Simple Slope Testing Automatically ###################
######################################################################

### test simple slopes 

# Johnson-Neyman intervals with plots
sim_slopes(step3.1, pred = c_iv1, modx = c_iv2, mod2 = c_iv3, modx.values = "plus-minus", mod2.values = "plus-minus", jnplot = TRUE)
sim_slopes(step3.1, pred = c_iv1, modx = c_iv3, mod2 = c_iv2, modx.values = "plus-minus", mod2.values = "plus-minus", jnplot = TRUE)
sim_slopes(step3.1, pred = c_iv2, modx = c_iv1, mod2 = c_iv3, modx.values = "plus-minus", mod2.values = "plus-minus", jnplot = TRUE)
sim_slopes(step3.1, pred = c_iv2, modx = c_iv3, mod2 = c_iv1, modx.values = "plus-minus", mod2.values = "plus-minus", jnplot = TRUE)
sim_slopes(step3.1, pred = c_iv3, modx = c_iv1, mod2 = c_iv2, modx.values = "plus-minus", mod2.values = "plus-minus", jnplot = TRUE)
sim_slopes(step3.1, pred = c_iv3, modx = c_iv2, mod2 = c_iv1, modx.values = "plus-minus", mod2.values = "plus-minus", jnplot = TRUE)

# simple slopes plots with Johnson-Neyman intervals in output
probe_interaction(step3.1, 
                  pred = c_iv1, modx = c_iv2, mod2 = c_iv3, 
                  modx.values = "plus-minus", 
                  mod2.values = "plus-minus")

probe_interaction(step3.1, 
                  pred = c_iv2, modx = c_iv1, mod2 = c_iv3, 
                  modx.values = "plus-minus", 
                  mod2.values = "plus-minus")

probe_interaction(step3.1, 
                  pred = c_iv3, modx = c_iv1, mod2 = c_iv2, 
                  modx.values = "plus-minus", 
                  mod2.values = "plus-minus")


##### simple slopes for Excel plotting

## create simple slopes using 'pequod'
model1 <- na.omit(lmres(dv ~ iv1 * iv2 * iv3, data=dat))

### iv1 as slope, iv2 and iv3 as moderator
s_slopes1 <- na.omit(simpleSlope(model1, pred = "iv1", mod1 = "iv2", mod2 = "iv3"))
summary(s_slopes1)

# generate simple slope points to plot manually in Excel
s_slopes1$Points

## iv2 as slope, iv1 and iv3 as moderator
s_slopes2 <- na.omit(simpleSlope(model1, pred = "iv2", mod1 = "iv1", mod2 = "iv3"))
summary(s_slopes2)

# generate simple slope points to plot manually in Excel
s_slopes2$Points


## iv3 as slope, iv1 and iv2 as moderator
s_slopes3 <- na.omit(simpleSlope(model1, pred = "iv3", mod1 = "iv1", mod2 = "iv2"))
summary(s_slopes3)

# generate simple slope points to plot manually in Excel
s_slopes3$Points


###############################################
######### Plotting 3-way interaction ##########
###############################################

# the follow commands will create APA-style, MS ready figures

### create x, z, w, and y columns by renaming IVs
dat$z <- dat$c_iv1     # x-axis variable here
dat$x <- dat$c_iv2     # moderator_1 variable here
dat$w <- dat$c_iv3         # moderator_2 variable here
dat$y <- dat$dv           # outcome variable here


## create labels for figure
# must use quotes for labels
# change labels in quotes to be what you want them to be

panel_a_label  <- "Panel A: YYY"         # panel A = low moderator
panel_b_label  <- "Panel B: ZZZ"         # panel B = high moderator
y_label        <- "dv_name"
y_range        <- c(-1.0, 1.0)           # desired numeric range of y-axis
y_axis_high    <- 1.0                    # high descrete numeric value displayed on y-axis
y_axis_low     <- -1.0                    # low descrete numeric value displayed on y-axis
z_label        <- "z_non-moderator_name" # X-axis variable (non-moderator)
z_values       <- c("low", "high")       # non-moderator values
z_range        <- c(-1.0, 1.0)           # non-moderator numerical values
modx_label     <- "x_moderator_name"     # figure legend (moderator)
modx_values    <- c("low", "high")       # moderator values

# run but only modify these if needed
# line type and color are set for APA 7ed
line_types    <- c("longdash", "solid")   # can be “solid”, “dashed”, “dotted”, “dotdash”, “longdash”, “twodash”
line_colors   <- c("#9E9E9E", "#000000")  # these are colorblind friendly color schemes, don't change unless needed

# change legend location if needed
# in the format of (x,y), can be any number between 0 and 1
# (0.8, 0.8) is upper-right corner, (0.2, 0.8) is the top left of the figure
legend_loc    <- c(0.2, 0.8)

# jtools ggplot2 plots
source("https://raw.githubusercontent.com/rastlab/R_templates/master/99991_ggplot2_simple_slopes.R")


#####################################
####### Here are the figures ########
#####################################

panel_a
panel_b
panel_a_b # don't worry about legend overlapping with figure, it'll fix when saving it


# if you are happy with figures, save them
# can change dimensions, file type, and dpi as per journal requirement specifications
ggsave('./figures/figure_1_a.png', panel_a, width = 8, height = 6, unit = 'in', dpi = 320)
ggsave('./figures/figure_1_b.png', panel_b, width = 8, height = 6, unit = 'in', dpi = 320)
ggsave('./figures/figure_1.png', panel_a_b, width = 8, height = 8, unit = 'in', dpi = 320)


#######################################
###### Create descriptives table ######
#######################################

# the below script is set up so that once YOU update the variable names in your analysis, 
# then the below commands will automatically create a descriptives table that can be placed
# directly into your MS, presentation, or poster
# we'll also remove NA values to make this simpler

dat3 = na.omit(dat %>% 
                 dplyr::select(iv1, iv2, iv3, dv) %>% 
                 rename(NEW_NAME_IV1 = iv1, # relabel whatever you want your variables to be named in the manuscript, cannot contain spaces though
                        NEW_NAME_IV2 = iv2, 
                        NEW_NAME_IV2 = iv3,
                        NEW_NAME_DV  = dv))

# correlation table
apa.cor.table(dat3, filename = "./tables/correlation_table.doc", table.number = 1,
              show.conf.interval = FALSE, landscape = TRUE)


#######################################
###### Saving Data and Workspace ######
#######################################
# Save current workspace: 
export(dat, "./data/22_three_way_regression.RData")

# save R data file as CSV
export(dat, "./data/22_three_way_regression.csv")

# save R data file as SAV SPSS file
export(dat, "./data/22_three_way_regression.sav")

