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

######## ONLY MODIFY THE WORDS IV1, IV2, IV3, AND DV 


### Plot iv1 as slope and iv2 as moderator and iv3 (low -1SD) as panel
yrange = c(4,7)  # modify the y-axis range
xrange = c(-1.5,1.5)
png(file="./figures/figure_1.png", width=8, height=6, units="in", res = 800)
par(bty = 'l')
par(family="Times")
plot(c(-1, 1), c((s_slopes1$Points[1, 1]), (s_slopes1$Points[1, 2])), type='b', lty=1, pch = 15, axes=F, xlab="", ylab="", ylim=yrange, xlim=xrange)
par(new = T)
plot(c(-1, 1), c((s_slopes1$Points[3, 1]), (s_slopes1$Points[3, 2])), type='b', lty=2, pch = 16, axes=F, xlab="iv1", ylab="dv", ylim=yrange, xlim=xrange)
axis(1, at=c(-1, 1), labels=c("iv1-low", "iv1-high"))
axis(2, at=c(4, 5, 6, 7))
legend("topright", title = "iv2", c("iv2-high (+1SD)", "iv2-low (-1SD)"), lty=1:2, cex=.90)
title(main="Panel B: iv3 (low -1SD)", adj = 0, font.main = 1, cex.main = 1)
box()
dev.off()
graphics.off()


### Plot iv1 as slope and iv2 as moderator and iv3 (high +1SD) as panel
yrange = c(4,7)  # modify the y-axis range
xrange = c(-1.5,1.5)
png(file="./figures/figure_2.png", width=8, height=6, units="in", res = 800)
par(bty = 'l')
par(family="Times")
plot(c(-1, 1), c((s_slopes1$Points[2, 1]), (s_slopes1$Points[2, 2])), type='b', lty=1, pch = 15, axes=F, xlab="", ylab="", ylim=yrange, xlim=xrange)
par(new = T)
plot(c(-1, 1), c((s_slopes1$Points[4, 1]), (s_slopes1$Points[4, 2])), type='b', lty=2, pch = 16, axes=F, xlab="iv1", ylab="dv", ylim=yrange, xlim=xrange)
axis(1, at=c(-1, 1), labels=c("iv1-low", "iv1-high"))
axis(2, at=c(4, 5, 6, 7))
legend("topright", title = "iv2", c("iv2-high (+1SD)", "iv2-low (-1SD)"), lty=1:2, cex=.90)
title(main="Panel B: iv3 (high +1SD)", adj = 0, font.main = 1, cex.main = 1)
box()
dev.off()
graphics.off()


### Plot iv2 as slope and iv1 as moderator and iv3 (low -1SD) as panel
yrange = c(4,7)  # modify the y-axis range
xrange = c(-1.5,1.5)
png(file="./figures/figure_3.png", width=8, height=6, units="in", res = 800)
par(bty = 'l')
par(family="Times")
plot(c(-1, 1), c((s_slopes2$Points[1, 1]), (s_slopes2$Points[1, 2])), type='b', lty=1, pch = 15, axes=F, xlab="", ylab="", ylim=yrange, xlim=xrange)
par(new = T)
plot(c(-1, 1), c((s_slopes2$Points[3, 1]), (s_slopes2$Points[3, 2])), type='b', lty=2, pch = 16, axes=F, xlab="iv2", ylab="dv", ylim=yrange, xlim=xrange)
axis(1, at=c(-1, 1), labels=c("iv2-low", "iv2-high"))
axis(2, at=c(4, 5, 6, 7))
legend("topright", title = "iv1", c("iv1-high (+1SD)", "iv1-low (-1SD)"), lty=1:2, cex=.90)
title(main="Panel B: iv3 (low -1SD)", adj = 0, font.main = 1, cex.main = 1)
box()
dev.off()
graphics.off()


### Plot iv2 as slope and iv1 as moderator and iv3 (high +1SD) as panel
yrange = c(4,7)  # modify the y-axis range
xrange = c(-1.5,1.5)
png(file="./figures/figure_4.png", width=8, height=6, units="in", res = 800)
par(bty = 'l')
par(family="Times")
plot(c(-1, 1), c((s_slopes2$Points[2, 1]), (s_slopes2$Points[2, 2])), type='b', lty=1, pch = 15, axes=F, xlab="", ylab="", ylim=yrange, xlim=xrange)
par(new = T)
plot(c(-1, 1), c((s_slopes2$Points[4, 1]), (s_slopes2$Points[4, 2])), type='b', lty=2, pch = 16, axes=F, xlab="iv2", ylab="dv", ylim=yrange, xlim=xrange)
axis(1, at=c(-1, 1), labels=c("iv2-low", "iv2-high"))
axis(2, at=c(4, 5, 6, 7))
legend("topright", title = "iv1", c("iv1-high (+1SD)", "iv1-low (-1SD)"), lty=1:2, cex=.90)
title(main="Panel B: iv3 (high +1SD)", adj = 0, font.main = 1, cex.main = 1)
box()
dev.off()
graphics.off()


### Plot iv3 as slope and iv1 as moderator and iv2 (low -1SD) as panel
yrange = c(4,7)  # modify the y-axis range
xrange = c(-1.5,1.5)
png(file="./figures/figure_5.png", width=8, height=6, units="in", res = 800)
par(bty = 'l')
par(family="Times")
plot(c(-1, 1), c((s_slopes3$Points[1, 1]), (s_slopes3$Points[1, 2])), type='b', lty=1, pch = 15, axes=F, xlab="", ylab="", ylim=yrange, xlim=xrange)
par(new = T)
plot(c(-1, 1), c((s_slopes3$Points[3, 1]), (s_slopes3$Points[3, 2])), type='b', lty=2, pch = 16, axes=F, xlab="iv3", ylab="dv", ylim=yrange, xlim=xrange)
axis(1, at=c(-1, 1), labels=c("iv3-low", "iv3-high"))
axis(2, at=c(4, 5, 6, 7))
legend("topright", title = "iv1", c("iv1-high (+1SD)", "iv1-low (-1SD)"), lty=1:2, cex=.90)
title(main="Panel B: iv2 (low -1SD)", adj = 0, font.main = 1, cex.main = 1)
box()
dev.off()
graphics.off()


### Plot iv3 as slope and iv1 as moderator and iv2 (high +1SD) as panel
yrange = c(4,7)  # modify the y-axis range
xrange = c(-1.5,1.5)
png(file="./figures/figure_6.png", width=8, height=6, units="in", res = 800)
par(bty = 'l')
par(family="Times")
plot(c(-1, 1), c((s_slopes3$Points[2, 1]), (s_slopes3$Points[2, 2])), type='b', lty=1, pch = 15, axes=F, xlab="", ylab="", ylim=yrange, xlim=xrange)
par(new = T)
plot(c(-1, 1), c((s_slopes2$Points[4, 1]), (s_slopes2$Points[4, 2])), type='b', lty=2, pch = 16, axes=F, xlab="iv3", ylab="dv", ylim=yrange, xlim=xrange)
axis(1, at=c(-1, 1), labels=c("iv3-low", "iv3-high"))
axis(2, at=c(4, 5, 6, 7))
legend("topright", title = "iv1", c("iv1-high (+1SD)", "iv1-low (-1SD)"), lty=1:2, cex=.90)
title(main="Panel B: iv2 (high +1SD)", adj = 0, font.main = 1, cex.main = 1)
box()
dev.off()
graphics.off()


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

