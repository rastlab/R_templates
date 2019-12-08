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
pacman::p_load(rio, pequod, QuantPsyc, lmSupport, jtools, interactions, 
               apaTables, stargazer, sjmisc, sjstats, psych, tidyverse, 
               parameters, performance, effectsize)
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
  dplyr::select(iv1, iv2, iv3 dv) %>%
  describe()

####### center IVs

dat$c_iv1 <- std(dat$iv1, robust = c("sd"))
dat$c_iv2 <- std(dat$iv2, robust = c("sd"))
dat$c_iv3 <- std(dat$iv3, robust = c("sd"))

# verify centering
dat %>%
  dplyr::select(starts_with("c_")) %>%
  describe()

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
                  mod2.values = "plus-minus", 
                  plot.points = TRUE)

probe_interaction(step3.1, 
                  pred = c_iv2, modx = c_iv1, mod2 = c_iv3, 
                  modx.values = "plus-minus", 
                  mod2.values = "plus-minus", 
                  plot.points = TRUE)

probe_interaction(step3.1, 
                  pred = c_iv3, modx = c_iv1, mod2 = c_iv2, 
                  modx.values = "plus-minus", 
                  mod2.values = "plus-minus", 
                  plot.points = TRUE)

### could also achive this differently by doing:

reghelper::build_model(dv, c(c_iv1 + c_iv2), 
                           c(c_iv1 * c_iv2, 
                             c_iv1 * c_iv3, 
                             c_iv2 * c_iv3),
                           c(c_iv1 * c_iv2 * c_iv3),
                       data=dat, model='lm') %>% summary()

# step 1
model_parameters(step1.1, standardize = "refit")

# step 2
model_parameters(step2.1, standardize = "refit")

# step 3
model_parameters(step3.1, standardize = "refit")

#####################################################################################
######################## Simple Slope Testing Automatically #########################
#####################################################################################

### iv1 as slope, iv2 and iv3 as moderator
s_slopes1 <- na.omit(simpleSlope(model1, pred = "iv1", mod1 = "iv2", mod2 = "iv3"))
summary(s_slopes1)

# simple slope points to plot manually in Excel
s_slopes1$Points

##### Autmatically Plot simple slopes

# tells R to provide a plot in APA style, figure is MS ready
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times'),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.text=element_text(size=12),
        legend.title = element_text(size=12),
        legend.position = c(.88,.88), # manually position the legend (numbers being from 0,0 at bottom left of whole plot to 1,1 at top right)
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'))

PlotSlope(s_slopes1, 
          namex="iv1", 
          namey="dv",
          limitx=c(-2, 2),
          limity=c(4, 7)) +
          apatheme

## iv2 as slope, iv1 and iv3 as moderator
s_slopes2 <- na.omit(simpleSlope(model1, pred = "iv2", mod1 = "iv1", mod2 = "iv3"))
summary(s_slopes2)

# simple slope points to plot manually in Excel
s_slopes2$Points

## Automatically plot simple slopes

PlotSlope(s_slopes2, 
          namex="iv1", 
          namey="dv",
          limitx=c(-2, 2),
          limity=c(4, 7)) +
          apatheme

## iv3 as slope, iv1 and iv2 as moderator
s_slopes3 <- na.omit(simpleSlope(model1, pred = "iv3", mod1 = "iv1", mod2 = "iv2"))
summary(s_slopes3)

# simple slope points to plot manually in Excel
s_slopes3$Points

## Automatically plot simple slopes

PlotSlope(s_slopes3,
          namex="iv1", 
          namey="dv",
          limitx=c(-2, 2),
          limity=c(4, 7)) +
          apatheme

###############################################################################################################
######## Prepare data for simple slopes of the 3-way interaction (see Mike's simples procedure sheet) #########
###############################################################################################################

## Step 3 of Mike's sheet

dat$c_iv1A <- dat$c_iv1 - sd(dat$c_iv1, na.rm=T)
dat$c_iv1B <- dat$c_iv1 + sd(dat$c_iv1, na.rm=T)
dat$c_iv2A <- dat$c_iv2 - sd(dat$c_iv2, na.rm=T)
dat$c_iv2B <- dat$c_iv2 + sd(dat$c_iv2, na.rm=T)
dat$c_iv3A <- dat$c_iv3 - sd(dat$c_iv2, na.rm=T)
dat$c_iv3B <- dat$c_iv3 + sd(dat$c_iv2, na.rm=T)

## Step 4 of Mike's sheet is not needed in R

## Step 5 & 6 of Mike's sheet

# simple slopes for iv1
iv1.bb <- lm(dv ~ c_iv1 * c_iv2B * c_iv3B, data=dat)
iv1.ab <- lm(dv ~ c_iv1 * c_iv2A * c_iv3B, data=dat)
iv1.ba <- lm(dv ~ c_iv1 * c_iv2B * c_iv3A, data=dat)
iv1.aa <- lm(dv ~ c_iv1 * c_iv2A * c_iv3A, data=dat)

summary(iv1.bb)
summary(iv1.ab)
summary(iv1.bb)
summary(iv1.aa)

# simple slopes for iv2
iv2.bb <- lm(dv ~ c_iv2 * c_iv1B * c_iv3B, data=dat)
iv2.ab <- lm(dv ~ c_iv2 * c_iv1A * c_iv3B, data=dat)
iv2.ba <- lm(dv ~ c_iv2 * c_iv1B * c_iv3A, data=dat)
iv2.aa <- lm(dv ~ c_iv2 * c_iv1A * c_iv3A, data=dat)

summary(iv2.bb)
summary(iv2.ab)
summary(iv2.bb)
summary(iv2.aa)

# simple slopes for iv3
iv3.bb <- lm(dv ~ c_iv3 * c_iv1B * c_iv2B, data=dat)
iv3.ab <- lm(dv ~ c_iv3 * c_iv1A * c_iv2B, data=dat)
iv3.ba <- lm(dv ~ c_iv3 * c_iv1B * c_iv2A, data=dat)
iv3.aa <- lm(dv ~ c_iv3 * c_iv1A * c_iv2A, data=dat)

summary(iv3.bb)
summary(iv3.ab)
summary(iv3.bb)
summary(iv3.aa)

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

# regression table
apa.reg.table(step1.1, step2.1, step3.1, filename = "./tables/regression_table.doc", table.number = 2)

# regression model summary (paste and copy into document)
stargazer(step1.1, step2.1, step3.1, 
          type="text", 
          title="Regression Results",
          dep.var.labels=c("Dependent Variable"),
          column.labels = c("Main Effects", "2-way Interactions", "3-way Interaction"),
          covariate.labels=c("iv1", "iv2", "iv3", 
                             "2-way interaction1", "2-way interaction2", "2-way interaction3", 
                             "3-way interaction"),
          omit.stat=c("ser"),
          align=TRUE,
          intercept.bottom = FALSE, 
          single.row=TRUE,     
          notes.append = FALSE, 
          header=FALSE)

#######################################
###### Saving Data and Workspace ######
#######################################
# Save current workspace: 
export(dat, "./data/22_three_way_regression.RData")

# save R data file as CSV
export(dat, "./data/22_three_way_regression.csv")

# save R data file as SAV SPSS file
export(dat, "./data/22_three_way_regression.sav")

