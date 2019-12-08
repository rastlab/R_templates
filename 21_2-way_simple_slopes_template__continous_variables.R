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
      dplyr::select(iv1, iv2, dv) %>%
      describe()

####### center IVs

dat$c_iv1 <- std(dat$iv1, robust = c("sd"))
dat$c_iv2 <- std(dat$iv2, robust = c("sd"))

# verify centering
dat %>%
       dplyr::select(starts_with("c_")) %>%
       describe()

####### test 2-way regression interaction

## regression function 
# simplified code to run regressions on all DVs at once

reg_models <- dat %>% 
                     dplyr::select(starts_with("avg_")) %>%   # this line tells the map() only use your DVs (all start "avg_" in my datasets)
                      map(~summ(lm(. ~ c_iv1 * c_iv2, data = dat))) 

reg_models 

### linear regression to dive into any significant 2-ways
step1.1 <- lm(dv ~ c_iv1 + c_iv2, data=dat)
step2.1 <- lm(dv ~ c_iv1 * c_iv2, data=dat)

##  check GLM assumptions for:
#heteroskedastic (error variance)
check_heteroscedasticity(step2.1)

# autocorrelation (independence of errors)
check_autocorrelation(step2.1)

# normality (normality of residuals)
check_normality(step2.1)

# multicollinearity (predictor independence)
check_collinearity(step2.1)

# tests for outliers in model then iteratively removes outliers and re-runs the model
# consider add "mcd" method to detect outliers (Mahalanobis et al., 2018)
check_outliers(step2.1, method = c("cook", "zscore", "mahalanobis"))

## SPSS-like regression summary
jmv::linReg(data = dat,
            dep = dv,
            covs = vars(c_iv1, c_iv2),
            blocks = list(
              list("c_iv1", "c_iv2"),
              list(c("c_iv1", "c_iv2"))),
            r2 = FALSE, r2Adj = TRUE, ci = TRUE, stdEst = TRUE,
            ciEmm = FALSE, emmPlots = FALSE, emmWeights = FALSE)


### test simple slopes 

# Johnson-Neyman intervals with plots
sim_slopes(step2.1, pred = c_iv1, modx = c_iv2, jnplot = TRUE)
sim_slopes(step2.1, pred = c_iv2, modx = c_iv1, jnplot = TRUE)

# simple slopes plots with Johnson-Neyman intervals in output
probe_interaction(step2.1, 
                  pred = c_iv1, modx = c_iv2,
                  modx.values = "plus-minus",  # nb: remove this line if you have a categorical predictor
                  interval = TRUE,
                  plot.points = TRUE)

probe_interaction(step2.1, 
                  pred = c_iv2, modx = c_iv1,
                  modx.values = "plus-minus",  # nb: remove this line if you have a categorical predictor
                  interval = TRUE,
                  plot.points = TRUE)

### could also achive this differently by doing:

reghelper::build_model(dv, c(c_iv1 + c_iv2), 
                           c(c_iv1 * c_iv2), 
                       data=dat, model='lm') %>% summary()

# step 1
model_parameters(step1.1, standardize = "refit")

# step 2
model_parameters(step2.1, standardize = "refit")

#####################################################################################
######################## Simple Slope Testing Automatically #########################
#####################################################################################

##### simple slopes 

## iv1 as slope, iv2 as moderator
s_slopes1 <- na.omit(simpleSlope(model1, pred="iv1",mod1="iv2"))
summary(s_slopes1)


## Plot simple slopes

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
        
PlotSlope(s_slopes1, namemod=c("iv2 (-1SD)", 
                                "iv2 (+1SD)"),
          namex="iv1", 
          namey="dv",
          limitx=c(-2, 2),
          limity=c(1, 9)) +
          apatheme

## iv2 as slope, iv1 as moderator
s_slopes2 <- na.omit(simpleSlope(model1, pred="iv2",mod1="iv1"))
summary(s_slopes2)

## Plot simple slopes

PlotSlope(s_slopes2, namemod=c("iv2 (-1SD)", 
                               "iv2 (+1SD)"),
          namex="iv1", 
          namey="dv",
          limitx=c(-2, 2),
          limity=c(1, 9)) +
          apatheme

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
summary(iv1.a)

# simple slopes for iv2
iv2.b <- lm(dv ~ c_iv2 * c_iv1B, data=dat)
iv2.a <- lm(dv ~ c_iv2 * c_iv1A, data=dat)

summary(iv2.b)
summary(iv2.a)

###############################################
######### Plotting 2-way interaction ##########
###############################################

# the folllowing commands will create APA-style, MS ready figures

# to get the correct figure, only change the follow 8 lines of code
# after entering your desired IV and DV names, run the plotting code provide directly beneath
iv1 <- "ENTER DESIRED IV1 NAME HERE"
iv1a <- "ENTER IV1 HIGH CONDITION HERE"
iv1b <- "ENTER IV1 LOW CONDITION HERE"
iv2 <- "ENTER DESIRED IV2 NAME HERE"
iv2a <- "ENTER IV2 HIGH CONDITION HERE (+1SD)"
iv2b <- "ENTER IV2 LOW CONDITION HERE (-1SD)"
dv <- "ENTER DESIRED DV NAME HERE"
yrange = c(4,7)  # this is to change y-axis range. The first number is the y-axis low point, the second number is the y-axis high point


## do not modify the plot code, run it as it. Only change the IV and DV names provided above!

### Plot iv1 as slope and iv2 as moderator
xrange = c(-1.5,1.5)
png(file="./figures/figure_1.png", width=8, height=6, units="in", res = 800)
par(bty = 'l')
par(family="Times")
plot(c(-1, 1), c((s_slopes1$Points[1, 1]), (s_slopes1$Points[1, 2])), type='b', lty=1, pch = 15, axes=F, xlab="", ylab="", ylim=yrange, xlim=xrange)
par(new = T)
plot(c(-1, 1), c((s_slopes1$Points[2, 1]), (s_slopes1$Points[2, 2])), type='b', lty=2, pch = 16, axes=F, xlab=iv1, ylab=dv, ylim=yrange, xlim=xrange)
axis(1, at=c(-1, 1), labels=c(iv1b, iv1a))
axis(2, at=seq(1, 9, by=1))
legend("topright", title = iv2, c(iv2a, iv2b), lty=1:2, cex=.90)
box()
dev.off()
graphics.off()

## repeat plot process here to change IV2 to the slope and IV1 as the moderator

# to get the correct figure, only change the follow 8 lines of code
# after entering your desired IV and DV names, run the plotting code provide directly beneath
iv1 <- "ENTER DESIRED IV1 NAME HERE"
iv1a <- "ENTER IV1 HIGH CONDITION HERE (+1SD)"
iv1b <- "ENTER IV1 LOW CONDITION HERE (-1SD)"
iv2 <- "ENTER DESIRED IV2 NAME HERE"
iv2a <- "ENTER IV2 HIGH CONDITION HERE"
iv2b <- "ENTER IV2 LOW CONDITION HERE"
dv <- "ENTER DESIRED DV NAME HERE"
yrange = c(4,7)  # this is to change y-axis range. The first number is the y-axis low point, the second number is the y-axis high point


## do not modify the plot code, run it as it. Only change the IV and DV names provided above!

### Plot iv2 as slope and iv1 as moderator
yrange1 = c(4,7) # modify the y-axis range
xrange1 = c(-1.5,1.5)
png(file="./figures/figure_2.png", width=8, height=6, units="in", res = 800)
par(bty = 'l')
par(family="Times")
plot(c(-1, 1), c((s_slopes2$Points[1, 1]), (s_slopes2$Points[1, 2])), type='b', lty=1, pch = 15, axes=F, xlab="", ylab="", ylim=yrange1, xlim=xrange1)
par(new = T)
plot(c(-1, 1), c((s_slopes2$Points[2, 1]), (s_slopes2$Points[2, 2])), type='b', lty=2, pch = 16, axes=F, xlab=iv2, ylab=dv, ylim=yrange1, xlim=xrange1)
axis(1, at=c(-1, 1), labels=c(iv2b, iv2a))
axis(2, at=c(4, 5, 6, 7))
legend("topright", title = iv1, c(iv1a, iv1b), lty=1:2, cex=.90)
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
                 select(iv1, iv2, dv) %>% 
                 rename(NEW_NAME_IV1 = iv1, # relabel whatever you want your variables to be named in the manuscript, cannot contain spaces though
                        NEW_NAME_IV2 = iv2, 
                        NEW_NAME_DV  = dv))

# correlation table
apa.cor.table(dat3, filename = "./tables/correlation_table.doc", table.number = 1,
              show.conf.interval = FALSE, landscape = TRUE)

#######################################
###### Saving Data and Workspace ######
#######################################

# Save current workspace: 
export(dat, "./data/21_two_way_regression.RData")

# save R data file as CSV
export(dat, "./data/21_two_way_regression.csv")

# save R data file as SAV SPSS file
export(dat, "./data/21_two_way_regression.sav")
