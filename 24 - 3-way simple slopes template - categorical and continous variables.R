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
update.packages(ask=FALSE, checkBuilt = TRUE)
if(!require(pacman)){install.packages("pacman")}
pacman::p_load(parallel, rio,psych, pequod, QuantPsyc, lmSupport,  tidyverse, apaTables)

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

###############################
########## Data Prep ##########
###############################

# if you a dichotomous/trichotomous X continous IVs (opposed a full survey design with only continuous variables)
# then you must turn your dichotomous/trichotomous factor into a numeric variable
# NB: this is how SPSS treats data, but R handles factors differently!
# repeat this for each dichotomous/trichotomous variable, as needed
# R and SPSS output will be different if the experimental variable is not changed into a numeric value here. 

dat$iv1_num <- as.numeric(dat$iv1) # iv1 should be replaced with the name of your dichotomous/trichotomous variable


################################
###### Summary Statistics ######
################################

## descriptive summary table 
# note: na.omit() removes any NAs contained within each of the IVs

dat1 = na.omit(select(dat, iv1, iv2, iv3, dv))
summarydat1 <- describe(dat1)

summarydat1

####### center IVs
# replace iv2 and iv3 names as appropriate if they were previously factors but turned in to numeric variables above
# e.g., iv2/iv3 changed to iv2_num/iv3_num if neccessary 

dat$c.iv1 <- scale(dat$iv1_num, center=TRUE) 
dat$c.iv2 <- scale(dat$iv2, center=TRUE)
dat$c.iv3 <- scale(dat$iv3, center=TRUE)

dat2 <- na.omit(select(dat, c.iv1, c.iv2, c.iv3, dv))
summarydat2 <- describe(dat2)

# verify centering
summarydat2

####### test 3-way regression interaction

### linear regression
step1.1 <- lm(dv ~ c.iv1 + c.iv2 + c.iv3, data=dat)
step2.1 <- lm(dv ~ c.iv1 + c.iv2 + c.iv3 + c.iv1:c.iv2 + c.iv1:c.iv3 + c.iv2:c.iv3, data=dat)
step3.1 <- lm(dv ~ c.iv1 * c.iv2 * c.iv3, data=dat)

# regression summaries for each step
summary(step1.1)
summary(step2.1)
summary(step3.1)

# F-change and Delta R-Squared statistics from here
modelCompare(step1.1, step2.1)
modelCompare(step2.1, step3.1)

# 95% confidence intervals (defaults to 95%), rounded to 3 decimal places
round(confint(step1.1), 3)
round(confint(step2.1), 3)
round(confint(step3.1), 3)

# Betas, rounded to 3 decimal places
round(lm.beta(step1.1), 3)
round(lm.beta(step2.1), 3)
round(lm.beta(step3.1), 3)

### could also achive this differently by doing:

### linear regression
model1 <- na.omit(lmres(dv ~ c.iv1 * c.iv2 * c.iv3, data=dat))

# regression summaries for each step
summary(model1$StepI)
summary(model1$StepII) 
summary(model1)

# F-change statistic from here
model1$F_change

# standardised coefficients (Beta weights)
model1$beta.StepI
model1$beta.StepII
model1$beta.Stepfin


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

dat$c.iv1A <- dat$c.iv1 - sd(dat$c.iv1, na.rm=T)
dat$c.iv1B <- dat$c.iv1 + sd(dat$c.iv1, na.rm=T)
dat$c.iv2A <- dat$c.iv2 - sd(dat$c.iv2, na.rm=T)
dat$c.iv2B <- dat$c.iv2 + sd(dat$c.iv2, na.rm=T)
dat$c.iv3A <- dat$c.iv3 - sd(dat$c.iv2, na.rm=T)
dat$c.iv3B <- dat$c.iv3 + sd(dat$c.iv2, na.rm=T)

## Step 4 of Mike's sheet is not needed in R

## Step 5 & 6 of Mike's sheet

# simple slopes for iv1
iv1.bb <- lm(dv ~ c.iv1 * c.iv2B * c.iv3B, data=dat)
iv1.ab <- lm(dv ~ c.iv1 * c.iv2A * c.iv3B, data=dat)
iv1.ba <- lm(dv ~ c.iv1 * c.iv2B * c.iv3A, data=dat)
iv1.aa <- lm(dv ~ c.iv1 * c.iv2A * c.iv3A, data=dat)

summary(iv1.bb)
summary(iv1.ab)
summary(iv1.bb)
summary(iv1.aa)

# simple slopes for iv2
iv2.bb <- lm(dv ~ c.iv2 * c.iv1B * c.iv3B, data=dat)
iv2.ab <- lm(dv ~ c.iv2 * c.iv1A * c.iv3B, data=dat)
iv2.ba <- lm(dv ~ c.iv2 * c.iv1B * c.iv3A, data=dat)
iv2.aa <- lm(dv ~ c.iv2 * c.iv1A * c.iv3A, data=dat)

summary(iv2.bb)
summary(iv2.ab)
summary(iv2.bb)
summary(iv2.aa)

# simple slopes for iv3
iv3.bb <- lm(dv ~ c.iv3 * c.iv1B * c.iv2B, data=dat)
iv3.ab <- lm(dv ~ c.iv3 * c.iv1A * c.iv2B, data=dat)
iv3.ba <- lm(dv ~ c.iv3 * c.iv1B * c.iv2A, data=dat)
iv3.aa <- lm(dv ~ c.iv3 * c.iv1A * c.iv2A, data=dat)

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
par(bty = 'l')
par(family="Times")
plot(c(-1, 1), c((s_slopes1$Points[1, 1]), (s_slopes1$Points[1, 2])), type='b', lty=1, pch = 15, axes=F, xlab="", ylab="", ylim=yrange, xlim=xrange)
par(new = T)
plot(c(-1, 1), c((s_slopes1$Points[3, 1]), (s_slopes1$Points[3, 2])), type='b', lty=2, pch = 17, axes=F, xlab="iv1", ylab="dv", ylim=yrange, xlim=xrange)
axis(1, at=c(-1, 1), labels=c("iv1-low", "iv1-high"))
axis(2, at=c(4, 5, 6, 7))
legend("topright", title = "iv2", c("iv2-low (-1SD)", "iv2-high (+1SD)"), lty=1:2, cex=.90)
title(main="Panel B: iv3 (low -1SD)", adj = 0, font.main = 1, cex.main = 1)
box()
dev.print(png, './figures/figure1.png', width = 480, height = 480)


### Plot iv1 as slope and iv2 as moderator and iv3 (high +1SD) as panel
yrange = c(4,7)  # modify the y-axis range
xrange = c(-1.5,1.5)
par(bty = 'l')
par(family="Times")
plot(c(-1, 1), c((s_slopes1$Points[2, 1]), (s_slopes1$Points[2, 2])), type='b', lty=1, pch = 15, axes=F, xlab="", ylab="", ylim=yrange, xlim=xrange)
par(new = T)
plot(c(-1, 1), c((s_slopes1$Points[4, 1]), (s_slopes1$Points[4, 2])), type='b', lty=2, pch = 17, axes=F, xlab="iv1", ylab="dv", ylim=yrange, xlim=xrange)
axis(1, at=c(-1, 1), labels=c("iv1-low", "iv1-high"))
axis(2, at=c(4, 5, 6, 7))
legend("topright", title = "iv2", c("iv2-low (-1SD)", "iv2-high (+1SD)"), lty=1:2, cex=.90)
title(main="Panel B: iv3 (high +1SD)", adj = 0, font.main = 1, cex.main = 1)
box()
dev.print(png, './figures/figure2.png', width = 480, height = 480)


### Plot iv2 as slope and iv1 as moderator and iv3 (low -1SD) as panel
yrange = c(4,7)  # modify the y-axis range
xrange = c(-1.5,1.5)
par(bty = 'l')
par(family="Times")
plot(c(-1, 1), c((s_slopes2$Points[1, 1]), (s_slopes2$Points[1, 2])), type='b', lty=1, pch = 15, axes=F, xlab="", ylab="", ylim=yrange, xlim=xrange)
par(new = T)
plot(c(-1, 1), c((s_slopes2$Points[3, 1]), (s_slopes2$Points[3, 2])), type='b', lty=2, pch = 17, axes=F, xlab="iv2", ylab="dv", ylim=yrange, xlim=xrange)
axis(1, at=c(-1, 1), labels=c("iv2-low", "iv2-high"))
axis(2, at=c(4, 5, 6, 7))
legend("topright", title = "iv1", c("iv1-low (-1SD)", "iv1-high (+1SD)"), lty=1:2, cex=.90)
title(main="Panel B: iv3 (low -1SD)", adj = 0, font.main = 1, cex.main = 1)
box()
dev.print(png, './figures/figure3.png', width = 480, height = 480)


### Plot iv2 as slope and iv1 as moderator and iv3 (high +1SD) as panel
yrange = c(4,7)  # modify the y-axis range
xrange = c(-1.5,1.5)
par(bty = 'l')
par(family="Times")
plot(c(-1, 1), c((s_slopes2$Points[2, 1]), (s_slopes2$Points[2, 2])), type='b', lty=1, pch = 15, axes=F, xlab="", ylab="", ylim=yrange, xlim=xrange)
par(new = T)
plot(c(-1, 1), c((s_slopes2$Points[4, 1]), (s_slopes2$Points[4, 2])), type='b', lty=2, pch = 17, axes=F, xlab="iv2", ylab="dv", ylim=yrange, xlim=xrange)
axis(1, at=c(-1, 1), labels=c("iv2-low", "iv2-high"))
axis(2, at=c(4, 5, 6, 7))
legend("topright", title = "iv1", c("iv1-low (-1SD)", "iv1-high (+1SD)"), lty=1:2, cex=.90)
title(main="Panel B: iv3 (high +1SD)", adj = 0, font.main = 1, cex.main = 1)
box()
dev.print(png, './figures/figure4.png', width = 480, height = 480)


### Plot iv3 as slope and iv1 as moderator and iv2 (low -1SD) as panel
yrange = c(4,7)  # modify the y-axis range
xrange = c(-1.5,1.5)
par(bty = 'l')
par(family="Times")
plot(c(-1, 1), c((s_slopes3$Points[1, 1]), (s_slopes3$Points[1, 2])), type='b', lty=1, pch = 15, axes=F, xlab="", ylab="", ylim=yrange, xlim=xrange)
par(new = T)
plot(c(-1, 1), c((s_slopes3$Points[3, 1]), (s_slopes3$Points[3, 2])), type='b', lty=2, pch = 17, axes=F, xlab="iv3", ylab="dv", ylim=yrange, xlim=xrange)
axis(1, at=c(-1, 1), labels=c("iv3-low", "iv3-high"))
axis(2, at=c(4, 5, 6, 7))
legend("topright", title = "iv1", c("iv1-low (-1SD)", "iv1-high (+1SD)"), lty=1:2, cex=.90)
title(main="Panel B: iv2 (low -1SD)", adj = 0, font.main = 1, cex.main = 1)
box()
dev.print(png, './figures/figure5.png', width = 480, height = 480)


### Plot iv3 as slope and iv1 as moderator and iv2 (high +1SD) as panel
yrange = c(4,7)  # modify the y-axis range
xrange = c(-1.5,1.5)
par(bty = 'l')
par(family="Times")
plot(c(-1, 1), c((s_slopes3$Points[2, 1]), (s_slopes3$Points[2, 2])), type='b', lty=1, pch = 15, axes=F, xlab="", ylab="", ylim=yrange, xlim=xrange)
par(new = T)
plot(c(-1, 1), c((s_slopes2$Points[4, 1]), (s_slopes2$Points[4, 2])), type='b', lty=2, pch = 17, axes=F, xlab="iv3", ylab="dv", ylim=yrange, xlim=xrange)
axis(1, at=c(-1, 1), labels=c("iv3-low", "iv3-high"))
axis(2, at=c(4, 5, 6, 7))
legend("topright", title = "iv1", c("iv1-low (-1SD)", "iv1-high (+1SD)"), lty=1:2, cex=.90)
title(main="Panel B: iv2 (high +1SD)", adj = 0, font.main = 1, cex.main = 1)
box()
dev.print(png, './figures/figure6.png', width = 480, height = 480)


#######################################
###### Create descriptives table ######
#######################################

# the below script is set up so that once YOU update the variable names in your analysis, 
# then the below commands will automatically create a descriptives table that can be placed
# directly into your MS, presentation, or poster
# we'll also remove NA values to make this simpler

dat3 = na.omit(dat %>% 
                 select(iv1_num, iv2, iv3, dv) %>% 
                 rename(NEW_NAME_IV1 = iv1_num, # relabel whatever you want your variables to be named in the manuscript, cannot contain spaces though
                        NEW_NAME_IV2 = iv2, 
                        NEW_NAME_IV2 = iv3,
                        NEW_NAME_DV = dv))

# correlation table
apa.cor.table(dat3, filename = "./tables/correlation_table.doc", table.number = 1,
              show.conf.interval = FALSE, landscape = TRUE)

# regression table
apa.reg.table(step1.1, step2.1, step3.1, filename = "./tables/regression_table.doc", table.number = 2)

#######################################
###### Saving Data and Workspace ######
#######################################
# Save current workspace: 
export(dat, "./data/24_three_way_regression.RData")

# save R data file as CSV
export(dat, "./data/24_three_way_regression.csv")

# save R data file as SAV SPSS file
export(dat, "./data/24_three_way_regression.sav")

