#########################################################################
##################### 3-way Simple Effects Testing ######################
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
pacman::p_load(parallel, tidyverse, haven, psych, car, lsr, phia, apaTables)


## load data

# select the approprate manner of importing your data 
# depends on whether you are opening a CSV, or SPSS file on Windows or OSX
# RData files work the best in R. 
# Try to only open RData files to avoid any issues.
# CSV works the next best in R. 
# Try to only save and open CSV files to avoid any issue if you cannot load RData files.
# SPSS files can be buggy to import, especially factors and labels
# only run one of the following commands
# running any of these commands opens a dialog box so you can selet your data file

# use this command if opening a RData file *** least likely to have import/export issues***
load(file.choose())

# use this command if opening a CSV file on Windows or OSX *** next least likely to have import/export issues***
dat <- read.csv(file.choose(), stringsAsFactors = TRUE)

# use this command if opening an SPSS SAV file on OSX
dat <- read_spss(file.choose())

# use this command if opening an SPSS SAV file on Windows
dat <- read_spss(choose.files())

setwd("./PROJECT_NAME/")       # change PROJECT_NAME to your project's name

# check to see that you loaded the correct dataset
View(dat)

# list variables in dataset
glimpse(dat)

#####################################################
###### significant IV1 x IV2 x IV3 interaction ######
#####################################################

# We need to change the default contrast for unordered factors from “cont.treatment” to “contr.helmert”. 
options(contrasts = c("contr.helmert", "contr.poly"))

## R way of Anova using 'car' package
# create ANOVA model
model1 <- lm(dv ~ iv1 * iv2 * iv3, data=dat)

# run ANOVA, rounded to 3 decimals
round(Anova(model1, type = 3), 3)

# eta-squared, rounded to 3 decimals
round(etaSquared(model1, type=3), 3)

# calculate 95% confidence interval, rounded to 3 decimals
round(confint(model1), 3)

## descriptives based on condition 
# note: na.omit() removes any NAs contained within each of the IVs
summarydat <- na.omit(dat %>%
                        group_by(iv1, iv2, iv3) %>%
                        summarise(N    = sum(!is.na(dv)),
                                  mean = round(mean(dv, na.rm=TRUE), 2),
                                  sd   = round(sd(dv, na.rm=TRUE), 2)))
summarydat

# new way of Anova using 'jmv' package, closer to SPSS output
# gives partial eta-squared and omega effect sizes
# also gives a nice descriptive table
pacman::p_load(jmv)

anova(data = dat,
      dep = "dv",
      factors = c("iv1", "iv2", "iv3"),
      effectSize = c("partEta", "omega"),
      descStats = TRUE)

pacman::p_unload(jmv) # jmv package can conflict with other ANOVA packages. need to load/unload it ony for htis analysis

############################################################################
########### Simple effects of IV1 at different IV2 & IV3 levels ############
############################################################################

# quickly examine simple effects
# see https://cran.r-project.org/web/packages/phia/vignettes/phia.pdf

# create interaction model of interest
modinter <- na.omit(lm(dv~ iv1 * iv2 * iv3, data=dat))

# examine simple effects
moderation1 <- testInteractions(modinter, fixed = c("iv2", "iv3"), across="iv1", adjustment="none")

moderation1

etasqiv1.l_l <- (moderation1[1,3]) / ((moderation1[1,3]) + (moderation1[5,3])) # eta-squared iv2-l, iv3-l, across iv3
etasqiv1.h_l <- (moderation1[2,3]) / ((moderation1[2,3]) + (moderation1[5,3])) # eta-squared iv2-h, iv3-l, across iv3
etasqiv1.l_h <- (moderation1[3,3]) / ((moderation1[3,3]) + (moderation1[5,3])) # eta-squared iv2-l, iv3-h, across iv3
etasqiv1.h_h <- (moderation1[4,3]) / ((moderation1[4,3]) + (moderation1[5,3])) # eta-squared iv2-h, iv3-h, across iv3

etasqiv1.l_l
etasqiv1.h_l
etasqiv1.l_h
etasqiv1.h_h

############################################################################
########### Simple effects of IV2 at different IV1 & IV3 levels ############
############################################################################

# examine simple effects
moderation2 <- testInteractions(modinter, fixed = c("iv1", "iv3"), across="iv2", adjustment="none")

moderation2

etasqiv2.l_l <- (moderation2[1,3]) / ((moderation2[1,3]) + (moderation2[5,3])) # eta-squared iv1-l, iv3-l, across iv2
etasqiv2.l_h <- (moderation2[2,3]) / ((moderation2[2,3]) + (moderation2[5,3])) # eta-squared iv1-h, iv3-l, across iv2
etasqiv2.h_l <- (moderation2[3,3]) / ((moderation2[3,3]) + (moderation2[5,3])) # eta-squared iv1-l, iv3-h, across iv2
etasqiv2.h_h <- (moderation2[4,3]) / ((moderation2[4,3]) + (moderation2[5,3])) # eta-squared iv1-h, iv3-h, across iv2

etasqiv2.l_l
etasqiv2.h_l
etasqiv2.l_h
etasqiv2.h_h


############################################################################
########### Simple effects of IV3 at different IV1 & IV2 levels ############
############################################################################

# examine simple effects
moderation3 <- testInteractions(modinter, fixed = c("iv1", "iv3"), across="iv2", adjustment="none")

moderation3

etasqiv3.l_l <- (moderation3[1,3]) / ((moderation3[1,3]) + (moderation3[5,3])) # eta-squared iv1-l, iv3-l, across iv2
etasqiv3.h_l <- (moderation3[2,3]) / ((moderation3[2,3]) + (moderation3[5,3])) # eta-squared iv1-h, iv3-l, across iv2
etasqiv3.l_h <- (moderation3[3,3]) / ((moderation3[3,3]) + (moderation3[5,3])) # eta-squared iv1-l, iv3-h, across iv2
etasqiv3.h_h <- (moderation3[4,3]) / ((moderation3[4,3]) + (moderation3[5,3])) # eta-squared iv1-h, iv3-h, across iv2

etasqiv3.l_l
etasqiv3.h_l
etasqiv3.l_h
etasqiv3.h_h


###############################################
######### Plotting 3-way interaction ##########
###############################################

## good tutorial here
# https://sakaluk.wordpress.com/2015/08/27/6-make-it-pretty-plotting-2-way-interactions-with-ggplot2/

dat1 = describeBy(dat$dv, list(dat$iv1, dat$iv2, dat$iv3), mat=TRUE,digits=2)
names(dat1)[names(dat1) == 'group1'] = 'iv1'
names(dat1)[names(dat1) == 'group2'] = 'iv2'
names(dat1)[names(dat1) == 'group3'] = 'iv3'
dat1$se = dat1$sd/sqrt(dat1$n) # calculates SE for error bars if desired

### tells R to provide a plot in APA style, figure is MS ready
limits = aes(ymax = mean + (1.96*se), ymin=mean - (1.96*se))
dodge = position_dodge(width=0.9)
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times'))

### now the actual figure
figure1 = ggplot(dat1, aes(x = iv1, y = mean, group = iv2)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = iv2)) +
  facet_wrap(  ~ iv3) +
  apatheme+
  coord_cartesian(ylim=c(1,9)) +  # this is the range of the y-axis
  scale_y_continuous(breaks=seq(1,9,1)) + # this is the increment ticks on the y-axis
  ylab('dv') +
  xlab('iv1') +
  scale_fill_grey() 

figure1

## if you like the figure, then can save it to your computer with this command:
# can change file name and storage location as you see fit
# if no path is selected, the the figure will be saved into your R stats working directory
ggsave('./figures/figure1.png', width=6, height=6, unit='in', dpi=300)




#######################################
###### Create descriptives table ######
#######################################

# the below script is set up so that once YOU update the variable names in your analysis, 
# then the below commands will automatically create a descriptives table that can be placed
# directly into your MS, presentation, or poster
# we'll also remove NA values to make this simpler

dat2 = na.omit(select(dat, c(iv1, iv2, iv3, dv)))

# can make multiple tables by changing 'table.number = X

#****NEED TO UPDATE****

#######################################
###### Saving Data and Workspace ######
#######################################

# Save current workspace: 
save(dat, file = "./data/12_three_way_anova.RData")

# Load saved workspace: 
load("./data/12_three_way_anova.Rdata")

# save R data file as CSV
write_csv(dat, "./data/12_three_way_anova.csv")

# save R data file as SAV SPSS file
write_sav(dat, "./data/12_three_way_anova.sav")
