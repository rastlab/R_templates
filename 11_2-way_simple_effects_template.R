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

## update packages then install these packages if not yet installed
# rm(list = ls())
update.packages(ask = FALSE, checkBuilt = TRUE)
if(!require(pacman)){install.packages("pacman")}
if(!require(jmv)){install.packages("jmv")}
pacman::p_load(parallel, rio, psych, car, lsr, phia, tidyverse, apaTables)


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

# NB: Alt + Shift + K --> will bring up keyboard shortcuts

###############################################
###### significant IV1 x IV2 interaction ######
###############################################

# We need to change the default contrast for unordered factors from “cont.treatment” to “contr.helmert”. 
options(contrasts = c("contr.helmert", "contr.poly"))

## R way of Anova using 'car' package
# create ANOVA model
model1 <- lm(dv ~ iv1 * iv2, data=dat)

# run ANOVA, rounded to 3 decimals
round(Anova(model1, type = 3), 3)

# eta-squared, rounded to 3 decimals
round(etaSquared(model1, type=3), 3)

# calculate 95% confidence interval, rounded to 3 decimals
round(confint(model1), 3)

## descriptives based on condition 
# note: na.omit() removes any NAs contained within each of the IVs

(summarydat <- na.omit(dat %>%
                         group_by(iv1, iv2) %>%
                         summarise(N    = sum(!is.na(dv)),
                                   mean = round(mean(dv, na.rm=TRUE), 2),
                                   sd   = round(sd(dv, na.rm=TRUE), 2))))

# new way of Anova using 'jmv' package, closer to SPSS output
# gives partial eta-squared and omega effect sizes in a nice table
# also gives a nice descriptive table

jmv::anova(data = dat,
      dep = "dv",
      factors = c("iv1", "iv2"),
      effectSize = c("partEta", "omega"),
      postHoc = list(c("iv1", "iv2")),  # calculates simple effects test, NB: gives t rather than F but t^2 = F
      postHocCorr = "none",
      descStats = TRUE)

############################################################################
############# Simple effects of IV1 at different IV2 levels ################
############################################################################

# quickly examine simple effects
# see https://cran.r-project.org/web/packages/phia/vignettes/phia.pdf

# create interaction model of interest
(modinter <- na.omit(lm(dv~ iv1 * iv2, data=dat)))

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

## good tutorial here
# https://sakaluk.wordpress.com/2015/08/27/6-make-it-pretty-plotting-2-way-interactions-with-ggplot2/
# and here
# http://egret.psychol.cam.ac.uk/statistics/R/graphs2.html

(dat1 = describeBy(dat$dv, list(dat$iv1, dat$iv2), mat = TRUE, digits = 2))
names(dat1)[names(dat1) == 'group1'] = 'iv1'
names(dat1)[names(dat1) == 'group2'] = 'iv2'
dat1$se = dat1$sd/sqrt(dat1$n) # calculates SE for error bars if desired

dat1

### tells R to provide a plot in APA style, figure is MS ready
limits = aes(ymax = mean + (1.96*se), ymin=mean - (1.96*se))
dodge = position_dodge(width=0.9)
apatheme=theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times'))

### now the actual figure
(figure1 = ggplot(dat1, aes(x = iv1, y = mean, group = iv2)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = iv2)) +
  geom_errorbar(limits, position=dodge, width=0.25) +
  apatheme +
  coord_cartesian(ylim=c(1,9)) +  # this is the range of the y-axis
  scale_y_continuous(breaks=seq(1,9,1)) + # this is the increment ticks on the y-axis
  ylab('dv') +
  xlab('iv1') +
  scale_fill_grey())

figure1

## if you like the figure, then can save it to your computer with this command:
# can change file name and storage location as you see fit
# if no path is selected, the the figure will be saved into your R stats working directory
ggsave('./figures/figure1.png', width=8, height=6, unit='in', dpi=300)


#######################################
###### Create descriptives table ######
#######################################

# the below script is set up so that once YOU update the variable names in your analysis, 
# then the below commands will automatically create a descriptives table that can be placed
# directly into your MS, presentation, or poster
# we'll also remove NA values to make this simpler

dat2 <- dat %>% select(iv1, iv2, dv) %>% na.omit()

# can make multiple tables by changing 'table.number = X
apa.2way.table(iv1, iv2, dv, data = dat2, landscape = TRUE, table.number = 1, filename="./tables/anova_table.doc")

apa.aov.table(modinter, conf.level = 0.9, type = 3,  table.number = 2, filename="./tables/anovasummary_table.doc")

# correlation matrix

dat3 = na.omit(dat %>% 
                 select(iv1_num, iv2, dv) %>% 
                 rename(NEW_NAME_IV1 = iv1_num, # relabel whatever you want your variables to be named in the manuscript, cannot contain spaces though
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

