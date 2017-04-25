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
update.packages(ask=FALSE, checkBuilt = TRUE)
if(!require(tidyverse)){install.packages('tidyverse')}
if(!require(car)){install.packages('car')}
if(!require(psych)){install.packages('psych')}
if(!require(HH)){install.packages('HH')}
if(!require(apaTables)){install.packages('apaTables')}
if(!require(phia)){install.packages('phia')}

## looad libraries
library("psych")
library("car")
library("plyr")
library("tidyverse")
library("haven")
library("apaTables")
library("jmv")
library("phia")

## load data

# select the approprate manner of importing your data 
# depends on whether you are opening a CSV, or SPSS file on Windows or OSX
# RData files work the best in R. 
# Try to only save and open RData files to avoid any issues.
# CSV works the next best in R. 
# Try to only save and open CSV files to avoid any issue if you cannot load RData files.
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

###############################################
###### significant IV1 x IV2 interaction ######
###############################################

# We need to change the default contrast for unordered factors from “cont.treatment” to “contr.helmert”. 
options(contrasts = c("contr.helmert", "contr.poly"))

## R way of Anova using 'car' package
Anova(lm(dv ~ iv1 * iv2, data=dat), type = 3)

# calculate 95% confidence interval 

confint(lm(dv ~ iv1 * iv2, data=dat))

## descriptives based on condition 
# note: na.omit() removes any NAs contained within each of the IVs

summarydat <- na.omit(dat %>%
                         group_by(iv1, iv2) %>%
                         summarise(N    = sum(!is.na(dv)),
                                   mean = round(mean(dv, na.rm=TRUE), 2),
                                   sd   = round(sd(dv, na.rm=TRUE), 2)))
summarydat

# new way of Anova using 'jmv' package, closer to SPSS output
# gives partial eta-squared and omega effect sizes
# also gives a nice descriptive table

anova(data = dat,
      dep = "dv",
      factors = c("iv1", "iv2"),
      effectSize = c("partEta", "omega"),
      descStats = TRUE)


############################################################################
############# Simple effects of IV1 at different IV2 levels ################
############################################################################

# quickly examine simple effects
# see https://cran.r-project.org/web/packages/phia/vignettes/phia.pdf

# create interaction model 
modinter <- na.omit(lm(dv~ iv1 * iv2, data=dat))

# quickly examine simple effects
moderation1 <- testInteractions(modinter, fixed = c("iv2"), across="iv1", adjustment="none")

(moderation1[1,3]) / ((moderation1[1,3]) + (moderation1[3,3])) # eta-squared iv2-l, across iv1
(moderation1[2,3]) / ((moderation1[2,3]) + (moderation1[3,3])) # eta-squared iv2-h, across iv1

## manually calculate simple effects

#### simple effects for 2-way interaction
## 1) need to create a subset of data along each level of our IVs
dat.iv1h = subset(dat, iv1 == "High")
dat.iv1l = subset(dat, iv1 == "Low")
dat.iv2h = subset(dat, iv2 == "High")
dat.iv2l = subset(dat, iv2 == "Low")

## 2) run ANOVAs testing the impact of IV1 within IV2 and IV2 within IV1

fullModelIII = Anova(lm(dv ~ iv1 * iv2, dat), type = 3)
part1ModelIII = Anova(lm(dv ~ iv1, dat.iv2h), type = 3)
part2ModelIII = Anova(lm(dv ~ iv1, dat.iv2l), type = 3)

# Full ANOVA Model
SSwin <- fullModelIII[5,1]
dfwin <-fullModelIII[5,2]
MSwin<- (SSwin/dfwin)

# Simple effect for IV1 at High
SSSE1<-part1ModelIII[2,1]
dfSE1<-part1ModelIII[2,2]
MSSE1<- (SSSE1/dfSE1)
FSE1 <-(MSSE1/MSwin)
pSE1 <- 1-pf(c(FSE1), df1=dfSE1, df2=dfwin, lower.tail=TRUE) 
etasqSE1 <- (SSSE1 / (SSSE1+SSwin))

# Simple effect for IV1 at Low
SSSE2<-part2ModelIII[2,1]
dfSE2<-part2ModelIII[2,2]
MSSE2<- (SSSE2/dfSE2)
FSE2 <-(MSSE2/MSwin)
pSE2 <- 1-pf(c(FSE2), df1=dfSE2, df2=dfwin, lower.tail=TRUE) 
etasqSE2 <- (SSSE2 / (SSSE2+SSwin))

# part1ModelIII F  (IV1 at High)
FSE1
# part1ModelIII p
pSE1
# part1ModelIII Eta-squared 
etasqSE1

# part2ModelIII F (IV1 at Low)
FSE2
# part2ModelIII p
pSE2
# part2ModelIII Eta-Squared 
etasqSE2

############################################################################
############# Simple effects of IV2 at different IV1 levels ################
############################################################################

# quickly examine simple effects
moderation2 <- testInteractions(modinter, fixed = c("iv1"), across="iv2", adjustment="none")

(moderation2[1,3]) / ((moderation2[1,3]) + (moderation2[3,3])) # eta-squared iv1-l, across iv2
(moderation2[2,3]) / ((moderation2[2,3]) + (moderation2[3,3])) # eta-squared iv1-h, across iv2


## manually calculate simple effects

#### simple effects for 2-way interaction
fullModelIII = Anova(lm(dv ~ iv1 * iv2, dat), type = 3)
part3ModelIII = Anova(lm(dv ~ iv2, dat.iv1h), type = 3)
part4ModelIII = Anova(lm(dv ~ iv2, dat.iv1l), type = 3)

# Full ANOVA Model
SSwin <- fullModelIII[5,1]
dfwin <-fullModelIII[5,2]
MSwin<- (SSwin/dfwin)

# Simple effect for IV2 at High
SSSE3<-part3ModelIII[2,1]
dfSE3<-part3ModelIII[2,2]
MSSE3<- (SSSE3/dfSE3)
FSE3 <-(MSSE3/MSwin)
pSE3 <- 1-pf(c(FSE3), df1=dfSE3, df2=dfwin, lower.tail=TRUE) 
etasqSE3 <- (SSSE3 / (SSSE3+SSwin))

# Simple effect for IV2 at Low
SSSE4<-part4ModelIII[2,1]
dfSE4<-part4ModelIII[2,2]
MSSE4<- (SSSE4/dfSE4)
FSE4 <-(MSSE4/MSwin)
pSE4 <- 1-pf(c(FSE4), df1=dfSE4, df2=dfwin, lower.tail=TRUE) 
etasqSE4 <- (SSSE4 / (SSSE4+SSwin))

# part3ModelIII F  (IV2 at High)
FSE3
# part2ModelIII p
pSE3
# part2ModelIII Eta-Squared 
etasqSE3

# part4ModelIII F (IV2 at Low)
FSE4
# part4ModelIII p
pSE4
# part4ModelIII Eta-Squared 
etasqSE4

###############################################
######### Plotting 2-way interaction ##########
###############################################

## good tutorial here
# https://sakaluk.wordpress.com/2015/08/27/6-make-it-pretty-plotting-2-way-interactions-with-ggplot2/
# and here
# http://egret.psychol.cam.ac.uk/statistics/R/graphs2.html

dat1 = describeBy(dat$dv, list(dat$iv1, dat$iv2), mat=TRUE,digits=2)
names(dat1)[names(dat1) == 'group1'] = 'iv1'
names(dat1)[names(dat1) == 'group2'] = 'iv2'
dat1$se = dat1$sd/sqrt(dat1$n) # calculates SE for error bars if desired

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
figure1 = ggplot(dat1, aes(x = iv1, y = mean, group = iv2)) +
  geom_bar(stat = "identity", position = "dodge", aes(fill = iv2)) +
  geom_errorbar(limits, position=dodge, width=0.25) +
  apatheme +
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

dat2 = na.omit(select(dat, c(iv1, iv2, dv)))

# can make multiple tables by changing 'table.number = X
apa.cor.table(dat2, filename = "./tables/table.doc", table.number = 1,
              show.conf.interval = FALSE, landscape = TRUE)

#######################################
###### Saving Data and Workspace ######
#######################################

# Save current workspace: 
save(dat, file = "./data/11_two_way_anova.RData")

# Load saved workspace: 
load("./data/11_two_way_anova.Rdata")

# save R data file as CSV
write.csv(dat, file = "./data/11_two_way_anova.csv")

# save R data file as SAV SPSS file
write_sav(dat, "./data/11_two_way_anova.sav")

