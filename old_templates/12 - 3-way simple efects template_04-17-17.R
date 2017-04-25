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
update.packages(ask=FALSE, checkBuilt = TRUE)
if(!require(tidyverse)){install.packages('tidyverse')}
if(!require(car)){install.packages('car')}
if(!require(psych)){install.packages('psych')}
if(!require(HH)){install.packages('HH')}
if(!require(apaTables)){install.packages('apaTables')}
if(!require(jmv)){install.packages('jmv')}
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

#####################################################
###### significant IV1 x IV2 x IV3 interaction ######
#####################################################

# We need to change the default contrast for unordered factors from “cont.treatment” to “contr.helmert”. 
options(contrasts = c("contr.helmert", "contr.poly"))

## R way of Anova using 'car' package
Anova(lm(dv ~ iv1 * iv2 * iv3, data=dat), type = 3)

# calculate 95% confidence interval 

confint(lm(dv ~ iv1 * iv2 * iv3, data=dat))

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

anova(data = dat,
      dep = "dv",
      factors = c("iv1", "iv2", "iv3"),
      effectSize = c("partEta", "omega"),
      descStats = TRUE)


############################################################################
########### Simple effects of IV1 at different IV2 & IV3 levels ############
############################################################################

# quickly examine simple effects
# see https://cran.r-project.org/web/packages/phia/vignettes/phia.pdf

# create interaction model 
modinter <- na.omit(lm(dv~ iv1 * iv2 * iv3, data=dat))

# quickly examine simple effects
moderation1 <- testInteractions(modinter, fixed = c("iv2", "iv3"), across="iv1", adjustment="none")

(moderation1[1,3]) / ((moderation1[1,3]) + (moderation1[5,3])) # eta-squared iv2-l, iv3-l, across iv3
(moderation1[2,3]) / ((moderation1[2,3]) + (moderation1[5,3])) # eta-squared iv2-h, iv3-l, across iv3
(moderation1[3,3]) / ((moderation1[3,3]) + (moderation1[5,3])) # eta-squared iv2-l, iv3-h, across iv3
(moderation1[4,3]) / ((moderation1[4,3]) + (moderation1[5,3])) # eta-squared iv2-h, iv3-h, across iv3

# manually examine simple effects
#### simple effects for 3-way interaction
## 1) need to create a subset of data along each level of our IVs
dat.iv1h_iv2h = subset(dat, iv1 == "High" & iv2 == "High")
dat.iv1h_iv2l = subset(dat, iv1 == "High" & iv2 == "Low")
dat.iv1l_iv2h = subset(dat, iv1 == "Low" & iv2 == "High")
dat.iv1l_iv2l = subset(dat, iv1 == "Low" & iv2 == "Low")

dat.iv1h_iv3h = subset(dat, iv1 == "High" & iv3 == "High")
dat.iv1h_iv3l = subset(dat, iv1 == "High" & iv3 == "Low")
dat.iv1l_iv3h = subset(dat, iv1 == "Low" & iv3 == "High")
dat.iv1l_iv3l = subset(dat, iv1 == "Low" & iv3 == "Low")

dat.iv2h_iv3h = subset(dat, iv2 == "High" & iv3 == "High")
dat.iv2h_iv3l = subset(dat, iv2 == "High" & iv3 == "Low")
dat.iv2l_iv3h = subset(dat, iv2 == "Low" & iv3 == "High")
dat.iv2l_iv3l = subset(dat, iv2 == "Low" & iv3 == "Low")


## 2) run ANOVAs testing the impact of IV1 within IV2 and IV3 and so forth

fullModelIII = Anova(lm(dv ~ iv1 * iv2 * iv3, data=dat), type = 3)
part1ModelIII = Anova(lm(dv ~ iv1, dat.iv2h_iv3h), type = 3)
part2ModelIII = Anova(lm(dv ~ iv1, dat.iv2h_iv3l), type = 3)
part3ModelIII = Anova(lm(dv ~ iv1, dat.iv2l_iv3h), type = 3)
part4ModelIII = Anova(lm(dv ~ iv1, dat.iv2l_iv3l), type = 3)

# Full ANOVA Model
SSwin <- fullModelIII[9,1]
dfwin <-fullModelIII[9,2]
MSwin<- (SSwin/dfwin)

# Simple effect for IV1 at High/High
SSSE1<-part1ModelIII[2,1]
dfSE1<-part1ModelIII[2,2]
MSSE1<- (SSSE1/dfSE1)
FSE1 <-(MSSE1/MSwin)
pSE1 <- 1-pf(c(FSE1), df1=dfSE1, df2=dfwin, lower.tail=TRUE) 
etasqSE1 <- (SSSE1 / (SSSE1+SSwin))

# Simple effect for IV1 at High/Low
SSSE2<-part2ModelIII[2,1]
dfSE2<-part2ModelIII[2,2]
MSSE2<- (SSSE2/dfSE2)
FSE2 <-(MSSE2/MSwin)
pSE2 <- 1-pf(c(FSE2), df1=dfSE2, df2=dfwin, lower.tail=TRUE) 
etasqSE2 <- (SSSE2 / (SSSE2+SSwin))

# Simple effect for IV1 at Low/High
SSSE3<-part3ModelIII[2,1]
dfSE3<-part3ModelIII[2,2]
MSSE3<- (SSSE3/dfSE3)
FSE3 <-(MSSE3/MSwin)
pSE3 <- 1-pf(c(FSE3), df1=dfSE3, df2=dfwin, lower.tail=TRUE) 
etasqSE3 <- (SSSE3 / (SSSE3+SSwin))

# Simple effect for IV1 at Low/Low
SSSE4<-part4ModelIII[2,1]
dfSE4<-part4ModelIII[2,2]
MSSE4<- (SSSE4/dfSE4)
FSE4 <-(MSSE4/MSwin)
pSE4 <- 1-pf(c(FSE4), df1=dfSE4, df2=dfwin, lower.tail=TRUE) 
etasqSE4 <- (SSSE4 / (SSSE4+SSwin))

# part1ModelIII F (IV1 at High/High)
FSE1
# part1ModelIII p
pSE1
# part1ModelIII Eta-squared 
etasqSE1

# part2ModelIII F (IV1 at High/Low)
FSE2
# part2ModelIII p
pSE2
# part2ModelIII Eta-Squared 
etasqSE2

# part3ModelIII F (IV1 at Low/High)
FSE3
# part2ModelIII p
pSE3
# part2ModelIII Eta-Squared 
etasqSE3

# part4ModelIII F (IV1 at Low/Low)
FSE4
# part4ModelIII p
pSE4
# part4ModelIII Eta-Squared 
etasqSE4





############################################################################
########### Simple effects of IV2 at different IV1 & IV3 levels ############
############################################################################

# quickly examine simple effects
moderation2 <- testInteractions(modinter, fixed = c("iv1", "iv3"), across="iv2", adjustment="none")

(moderation2[1,3]) / ((moderation2[1,3]) + (moderation2[5,3])) # eta-squared iv1-l, iv3-l, across iv2
(moderation2[2,3]) / ((moderation2[2,3]) + (moderation2[5,3])) # eta-squared iv1-h, iv3-l, across iv2
(moderation2[3,3]) / ((moderation2[3,3]) + (moderation2[5,3])) # eta-squared iv1-l, iv3-h, across iv2
(moderation2[4,3]) / ((moderation2[4,3]) + (moderation2[5,3])) # eta-squared iv1-h, iv3-h, across iv2

# manually examine simple effects
fullModelIII = Anova(lm(dv ~ iv1 * iv2 * iv3, data=dat), type = 3)
part5ModelIII = Anova(lm(dv ~ iv2, dat.iv1h_iv3h), type = 3)
part6ModelIII = Anova(lm(dv ~ iv2, dat.iv1h_iv3l), type = 3)
part7ModelIII = Anova(lm(dv ~ iv2, dat.iv1l_iv3h), type = 3)
part8ModelIII = Anova(lm(dv ~ iv2, dat.iv1l_iv3l), type = 3)

# Full ANOVA Model
SSwin <- fullModelIII[9,1]
dfwin <-fullModelIII[9,2]
MSwin<- (SSwin/dfwin)

# Simple effect for IV2 at High/High
SSSE5<-part5ModelIII[2,1]
dfSE5<-part5ModelIII[2,2]
MSSE5<- (SSSE5/dfSE5)
FSE5 <-(MSSE5/MSwin)
pSE5 <- 1-pf(c(FSE5), df1=dfSE5, df2=dfwin, lower.tail=TRUE) 
etasqSE5 <- (SSSE5 / (SSSE5+SSwin))

# Simple effect for IV2 at High/Low
SSSE6<-part6ModelIII[2,1]
dfSE6<-part6ModelIII[2,2]
MSSE6<- (SSSE6/dfSE6)
FSE6 <-(MSSE6/MSwin)
pSE6 <- 1-pf(c(FSE6), df1=dfSE6, df2=dfwin, lower.tail=TRUE) 
etasqSE6 <- (SSSE6 / (SSSE6+SSwin))

# Simple effect for IV2 at Low/High
SSSE7<-part7ModelIII[2,1]
dfSE7<-part7ModelIII[2,2]
MSSE7<- (SSSE7/dfSE7)
FSE7 <-(MSSE7/MSwin)
pSE7 <- 1-pf(c(FSE7), df1=dfSE7, df2=dfwin, lower.tail=TRUE) 
etasqSE7 <- (SSSE7 / (SSSE7+SSwin))

# Simple effect for IV2 at Low/Low
SSSE8<-part8ModelIII[2,1]
dfSE8<-part8ModelIII[2,2]
MSSE8<- (SSSE8/dfSE8)
FSE8 <-(MSSE8/MSwin)
pSE8 <- 1-pf(c(FSE8), df1=dfSE8, df2=dfwin, lower.tail=TRUE) 
etasqSE8 <- (SSSE8 / (SSSE8+SSwin))

# part5ModelIII F (IV2 at High/High)
FSE5
# part5ModelIII p
pSE5
# part5ModelIII Eta-squared 
etasqSE5

# part6ModelIII F (IV2 at High/Low)
FSE6
# part6ModelIII p
pSE6
# part6ModelIII Eta-Squared 
etasqSE6

# part7ModelIII F (IV2 at Low/High)
FSE7
# part7ModelIII p
pSE7
# part7ModelIII Eta-Squared 
etasqSE7

# part8ModelIII F (IV2 at Low/Low)
FSE8
# part8ModelIII p
pSE8
# part8ModelIII Eta-Squared 
etasqSE8





############################################################################
########### Simple effects of IV3 at different IV1 & IV2 levels ############
############################################################################

# quickly examine simple effects
moderation3 <- testInteractions(modinter, fixed = c("iv1", "iv3"), across="iv2", adjustment="none")

(moderation3[1,3]) / ((moderation3[1,3]) + (moderation3[5,3])) # eta-squared iv1-l, iv3-l, across iv2
(moderation3[2,3]) / ((moderation3[2,3]) + (moderation3[5,3])) # eta-squared iv1-h, iv3-l, across iv2
(moderation3[3,3]) / ((moderation3[3,3]) + (moderation3[5,3])) # eta-squared iv1-l, iv3-h, across iv2
(moderation3[4,3]) / ((moderation3[4,3]) + (moderation3[5,3])) # eta-squared iv1-h, iv3-h, across iv2

# manually examine simple effects
fullModelIII = Anova(lm(dv ~ iv1 * iv2 * iv3, data=dat), type = 3)
part9ModelIII = Anova(lm(dv ~ iv3, dat.iv1h_iv2h), type = 3)
part10ModelIII = Anova(lm(dv ~ iv3, dat.iv1h_iv2l), type = 3)
part11ModelIII = Anova(lm(dv ~ iv3, dat.iv1l_iv2h), type = 3)
part12ModelIII = Anova(lm(dv ~ iv3, dat.iv1l_iv2l), type = 3)

# Full ANOVA Model
SSwin <- fullModelIII[9,1]
dfwin <-fullModelIII[9,2]
MSwin<- (SSwin/dfwin)

# Simple effect for IV3 at High/High
SSSE9<-part9ModelIII[2,1]
dfSE9<-part9ModelIII[2,2]
MSSE9<- (SSSE9/dfSE9)
FSE9 <-(MSSE9/MSwin)
pSE9 <- 1-pf(c(FSE9), df1=dfSE9, df2=dfwin, lower.tail=TRUE) 
etasqSE9 <- (SSSE9 / (SSSE9+SSwin))

# Simple effect for IV3 at High/Low
SSSE10<-part10ModelIII[2,1]
dfSE10<-part10ModelIII[2,2]
MSSE10<- (SSSE10/dfSE10)
FSE10 <-(MSSE10/MSwin)
pSE10 <- 1-pf(c(FSE10), df1=dfSE10, df2=dfwin, lower.tail=TRUE) 
etasqSE10 <- (SSSE10 / (SSSE10+SSwin))

# Simple effect for IV3 at Low/High
SSSE11<-part11ModelIII[2,1]
dfSE11<-part11ModelIII[2,2]
MSSE11<- (SSSE11/dfSE11)
FSE11 <-(MSSE11/MSwin)
pSE11 <- 1-pf(c(FSE11), df1=dfSE11, df2=dfwin, lower.tail=TRUE) 
etasqSE11 <- (SSSE11 / (SSSE11+SSwin))

# Simple effect for IV3 at Low/Low
SSSE12<-part12ModelIII[2,1]
dfSE12<-part12ModelIII[2,2]
MSSE12<- (SSSE12/dfSE12)
FSE12 <-(MSSE12/MSwin)
pSE12 <- 1-pf(c(FSE12), df1=dfSE12, df2=dfwin, lower.tail=TRUE) 
etasqSE12 <- (SSSE12 / (SSSE12+SSwin))

# part9ModelIII F (IV3 at High/High)
FSE9
# part9ModelIII p
pSE9
# part9ModelIII Eta-squared 
etasqSE9

# part10ModelIII F (IV3 at Low/High)
FSE10
# part10ModelIII p
pSE10
# part10ModelIII Eta-Squared 
etasqSE10

# part11ModelIII F (IV3 at Low/High)
FSE11
# part11ModelIII p
pSE11
# part11ModelIII Eta-Squared 
etasqSE11

# part12ModelIII F(IV3 at Low/Low)
FSE12
# part12ModelIII p
pSE12
# part12ModelIII Eta-Squared 
etasqSE12




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
apa.cor.table(dat2, filename = "./tables/table.doc", table.number = 1,
              show.conf.interval = FALSE, landscape = TRUE)

#######################################
###### Saving Data and Workspace ######
#######################################

# Save current workspace: 
save(dat, file = "./data/12_three_way_anova.RData")

# Load saved workspace: 
load("./data/12_three_way_anova.Rdata")

# save R data file as CSV
write.csv(dat, file = "./data/12_three_way_anova.csv")

# save R data file as SAV SPSS file
write_sav(dat, "./data/12_three_way_anova.sav")
