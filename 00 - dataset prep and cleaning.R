#############################################
########### Data Prep and Cleaning ##########
#############################################

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
pacman::p_load(parallel, rio, tidyverse, psych, lubridate)


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

################################
###### Recoding Variables ######
################################

# Note: this suggested recoding scheme is based on Qualtrics data 
# where all levels of the random assignment conditions are coded as 1

## Recode IV1 levels....high = 2, low = 1

dat$iv1_high = dat$iv1_high * 2
dat$iv1 <- rowSums((dat[, c('iv1_high', 'iv1_low')]), na.rm = T)
dat$iv1[dat$iv1 == 0] <- NA
dat$iv1

## Recode IV2 levels....high = 2, low = 1

dat$iv2_high = dat$iv2_high * 2
dat$iv2 <- rowSums((dat[, c('iv2_high', 'iv2_low')]), na.rm = T)
dat$iv2[dat$iv2 == 0] <- NA
dat$iv2

# reverse code any items required:
# example syntax is for a 1 to 9 scale (10 - x)
dat$dvitem_r = 10 - dat$dvitem
dat$dvitem_r


# check number of valid responses per condition
sum(!is.na(dat$iv1_high))
sum(!is.na(dat$iv1_low))
sum(!is.na(dat$iv2_high))
sum(!is.na(dat$iv2_low))
sum(!is.na(dat$iv1))
sum(!is.na(dat$iv2))


##################################################################
###### Ensuring R Generates the Same ANOVA F-values as SPSS ######
##################################################################

### for ANOVA output to be the same as SPSS, we have to:
# 1) change IVs to categorical factors
# 2) provide label for new factors
# 3) change the default contrast used in R so it's the same one used in SPSS
# from: https://www.r-bloggers.com/ensuring-r-generates-the-same-anova-f-values-as-spss/

# Set the variables to factors

dat$iv1 = factor(dat$iv1, levels=c(1, 2), labels=c("Low", "High"), exclude = NA)
dat$iv2 = factor(dat$iv2, levels=c(1, 2), labels=c("Low", "High"), exclude = NA)
dat$gender = factor(dat$gender, levels=c(1, 2), labels=c("Male", "Female"), exclude = NA) 
dat$year = factor(dat$year, levels=c(1, 2, 3, 4, 5), 
                  labels=c("First","Second", "Third","Fourth", "GradStudent"), exclude = NA) 

# revise this to match your data
dat$ethnicity = factor(dat$ethnicity, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                       labels=c("Canadian","American", "British", "Chinese", "Dutch","French", 
                                "German", "Irish", "Italian", "Native","Ukrainian", "Other"), 
                       exclude = NA)


## check IVs to ensure recoding worked correct
describe(select(dat, c(iv1, iv2)))


#########################################
###### Recode manipulation timings ######
#########################################

# again, based on Qualtrics data, assume page timings were obtained
## Recode manipulation timings

dat$iv1_timing <- rowSums(dat[, c('iv1_1_3', 'iv1_2_3')], 
                          na.rm = T)
dat$iv2_timing <- rowSums(dat[, c('iv2_1_3', 'iv2_2_3')], 
                          na.rm = T)

# summary of page timings
describe(select(dat, c(iv1_timing, iv2_timing)))



##########################
###### Creating DVs ######
##########################

## Create DVs
dat$avg_dv1 <- rowMeans(dat[, c('dv1_1', 'dv1_2', 'dv1_3', 'dv1_4')], 
                          na.rm = T)

dat$avg_dv2 <- rowMeans(dat[, c('dv2_1', 'dv2_2', 'dv2_3', 'dv2_4')], 
                    na.rm = T)

# if difference score is needed:
dat$diff = with(dat, avg_dv1 - avg_dv2)
  

#######################################
########### Initial Analyses ##########
#######################################

##### Example demographic summaries

# Number of males and females in sample 
table(dat$gender) 

# Percentage of males and females in sample 
table(dat$gender) / sum(table(dat$gender) * 1) 

# Mean of sample age
mean(dat$age, na.rm =  T)

# Number from each ethnicity in sample 
table(dat$ethnicity) 

# Table of gender by faculty
table(dat$gender, dat$ethnicity)

# descriptives of IVs and DVs

describe(select(dat, c(iv1, iv2, avg_dv1, avg_dv2)))

########################################################################
###### Create Project Folder System and Saving Data and Workspace ######
########################################################################

## the following commands create a coherent system for automating the thoughtless parts of a data analysis project
## It provides a log of your work in a clear file/folder hierarchy

# replace PROJECT_NAME with your desire project name
dir.create("./PROJECT_NAME")   # this is where we will save our entire project
setwd("./PROJECT_NAME/")       # this will set the working directory to your new project

# do not modify these lines
dir.create("./r")         # this is where we will save our R scripts
dir.create("./data")      # this is where we will save our datasets
dir.create("./doc")       # this is where we will save our manuscripts and reports
dir.create("./figures")   # this is where we will save our figures
dir.create("./tables")    # this is where we will save our tables

# Save current workspace: 
save(dat, file = "./data/00_data_cleaned.RData")

# Load saved workspace: 
load("./data/00_data_cleaned.Rdata")

# save R data file as CSV
export(dat, file = "./data/00_data_cleaned.csv")

# save R data file as SAV SPSS file
export(dat, "./data/00_data_cleaned.sav")
