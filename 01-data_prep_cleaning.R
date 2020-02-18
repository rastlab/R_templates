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

## Install the required script packages if not yet installed

# Install pacman package if necessary
if(!"pacman" %in% rownames(installed.packages())) install.packages("pacman")
pacman::p_load(parallel, rio, psych, sjmisc, lubridate, tidyverse, janitor)


## load data

# RData files work the best in R. 
# Try to only open RData files to avoid any issues.
# CSV works the next best in R. 
# Try to only save and open CSV files to avoid any issue if you cannot load RData files.
# SPSS files can be buggy to import, especially factors and labels

# the following command will open a dialog box and allow you to select the file you wish to laod
dat <- import(file.choose()) %>%
          clean_names() 

# check to see that you loaded the correct dataset
View(dat)

# list variables in dataset
glimpse(dat)

# NB: Alt + Shift + K --> will bring up keyboard shortcuts

################################
###### Recoding Variables ######
################################


### remove non-reconsenting pparticipants, if needed

# count how many participants did not provide reconsent
sum(!is.na(dat$reconsent[dat$reconsent == 2]))

frq(dat$reconsent)

#remove 2 NAs & 1 no
dat <- filter(dat, (reconsent != 2))
frq(dat$reconsent)


#### recode IVs

# Note: this suggested recoding scheme is based on Qualtrics data 
# where all levels of the random assignment conditions are coded as 1

## Recode IV1 levels....high = 2, low = 1

dat$iv1 <- with(dat, rowSums(cbind((iv1_high * 2), iv1_low), na.rm = T))
dat$iv1[dat$iv1 == 0] <- NA
dat$iv1

## Recode IV2 levels....high = 2, low = 1

dat$iv2 <- with(dat, rowSums(cbind((iv2_high * 2), iv2_low), na.rm = T))
dat$iv2[dat$iv2 == 0] <- NA
dat$iv2

# reverse code any items required:
# example syntax is for a 1 to 9 scale (10 - x)
dat$dvitem_r <- 10 - dat$dvitem
dat$dvitem_r


# check number of valid responses per condition
sum(!is.na(dat$iv1_high))
sum(!is.na(dat$iv1_low))
sum(!is.na(dat$iv2_high))
sum(!is.na(dat$iv2_low))
sum(!is.na(dat$iv1))
sum(!is.na(dat$iv2))

describe(select(dat, iv1_high, iv1_low, iv1, iv2_high, iv2_low, iv2))

##################################################################
####### Adding labels and levels to categorical variables ########
##################################################################

# Set the variables to factors and labels, if experimental manipulations
# if no conditions (e.g., a survey), then create labels and factor levels as need for ethnicity, year in school, etc.

dat$iv1 <- factor(dat$iv1, levels=c(1, 2), labels=c("Low", "High"), exclude = NA)
dat$iv2 <- factor(dat$iv2, levels=c(1, 2), labels=c("Low", "High"), exclude = NA)
dat$gender <- factor(dat$gender, levels=c(1, 2), labels=c("Male", "Female"), exclude = NA) 
dat$year <- factor(dat$year, levels=c(1, 2, 3, 4, 5), 
                  labels=c("First","Second", "Third","Fourth", "GradStudent"), exclude = NA) 

# revise this to match your data
dat$ethnicity <- factor(dat$ethnicity, levels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                       labels=c("Canadian","American", "British", "Chinese", "Dutch","French", 
                                "German", "Irish", "Italian", "Native","Ukrainian", "Other"), 
                       exclude = NA)


## check IVs to ensure recoding worked corrected for manipulations
describe(select(dat, iv1, iv2))

#########################################
###### Recode manipulation timings ######
#########################################

# again, based on Qualtrics data, assume page timings were obtained
## Recode manipulation timings based on default Qualtrics variable naming scheme


dat$iv1_timing <- dat %>% 
                        select(iv1_1_3, iv1_2_3) %>% 
                        rowSums(na.rm = TRUE)

dat$iv2_timing <- dat %>% 
                         select(iv2_1_3, iv2_2_3) %>% 
                         rowSums(na.rm = TRUE)

# summary of page timings
describe(select(dat, iv1_timing, iv2_timing))


##########################
###### Creating DVs ######
##########################

## Create DVs

dat$avg_dv1 <- dat %>% 
                      select(dv1_1, dv1_2, dv1_3, dv1_4) %>% 
                      rowMeans(na.rm = TRUE)

dat$avg_dv2 <- dat %>% 
                      select(dv2_1, dv2_2, dv2_3, dv2_4) %>% 
                      rowMeans(na.rm = TRUE)

# NB: if your variables start with same characters, then could use the following to simplify:
# dat$avg_dv1 <- dat %>% select(starts_with("dv1_")) %>% rowMeans(na.rm = TRUE)

# if difference score is needed:
dat$diff <- with(dat, avg_dv1 - avg_dv2)
  

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

describe(select(dat, iv1, iv2, avg_dv1, avg_dv2))


########################################################################
############### Remove unwanted variables from dataset #################
########################################################################

# get list of variables in dataset
glimpse(dat)

# included any unwanted variables inside the -c()....example: -c(iv1_h, iv1_l)
dat <- select(dat, -c(iv1_h, iv1_l, iv2_h, iv2_l))

glimpse(dat)

########################################################################
###### Create Project Folder System and Saving Data and Workspace ######
########################################################################

## the following commands create a coherent system for automating the thoughtless parts of a data analysis project
## It provides a log of your work in a clear file/folder hierarchy

# replace PROJECT_NAME with your desire project name
if(!exists("./PROJECT_NAME")) {dir.create("./PROJECT_NAME")}   # this is where we will save our entire project
# setwd("./PROJECT_NAME/")       # this will set the working directory to your new project

# do not modify these lines
if(!exists("./r")) {dir.create("./r")}              # this is where we will save our R scripts
if(!exists("./data")) {dir.create("./data")}        # this is where we will save our datasets
if(!exists("./data/raw_data")) {dir.create("./data/raw_data")}        # this is where we will save our datasets
if(!exists("./doc")) {dir.create("./doc")}          # this is where we will save our manuscripts and reports
if(!exists("./figures")) {dir.create("./figures")}  # this is where we will save our figures
if(!exists("./tables")) {dir.create("./tables")}    # this is where we will save our tables
if(!exists("./manuscript")) {dir.create("./manuscript")}    # this is where we will save our tables
download.file(url = "https://raw.githubusercontent.com/rastlab/R_project_template/master/.gitignore", 
              destfile = ".gitignore") # creates recommended .gitignore file

# Save current workspace: 
export(dat, "./data/00_data_cleaned.RData")

# save R data file as CSV
export(dat, "./data/00_data_cleaned.csv")

# save R data file as SAV SPSS file
export(dat, "./data/00_data_cleaned.sav")
