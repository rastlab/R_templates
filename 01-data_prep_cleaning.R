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
pacman::p_load(rio, psych, sjmisc, lubridate, tidyverse, janitor, here, fs)


## load data

# RDS files work the best in R. 
# Try to only open RData/RDS files to avoid any issues.
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


### remove non-reconsenting participants, if needed

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

dat <- dat %>%
  mutate(
    iv1 = case_when(
      iv1_high == 1 ~ 2,
      iv1_low  == 1 ~ 1,
      TRUE     ~ NA_real_))

## Recode IV2 levels....high = 2, low = 1

dat <- dat %>%
  mutate(
    iv2 = case_when(
      iv2_high == 1 ~ 2,
      iv2_low  == 1 ~ 1,
      TRUE     ~ NA_real_))

# reverse code any items required:
# example syntax is for a 1 to 9 scale (10 - x)
dat <- dat %>%
  mutate(dvitem_r = 10 - dvitem)

dat$dvitem_r


# check number of valid responses per condition
sum(!is.na(dat$iv1_high))
sum(!is.na(dat$iv1_low))
sum(!is.na(dat$iv2_high))
sum(!is.na(dat$iv2_low))
sum(!is.na(dat$iv1))
sum(!is.na(dat$iv2))

dat %>% 
  select(iv1_high, iv1_low, iv1, iv2_high, iv2_low, iv2) %>% 
  describe()

##################################################################
####### Adding labels and levels to categorical variables ########
##################################################################

# Set the variables to factors and labels, if experimental manipulations
# if no conditions (e.g., a survey), then create labels and factor levels as need for ethnicity, year in school, etc.

dat <- dat %>%
  mutate(
    iv1 = factor(iv1, levels = c(1, 2), labels = c("Low", "High")))

dat <- dat %>%
  mutate(
    iv2 = factor(iv1, levels = c(1, 2), labels = c("Low", "High")))

dat <- dat %>%
  mutate(
    gender = factor(gender, levels = c(1, 2), labels = c("Male", "Female")))

dat <- dat %>%
  mutate(
    year = factor(year, levels = c(1, 2, 3, 4, 5), labels = c("First","Second", "Third","Fourth", "GradStudent")))

dat <- dat %>%
  mutate(
    ethnicity = factor(ethnicity, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels = c(
      "Indigenous/First Nations", "African", "East Asian", "South Asian", 
      "European", "Hispanic/Latin-American", "Middle Eastern", 
      "Euro-North American", "Pacific Islander", "Other")))


## check IVs to ensure recoding worked corrected for manipulations
dat %>% 
  select(iv1, iv2) %>% 
  describe()

#########################################
###### Recode manipulation timings ######
#########################################

# again, based on Qualtrics data, assume page timings were obtained
## Recode manipulation timings based on default Qualtrics variable naming scheme


dat <- dat %>%
  mutate(
    iv1_timing = ifelse(
      if_all(c(iv1_1_3, iv1_2_3), is.na),
      NA_real_,
      rowSums(pick(iv1_1_3, iv1_2_3), na.rm = TRUE)))

dat <- dat %>%
  mutate(
    iv2_timing = ifelse(
      if_all(c(iv2_1_3, iv2_2_3), is.na),
      NA_real_,
      rowSums(pick(iv2_1_3, iv2_2_3), na.rm = TRUE)))


# summary of page timings
dat %>% 
  select(iv1_timing, iv2_timing) %>% 
  describe()


##########################
###### Creating DVs ######
##########################

## Create DVs

dat <- dat %>%
  mutate(
    avg_dv1 = rowMeans(pick(dv1_1, dv1_2, dv1_3, dv1_4), na.rm = TRUE),
    avg_dv1 = ifelse(is.nan(avg_dv1), NA_real_, avg_dv1))

# could also shorten to this is every variable starts with same string
dat <- dat %>%
  mutate(
    avg_dv1 = rowMeans(pick(starts_with("dv1_")), na.rm = TRUE),
    avg_dv1 = ifelse(is.nan(avg_dv1), NA_real_, avg_dv1))

dat <- dat %>%
  mutate(
    avg_dv2 = rowMeans(pick(dv2_1, dv2_2, dv2_3, dv2_4), na.rm = TRUE),
    avg_dv2 = ifelse(is.nan(avg_dv2), NA_real_, avg_dv2))

# NB: if your variables start with same characters, then could use the following to simplify:
# dat$avg_dv1 <- dat %>% select(starts_with("dv1_")) %>% rowMeans(na.rm = TRUE)

# if difference score is needed:
dat <- dat %>%
  mutate(diff = avg_dv1 - avg_dv2)
  

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

dat %>% 
  select(iv1, iv2, avg_dv1, avg_dv2) %>% 
  describe()

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

# replace PROJECT_NAME with your desire project nam

project_name <- "PROJECT_NAME"

# create standard subdirectories inside project directory
fs::dir_create(fs::path(project_name, c(
  "r",
  "data/raw_data",
  "doc",
  "figures",
  "tables",
  "manuscript")))

# download .gitignore directly into the target project folder
download.file(
  url = "https://raw.githubusercontent.com/rastlab/R_project_template/master/.gitignore", 
  destfile = fs::path(project_name, ".gitignore"))

# Export cleaned dataset using fs::path
export(dat, fs::path("data", "00_data_cleaned.rds")) # save in preferred R format
export(dat, fs::path("data", "00_data_cleaned.csv")) # save as CSV
export(dat, fs::path("data", "00_data_cleaned.sav")) # save as SAV SPSS file
