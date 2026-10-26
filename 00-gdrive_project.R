####################################################
########### Google Drive Project Template ##########
####################################################


####################################
########## load libraries ##########
####################################

## Install the required script packages if not yet installed

# Install pacman package if necessary
if(!"pacman" %in% rownames(installed.packages())) install.packages("pacman")

pacman::p_load(googledrive, dplyr, readr, here)

#####################################
########## Google Drive #############
#####################################

## vignette for 'googledrive' here: https://googledrive.tidyverse.org/

## login to Google Drive

# Authenticate with Google Drive.
# Note: Google disabled Out-Of-Bounds (use_oob = TRUE) auth in 2022.
# Default drive_auth() opens standard interactive OAuth in your browser.
drive_auth()

##########################################################################
########## locate files or folders in Google Drive #######################
##########################################################################

# show root folder directories
drive_ls("~/")

# if using Google Shared Drives then use this command:
shared_drive_find()

# Confirm that the parent folder is the one you expect before creating anything
drive_get(as_id(parent_id))

##################################################################
############## create template in Google Drive ###################
##################################################################

## create the GPL"s Project Template on your Google Drive within RStudio 


# make new project folder using Google Drive file 'id'
project <- drive_mkdir("PROJECT_NAME_HERE",                    # project name here
                       parent = as_id('GDRIVE_FILE_ID_HERE'))  # insert your project 'id' here

project_lit <- drive_mkdir("Literature", 
                           parent = as_id(project$id))

project_mat <- drive_mkdir("Materials and Methods", 
                           parent = as_id(project$id))

project_doc <- drive_mkdir("Notes and Documentation", 
                           parent = as_id(project$id))

project_ana <- drive_mkdir("Analysis Scripts", 
                           parent = as_id(project$id))

project_dat <- drive_mkdir("Data", 
                           parent = as_id(project$id))

project_raw <- drive_mkdir("Raw Data", 
                           parent = as_id(project_dat$id))

project_out <- drive_mkdir("Scholarly Output", 
                           parent = as_id(project$id))

project_reg <- drive_mkdir("Registration", 
                           parent = as_id(project$id))

# copy registration form into your project folder
registration_form <- drive_cp(as_id('14qNm5tLQhOrDs_Q-eh3e25hv4nXZVHBdbpsdVc9_Ywg'),
                              path = project_reg,
                              name = "Pre-Registration of Project")


# save Google Drive 'ids' to you can use them later if necessary
if(!exists("./project_info")) {dir.create("./project_info")}    # this is where we will save our tables
rio::export(as.data.frame(project_info), "./project_info/gdrive_project_info.csv")
