###########################################
########### OSF Project Template ##########
###########################################

#####
# Date: TODAY'S DATE
# By: *INSERT NAME HERE*
# Description: PROJECT DISCRIPTION
# Version of R used: CURRENT VERSION OF R

####################################
########## load libraries ##########
####################################

## update packages then install these packages if not yet installed
if(!require(pacman)){install.packages("pacman")}
devtools::install_github("chartgerink/osfr")
pacman::p_load(osfr)

###############################
############# OSF #############
###############################

# login to OSF
login()

# verify login
welcome()

## create the GPL"s Project Template on your OSF within RStudio 

# get your research project id from "Researcher: XX" link on the GPL"s OSF
# your id is the weblink: osf.io/YOUR_ID

project <- create_component(id = "OSF_ID_HERE",               # put your research ID here
                            title = "PROJECT_NAME_HERE",
                            category = "project",
                            description = "A new project")

project_lit <- create_component(id = project,               
                                title = "Literature",
                                category = "other",
                                description = "Store your background papers here. A space to collect associated literature that relates to your project.")

project_mat <- create_component(id = project,               
                                title = "Materials and Methods",
                                category = "instrumentation",
                                description = "Any digital materials, such as questionnaires or ethics applications/approvals, related to the study")

project_doc <- create_component(id = project,               
                                title = "Notes and Documentation",
                                category = "other",
                                description = "A space to keep notes from each work session and documentation about the data and related materials produced.")

project_ana <- create_component(id = project,               
                                title = "Analysis Scripts",
                                category = "analysis",
                                description = "Store analysis scripts here")

project_dat <- create_component(id = project,               
                                title = "Data",
                                category = "data",
                                description = "Data that does not have identifying information and has been cleaned and ready for analysis.   Remember to make sure that all data and related materials connected to the OSF must not contain any personally identifiable information or personal health information.")

project_raw <- create_component(id = project_dat,               
                                title = "Raw Data",
                                category = "data",
                                description = "!!NEVER MAKE PUBLIC***Raw data, may contain identifying information**NEVER MAKE PUBLIC!!!")

project_out <- create_component(id = project,               
                                title = "Scholarly Output",
                                category = "communication",
                                description = "A space to keep materials that communicate to different audiences the process and the results of the experiment/project.")

project_reg <- create_component(id = project,               
                                title = "Registration",
                                category = "other",
                                description = "Project registration and disclosure documents")



## create list of the OSF project IDs for later reference

project_info <- list()
project_info$project     <- project
project_info$project_lit <- project_lit
project_info$project_mat <- project_mat
project_info$project_doc <- project_doc
project_info$project_ana <- project_ana 
project_info$project_dat <- project_dat
project_info$project_raw <- project_raw
project_info$project_out <- project_out
project_info$project_reg <- project_reg

# verify IDs were correctly saved
project_info

# save OSF IDs to you can use them later if necessary
if(!exists("./osf_project_info")) {dir.create("./osf_project_info")}    # this is where we will save our tables
rio::export(as.data.frame(project_info), "./osf_project_info/project_info.csv")

# don't forget to log out of the OSF within RStudio!
logout()
