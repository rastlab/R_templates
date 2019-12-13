###########################################
########### OSF Project Template ##########
###########################################


####################################
########## load libraries ##########
####################################

## Install the required script packages if not yet installed

# Install pacman & devtools packages if necessary
if(!"pacman" %in% rownames(installed.packages())) install.packages("pacman")
if(!"devtools" %in% rownames(installed.packages())) install.packages("devtools")

remotes::install_github("centerforopenscience/osfr")
pacman::p_load(osfr)

###############################
############# OSF #############
###############################

# login to OSF
osf_auth(token = 'INSERT_OSF_PAT_HERE')   # to get a PAT, follow instructions here: http://centerforopenscience.github.io/osfr/articles/auth.html



## create the GPL"s Project Template on your OSF within RStudio 

# get your research project id from "Researcher: XX" link on the GPL"s OSF
# your id is the weblink: osf.io/YOUR_ID

new_project <- osf_retrieve_node("INSERT_YOUR_RESEACHER_ID_HERE")  ## will be a random string and will look like 'vha2d'

## everything below this will run and place the OSF project/component into your OSF based on the GPL's OSF template

## only modify the lines instructing you to modify them!

project <- osf_create_component(x = new_project,                
                                title = "RStudio_Test",                    # give appropriate project title
                                description = "A new project",             # give brief description to help others identify project
                                category = "project",
                                public = FALSE)

project_lit <- osf_create_component(x = project,               
                                    title = "Literature",
                                    category = "other",
                                    description = "Store your background papers here. A space to collect associated literature that relates to your project.",
                                    public = FALSE)

project_mat <- osf_create_component(x = project,              
                                    title = "Materials and Methods",
                                    category = "instrumentation",
                                    description = "Any digital materials, such as questionnaires or ethics applications/approvals, related to the study",
                                    public = FALSE)

project_doc <- osf_create_component(x = project,              
                                    title = "Notes and Documentation",
                                    category = "other",
                                    description = "A space to keep notes from each work session and documentation about the data and related materials produced.",
                                    public = FALSE)

project_ana <- osf_create_component(x = project,             
                                    title = "Analysis Scripts",
                                    category = "analysis",
                                    description = "Store analysis scripts here",
                                    public = FALSE)

project_dat <- osf_create_component(x = project,              
                                    title = "Data",
                                    category = "data",
                                    description = "Data that does not have identifying information and has been cleaned and ready for analysis. Remember to make sure that all data and related materials connected to the OSF must not contain any personally identifiable information or personal health information.",
                                    public = FALSE)

project_raw <- osf_create_component(x = project_dat,               
                                    title = "Raw Data",
                                    category = "data",
                                    description = "!!NEVER MAKE PUBLIC***Raw data, may contain identifying information**NEVER MAKE PUBLIC!!!",
                                    public = FALSE)

project_out <- osf_create_component(x = project,              
                                    title = "Scholarly Output",
                                    category = "communication",
                                    description = "A space to keep materials that communicate to different audiences the process and the results of the experiment/project.",
                                    public = FALSE)

project_reg <- osf_create_component(x = project,               
                                    title = "Registration",
                                    category = "other",
                                    description = "Project registration and disclosure documents",
                                    public = FALSE)



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

# copy your raw data from RStudio Server to OSF as well
# goal is to keep our analysis scripts completely reproducible
if(!exists("./data")) {dir.create("./data")}    
if(!exists("./data/raw_data")) {dir.create("./data/raw_data")}    