# List and installation script for all packages used in Rastlab templates

if(!"pacman" %in% rownames(installed.packages())) install.packages("pacman")


pacman::p_load(googledrive, rio, tidyverse, parallel, psych, sjmisc, 
               lubridate, janitor, car, corrr, jmv, nFactors, parameters, 
               Superpower, lsr, phia, apaTables, patchwork, pwr2ppl, pequod, 
               jtools, interactions, stargazer, performance, effectsize, 
               ggstance, lavaan, sjstats, poorman, tinytex, osfr, gitcreds)
