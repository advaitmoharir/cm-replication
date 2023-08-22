
# ------------------------------------------------------------------------------
# Title: Master R script for CM Replication
# Authors: Advait Moharir
# Status: Complete
# Date: 28-03-2022
# ------------------------------------------------------------------------------

#Load required packages and state root directory

library(pacman)
p_load(stargazer,gmm,sandwich,lmtest,estimatr,
       jtools,broom.mixed,fixest,purrr, dplyr, 
       tidyverse, tidyr,plm,varhandle,
       collapse,ggplot2,kableExtra, knitr)# installs and loads reqd. packages.
#Set location

here::i_am("cm-replication.Rproj")

# Run all files

source("02_code/01_cleanup.R") # cleans raw data
source("02_code/02_replication.R") # narrow and wide dataset
source("02_code/02_replication_novel.R") # novel dataset
source("02_code/04_rep_newdata.R") # new data only