# ------------------------------------------------------------------------------
# Title: Code for narrow and long replication of Chalfin and McCrary (2018, REStat)
# Authors: Advait Moharir and Federico Crudu
# Status: Complete
# Date: 06-09-2022
# ------------------------------------------------------------------------------

#------------------- BRIEF DESCRIPTION -----------------------------------------
# The following code replicates Table-3 and Table-6 of
# Chalfin and McCrary (2018, REStat). We replicate the
# exact results (called 'narrow'), and also implement the
# empirical strategy for the extended dataset (called 'long').
# In addition, we also implement the optimal two sample IV
# procedure, as per Anderson and Moen (2016, OBES) for both datasets.
#-------------------------------------------------------------------------------

# Loading required libraries
library(pacman)# Package manager
p_load(stargazer,gmm,sandwich,lmtest,estimatr,
       jtools,broom.mixed,fixest,purrr)# installs and loads reqd. packages.

# The here::i_am command finds the associated folder and automatically
# sets working directory there. IMP: Only works when file is opened inside
# the RProj. Please ensure that the RProj is opened, followed by the file.
here::i_am("replication.R")

# Begin by reading in csv files for both datasets

narrow<-read.csv("narrow.csv")
wide<-read.csv("wide.csv")

#--------------------- REPLICATION ---------------------------------------------

# Since we are doing exactly the same set of regressions for
# both datasets, we write the operations inside a function
# called 'replicate'. A further function called 'comparison'
# produces p values for a (pairwise) t test of equality of 
# elasticities. Additional functions are required to compute
# the GMM estimators and the OptIV estimator. All the functions
# are bundled into the 'replicationfuns.R' file and are called
# via the command 

source("replicationfuns.R")


# Now that the functions are defined, we put both dataframes in a list
# and implement the function on both dfs, using the 'map' command.
# Final results are stored in a list called 'output' and 'output2' respectively.

data<-list(narrow, wide)
output<-data%>%map(replicate)
output2<-data%>%map(comparison)

#-------------------------- RESULT TABS ----------------------------------------
#Note: The Table names are as per our replication paper.

#---------- TABLE I: SUMMARY STATISTICS ----------------------------------------
stargazer(as.data.frame(narrow[,7:22]), omit.summary.stat = c("p25", "p75"))
stargazer(as.data.frame(long[,7:22]), omit.summary.stat = c("p25", "p75"))

#--------- TABLE II: REGRESSION RESULTS: NARROW DATASET ------------------------

etable(output[[1]]$ols.Z)
etable(output[[1]]$ols.S)
etable(output[[1]]$iv.forward)
etable(output[[1]]$iv.reflected)
output[[1]]$GMM
output[[1]]$opt.iv

#---- TABLE III: CROSS-CRIME ELASTICITIES' EQUALITY TEST: NARROW DATASET -------

output2[[1]]

#--------- TABLE IV: REGRESSION RESULTS: LONG DATASET --------------------------

etable(output[[2]]$ols.Z)
etable(output[[2]]$ols.S)
etable(output[[2]]$iv.forward)
etable(output[[2]]$iv.reflected)
output[[2]]$GMM
output[[2]]$opt.iv

#---- TABLE V: CROSS-CRIME ELASTICITIES' EQUALITY TEST: LONG DATASET -----------

output2[[2]]

#-------------------------- END ------------------------------------------------
