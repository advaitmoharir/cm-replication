# ------------------------------------------------------------------------------
# Title: Code for narrow and long replication of Chalfin and McCrary (2018, REStat)
# Authors: Advait Moharir 
# Status: Complete
# Date: 09-05-2023
# ------------------------------------------------------------------------------

#------------------- BRIEF DESCRIPTION -----------------------------------------
# The following code replicates Table-3 and Table-6 of
# Chalfin and McCrary (2018, REStat). This
# file does the same replication for the "novel" dataset
# In addition, we also implement the optimal two sample IV
# procedure, as per Anderson and Moen (2016, OBES) for both datasets.
#-------------------------------------------------------------------------------


# Begin by reading in csv files for novel dataset

novel<-read.csv("04_output/novel.csv")


#--------------------- REPLICATION ---------------------------------------------

# Since we are doing exactly the same set of regressions for
# both datasets, we write the operations inside a function
# called 'replicate'. A further function called 'comparison'
# produces p values for a (pairwise) t test of equality of 
# elasticities. Additional functions are required to compute
# the GMM estimators and the OptIV estimator. All the functions
# are bundled into the 'replicationfuns.R' file and are called
# via the command 

source("02_code/03_replicationfuns.R")


# Now that the functions are defined, we put both dataframes in a list
# and implement the function on both dfs, using the 'map' command.
# Final results are stored in a list called 'output' and 'output2' respectively.

data<-list(novel)
output<-data%>%map(replicate)
output2<-data%>%map(comparison)

#-------------------------- RESULT TABS ----------------------------------------
#Note: The Table names are as per our replication paper.



#--------- TABLE II: REGRESSION RESULTS: NOVEL DATASET ------------------------

etable(output[[1]]$ols.Z, digits = 3)
etable(output[[1]]$ols.S, digits = 3)
etable(output[[1]]$iv.forward, digits = 3)
etable(output[[1]]$iv.reflected, digits = 3)
output[[1]]$GMM
output[[1]]$opt.iv


#---- TABLE III: CROSS-CRIME ELASTICITIES' EQUALITY TEST: NOVEL DATASET -------

output2[[1]]

