# ------------------------------------------------------------------------------
# Title: Code for replicating CM-2018 for subsample (2011-2019)
# Authors: Advait Moharir
# Status: Complete
# Date: 12-06-2023
# ------------------------------------------------------------------------------

wide_new<-read.csv("04_output/wide.csv")%>%
  select(-c(1))%>%
  filter(year>2010) # filters out data from 2011-2019

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

data<-list(wide_new)
output<-data%>%map(replicate)



#---------  REGRESSION RESULTS: NEW DATA ------------------------

etable(output[[1]]$ols.Z)
etable(output[[1]]$ols.S)
etable(output[[1]]$iv.forward)
etable(output[[1]]$iv.reflected)
output[[1]]$GMM
output[[1]]$opt.iv
