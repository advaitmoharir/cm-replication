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


# Begin by reading in csv files for both datasets

narrow<-read.csv("03_raw/narrow.csv")
wide<-read.csv("04_output/wide.csv")%>%select(-c(1))

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

data<-list(narrow,wide)
output<-data%>%map(replicate)
output2<-data%>%map(comparison)

#-------------------------- RESULT TABS ----------------------------------------
#Note: The Table names are as per our replication paper.

#---------- TABLE I: SUMMARY STATISTICS ----------------------------------------



# Narrow

stargazer(as.data.frame(narrow[,8:22]),
          omit.summary.stat = c("p25", "p75"))
# Wide

stargazer(as.data.frame(wide[,5:21]), 
          omit.summary.stat = c("p25", "p75"))
 
# New data only (for Appendix)

new<-wide%>%filter(year>2010)

stargazer(as.data.frame(new[,5:21]), 
          omit.summary.stat = c("p25", "p75"))



#--------- TABLE II: REGRESSION RESULTS: NARROW DATASET ------------------------

etable(output[[1]]$ols.Z)
etable(output[[1]]$ols.S)
etable(output[[1]]$iv.forward)
etable(output[[1]]$iv.reflected)
output[[1]]$GMM
output[[1]]$opt.iv


#---- TABLE III: CROSS-CRIME ELASTICITIES' EQUALITY TEST: NARROW DATASET -------

output2[[1]]


## Output as tex table for Appendix

kable(output2[[1]],format="latex",booktabs=T, col.names=c("Crime 1", "Crime 2",
                                                          "p-value", "p-value (HC1)"))%>%
  save_kable("05_latex/Writeup/tables/tab6_hc1_narrow.tex")


#--------- TABLE IV: REGRESSION RESULTS: LONG DATASET --------------------------

etable(output[[2]]$ols.Z, digits=3)
etable(output[[2]]$ols.S,digits=3)
etable(output[[2]]$iv.forward,digits=3)
etable(output[[2]]$iv.reflected,digits=3)
output[[2]]$GMM
output[[2]]$opt.iv

#---- TABLE V: CROSS-CRIME ELASTICITIES' EQUALITY TEST: LONG DATASET -----------

output2[[2]]

## Output as tex table for Appendix

kable(output2[[2]],format="latex",booktabs=T, col.names=c("Crime 1", "Crime 2",
                                             "p-value", "p-value (HC1)" ))%>%
  save_kable("05_latex/Writeup/tables/tab6_hc1_extended.tex")

#-------------------------- END ------------------------------------------------

