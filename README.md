# Replication of Chalfin and McCrary (2018)

This repository contains the data and code for the replication of [Are US Cities Underpoliced? Theory and Evidence](https://eml.berkeley.edu/~jmccrary/chalfin_mccrary2018.pdf). This project was undertaken with [Federico Crudu](https://sites.google.com/site/federicocrudu/).

We extend the Chalfin- McCrary dataset on police employment and crime (1960-2010), till 2019. The original paper (ReStat 2018), computes the police elasticity of crime for a defined set of violent and property crimes as defined by the FBI Return A using a panel of 242 cities.

The authors identify two sources of data for police employment, UCR and ASG. We first extend the data for both these variables, by cleaning the raw data, and preparing it for further inference. Then we replicate the key results of Chalfin and McCrary (2018) for the original and well as the extended dataset.

## Code

The folder `02_code` consists all the data and scripts required to clean and replicate the original results, consisting of the following files:

- `00_master.R` -  Master R script
- `01_cleanup.R`- cleans original and extended dataset (1960-2010).
- `02_replication.R`- replicates Table 3 and 6 of CM 2018 with narrow and wide dataset.
- `02_replication_novel.R`- replicates Table 3 and 6 of CM 2018 with novel dataset.
- `03_replicationfuns.R`- R script hosting all functions required to replicate the results.
- ` 04_rep_newdata.R`- replicates Table 3 and 6 of CM 2018 with new data (2011-2019) as a robustness check.

## Replication
To implement the replication, follow these steps

1. Open `cm-replication.Rproj`.
3. Within the project, open `00_master.R`.
4. Run the file.

The R script for running raw data (`01_cleanup.R`) has been commented outin the master script as the raw files are too big for Github. However, those wishing to replicate the cleanup can contact me via email, and the raw files can be shared as required.

## Software

The raw data was opened using Microsoft Excel 16, and Stata 16. The replication was conducted in R using R Studio (Version 1.4.1106).
