# Replication of Chalfin and McCrary (2018)

This repository contains the data and code for the replication of [Are US Cities Underpoliced? Theory and Evidence](https://eml.berkeley.edu/~jmccrary/chalfin_mccrary2018.pdf). This project was undertaken with [Federico Crudu](https://sites.google.com/site/federicocrudu/).

We extend the Chalfin- McCrary dataset on police employment and crime (1960-2010), till 2019. The original paper (ReStat 2018), computes the police elasticity of crime for a defined set of violent and property crimes as defined by the FBI Return A using a panel of 242 cities.

The authors identify two sources of data for police employment, UCR and ASG. We first extend the data for both these variables, by cleaning the raw data, and preparing it for further inference. Then we replicate the key results of Chalfin and McCrary (2018) for the original and well as the extended dataset.

## Code

The folder `replication` consists all the data and scripts required to clean and replicate the original results, consisting of the following files:

- `narrow.csv`- cleaned original dataset (1960-2010).
- `wide.csv`- cleaned extended dataset (1960-2019).
- `replication.Rproj`- R Project associated with this folder.
- `replicationfuns.R`- R script hosting all functions required to replicate the results.
` replication.R`- R script implementing the replication.

To implement the replication, follow these steps

1. Go to the folder `replication`.
2. Open `replication.Rproj`.
3. Within the project, open `replication.R`.
4. Run the file.

This replicates the Table-2, Table-3 and Table-6 of Chalfin and McCrary (2018).
## Software

The raw data was opened using Microsoft Excel 16, and Stata 16. The replication was conducted in R using R Studio (Version 1.4.1106).
