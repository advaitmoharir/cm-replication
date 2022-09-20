# Replication of Chalfin and McCrary (2018)

This repository contains the data and code for the replication of [Are US Cities Underpoliced? Theory and Evidence](https://eml.berkeley.edu/~jmccrary/chalfin_mccrary2018.pdf). This project was undertaken with [Federico Crudu](https://sites.google.com/site/federicocrudu/).

We extend the Chalfin- McCrary dataset on police employment and crime (1960-2010), till 2019. The original paper (ReStat 2018), computes the police elasticity of crime for a defined set of violent and property crimes as defined by the FBI Return A using a panel of 242 cities.

The authors identify two sources of data for police employment, UCR and ASG. In this file, we first extend the data for both these variables, by cleaning the raw data, and preparing it for further inference.

## Code

The folder `code` consists all the data and scripts required to clean and replicate the original results. There are two main subfolders:

- `cleanup` - This has the raw data and code, which cleans and extends the data on crime and police employment
- `replication`- This has the cleaned dataset and code, which replicates the results

To obtain the cleaned extended dataset, follow these steps

1. Go to the folder `cleanup`.
2. Open `Cleanup.Rproj`
3. Within the project, open `Cleanup.R`
4. Run the file. The final csv output is already stored in the replication folder.

To implement the replication, follow these steps

1. Go to the folder `replication`.
2. Open `replication.Rproj`.
3. Within the project, open `replication.R`.
4. Run the file.

