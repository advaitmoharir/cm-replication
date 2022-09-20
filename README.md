# cm-replication

This repository contains the data and code for the replication of [Are US Cities Underpoliced? Theory and Evidence](https://eml.berkeley.edu/~jmccrary/chalfin_mccrary2018.pdf). This project was undertaken with [Federico Crudu](https://sites.google.com/site/federicocrudu/).

We extend the Chalfin- McCrary dataset on police employment and crime (1960-2010), till 2019. The original paper (ReStat 2018), computes the police elasticity of crime for a defined set of violent and property crimes as defined by the FBI Return A using a panel of 242 cities.

The authors identify two sources of data for police employment, UCR and ASG. In this file, we extend the data for both these variables, by cleaning the raw data, and preparing it for further inference. First, we load the raw UCR data on crime and police employment (LEOKA) available from Dr. Jacob Kaplan's website for the period 1960-2019, and clean it from scratch to get the extended sample. Then, we extend the ASG data by obtaining the police employment files from the authors' till 2010, and extend it by cleaning the yearwise raw data from 2011-2019 from the Census Bureau website. The same procedure is followed to obtain citywise population data from 2011-2019. The original and extended (2011-2019) ASG files are merged using the Crosswalk data (this gives unit-level ids) published by the Census. The final output ('wide.csv') is the authors' original panel extended till 2019.

To obtain the cleaned extended dataset, follow these steps

1. Go to the folder `cleanup`.
2. Open `Cleanup.Rproj`
3. Within the project, open `Cleanup.R`
4. Run the file. The final csv output is already stored in the replication folder.

