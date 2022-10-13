#  UK Health Index 2015-2018

This repository contains data and code to compute the original Office for National Statistics  Health Index for the Upper Tier Local Authority and Regions over the 2015-2018.  

## Description

This folder contains data and code to accompany the paper on ['Assessments and developments in constructing a National Health Index for policy making, in the United Kingdom'](https://arxiv.org/abs/2210.05154). 

We have included R  scripts  to compute the Beta (experimental) Health Index and the modified version presented in the paper, where we also conducted an assessment on statistical coherence and sensitivity  & uncertainty analysis.

##  Repository Structure
```
Health Index/
├── data/
│   ├── England_all_geog_aggregated_2015.csv 
│   ├── England_all_geog_aggregated_2016.csv 
│   ├── England_all_geog_aggregated_2017.csv 
│   └── England_all_geog_aggregated_2018.csv 
|
├── input_files/
│   ├── Allvar.csv  # indicators and strucuture labels
|   ├── Dico.csv  # data dictionary
|   ├── Domains_Subdomains.csv # nested structure
|   ├── mymetadata.csv  # indicators descriptions
|   ├── ParentsLavels.csv # nice lables for plots
|   ├── sdblab.csv   # lables for indicators codes  
|   ├── MymodifiedFoo.R # contains modified R functions
|   ├── ONS_NORM.Rdata # normailized indicators data for comparison plot 
|   ├── subdomains2domains.csv  # nested strucutres
|   ├── variables_reverse.json # list of data to reverse
|   ├── variable_transformations.json # list of data transformation
│   └── index_structure.json # Index strucutre
|
├── lookup/
|    └── geog_pop_lookup.csv 
|   
├── 1.CommonOrig.R  # generates: Df_indicators.Rdata, OutputsOrig.Rdata, FA_Exp_Weights_ONS.Rdata
├── 2.Plots_HI.R
├── 3.DataPreparation_COIN.R  # generates:HIBO.Rdata 
├── 4.Correlation_Analysis.R 
├── 5.Sensitivity_First_Total.R 
├── 6.SA_UA_Index2HPC.R # Needs to Run on HPC 
└── 7.Sensitivity_Uncertainty2Plot.R 

```

## Getting Started

### Dependencies

* The code runs on R 4.2.1 
* These files run on the previous version of the  COINr package,  named COINr6, all details can be found [here](https://rdrr.io/cran/COINr/f/README.md).

### Executing program

* Run the files in the numerical order.

## Folder Description 

The sub-folders are:

* data: contains the raw csv files for 2015-2018
* input_files: contains auxiliary files as CSV with the health index labels and structures
* lookups: contains geographical information 


## R scripts Description 


* 1.CommonOrig.R: it reads data in, and the Beta Health Index is replicated as per 2015-2018. The code generates several outputs that are called in the following codes. 
* 2.Plots_HI.R: Plots related to the ONS original Health Index.
* 3.DataPrepatation_COIN.R: here, we use the library COINr (version: COINr6) to compute the modified version for the Health Index; the main difference with the original is the minimal data transformation, mainly winsorization and logarithmic function. 
* 4.Correlation_Analysis.R: correlation analysis is carried out, at the different hierarchical levels and other comparisons.  
* 5.Sensitivity_First_Total.R: we computed the sensitivity Index, i.e. correlation ratio 
* 6.SA_UA_Index2HPC.R: This file runs the MonteCarlo Sensitivity and Uncertainty analysis. We recommend running on a High-Performance-Computing Machine, as it is incredibly time-consuming. 
* 7.Sensitivity_Uncertainy2Plot.R: In this file, we read the output from the MonteCarlo analysis generated in the previous file and create  the plots for the rank distributions. 

## Authors


[Anna Freni Sterrantino](mailto:afrenisterrantino@turing.ac.uk).


## License

This work is licensed under the MIT license (code) and Creative Commons Attribution 4.0 International license (for documentation). You are free to share and adapt the material for any purpose, even commercially, as long as you provide attribution (give appropriate credit, provide a link to the license, and indicate if changes were made) in any reasonable manner, but not in any way that suggests the licensor endorses you or your use, and with no additional restrictions.
