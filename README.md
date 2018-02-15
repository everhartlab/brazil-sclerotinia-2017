
Click Here to Launch an online session: [![Binder](http://mybinder.org/badge.svg)](http://beta.mybinder.org/v2/gh/everhartlab/brazil-sclerotinia-2017/master)

Purpose
=======

Scripts for Population Genetic Diversity of Sclerotinia sclerotiorum, Causal Agent of White Mold Disease of Dry Bean, and Implications for Fungicide Resistance / Disease Management.

This directory contains the scripts, written by Anthony Pannullo, to generate the figures and tables used in this manuscript. These scripts were modified by Zhian Kamvar in the fall of 2017 to ensure reproducibility of the results.  

R Project File
==============

On 2017-08-24, Zhian Kamvar has created the Rproject file as an anchor for this project and removed all instances of specific file paths with invocations with the package "here".

DO NOT OPEN THIS Rproj FILE FROM THE BOX FOLDER. Instead, create a new folder where you want to work (e.g. a new folder in your documents called "Thesis Project") and copy all the files within the Box folder "Scripts" there. Once you have copied these files, you can open the RStudio project and select "Build > Build All" from the dropdown menu. This will update your packages and compile all the figures and tables. 

Details: Building the results
-----------------------------

All of the results are controlled via the Makefile. A diagram of how the makefile proceeds can be found in workflow.pdf:

 1. install.R is run, which installs/updates the packages needed for these analyses. This creates bootstrap.txt and the folders "figs", "results", and "tables"
 2. All the R scripts are run, creating their results/*.Rout files and the figures and tables needed for the manuscript.
 3. rsync will copy all of the results (ignoring .git/ and .Rproj.user/) to the box drive (assuming OSX)
 
 
Data
=====

There are two data sets. In both data sets missing data are represented with 
`NA` values. 

### [data.csv]

This data set contains genotypes and location names with the following columns:

|Column                       |                                     Value |
|:----------------------------|------------------------------------------:|
|Sample                       | Sample name                               |
|Continent_Country_Population | population factor separated by underscores|
|5-2                          | locus  1                                  |
|6-2                          | locus  2                                  |
|7-2                          | locus  3                                  |
|8-3                          | locus  4                                  |
|9-2                          | locus  5                                  |
|12-2                         | locus  6                                  |
|20-3                         | locus  7                                  |
|55-4                         | locus  8                                  |
|110-4                        | locus  9                                  |
|114-4                        | locus 10                                  |
|17-3                         | locus 11                                  |


### [MasterGenoMCGDataBrazilPaper2018.xlsx]

Metadata of information from the JR Steadman lab database. These data were 
compiled by SEE on 2018-02-14 and checked by ZNK. 

|Column                          |                                               Value|
|:-------------------------------|---------------------------------------------------:|
|MCG                             | Mycelial Compatibility Grouping as recorded by TJM |
|AP-GenoID                       | Isolate ID used by AP                              |
|InventoryID                     | ID used in JRS lab collection                      |
|in-JRS-collection               | Logical indicator for presence of isolate in JRS collection|
|AP-Continent_Country_Population | population factor used by AP (encoding canged)     |
|JRS-Isolate #                   | Identical to InventoryID, was linked to JRS database for confirmation |
|JRS-Collection Date             | Collection year or date in D/M/Y format            |
|JRS-Source (Host)               | Host Name                                          |
|JRS-Geographical Location       | Location in various formats. Usually City, State. For BR isolates: City/State, Country |
|JRS-Notes                       | Notes from the JRS data base                       |


The identifiers [data.csv] were based on the original isolate names from Brazil.
However, because of name clashes with the JR Steadman lab database, these names 
were changed as soon as the isolates were entered into the database. These two
files are harmonized in [01-CleanData.R] before being used in any other script
to ensure consistent use of region names. 
 
[data.csv]: data/data.csv
[MasterGenoMCGDataBrazilPaper2018.xlsx]: data/MasterGenoMCGDataBrazilPaper2018.xlsx
[01-CleanData.R]: 01-CleanData.R