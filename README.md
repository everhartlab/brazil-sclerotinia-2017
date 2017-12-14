
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

 1. packages.R is run, which installs/updates the packages needed for these analyses. This creates bootstrap.txt and the folders "figs", "results", and "tables"
 2. All the R scripts are run, creating their results/*.Rout files and the figures and tables needed for the manuscript.
 3. rsync will copy all of the results (ignoring .git/ and .Rproj.user/) to the box drive (assuming OSX)