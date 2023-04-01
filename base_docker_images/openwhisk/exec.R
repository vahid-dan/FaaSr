#!/usr/local/bin/Rscript
#This Rscript will be invoked when the action starts
#This will be replaced by library("faasr")
source("faasr.R")
#Get all functions from Rscripts in the /action/ directory
r_files <- list.files(pattern="\\.R$")
for (file in r_files){
	source(file)
}
#Start Ibmcloud action with faasr_start
faasr <- commandArgs(TRUE)
faasr_start(faasr)

