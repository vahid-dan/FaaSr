#!/usr/local/bin/Rscript
#This Rscript will be invoked when the action starts
#This will be replaced by library("faasr")
source("faasr.R")
#Get all functions from Rscripts in the /action/ directory
library("jsonlite")
library("devtools")

faasr <- commandArgs(TRUE)
faasr_source <- fromJSON(faasr)


#1st idea - use "source_url", a function from devtools library.
#"raw.githubusercontent.com" 
url_list<-faasr_source$FunctionGitRepo[[faasr_source$FunctionInvoke]]
for (file in url_list){
	source_url(file)
	}

#2nd idea - use "system"
#download the file from github by using "git"-required to download git as a dependency.


#3nd idea - no change, hardly wired R files in the Dockerfile.
#r_files <- list.files(pattern="\\.R$")
#for (file in r_files){
#	source(file)
#}


#Start Ibmcloud action with faasr_start
faasr_start(faasr)

