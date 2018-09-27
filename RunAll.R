################################################################# Load Libraries
 library(rethinking)
 library(MASS)
 library(maps)
 library(maptools)
 library(ggplot2)
 library(grid)
 library(gridExtra)
 library(xtable)
 library(Cairo)
 
############################################################ Stan Control Params
mtd <- 13
ad <- 0.95
seed <- 12345
nchains <- 2
refresh <- 100
iter <- 4000
warmup <- iter/2
 
################################################################## Set directory
 setwd("C:/Users/cody_ross/Dropbox/Completed and Published Projects/1-papers/Embera Search/PLOS/Workflow")
 source("./Code/ProjectSupport.R")
 
################################################################### Run Analyses
source("./Code/MakeTable1.R")
source("./Code/MakeCalculations.R")
source("./Code/MakeMaps.R")  

source("./Code/RunStanMain.R")  
source("./Code/MakeCaterpillarPlots.R")

source("./Code/RunStanDist.R") 

source("./Code/MakeDensityPlots.R")

source("./Code/AR1RobustnessCheck.R")
source("./Code/AR1RobustnessCheckPlots.R")


source("./Code/EncounterTypeRobustnessCheck.R")
source("./Code/EncounterTypeRobustnessCheckPlots.R")

source("./Code/CheckTraceplots.R")
