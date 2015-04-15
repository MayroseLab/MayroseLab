library(phytools)
library(diversitree)
library(geiger)
require(R.utils)

setwd("~/Niv/dioecy/mapping")
source("functions.R")
source("fitPagel.const.R")


##################################################################
# examine the effect of SIM.PCT.DIFF
genera = c("Lycium")

main_dir="Lycium1"
prep_mapping(main_dir,genera,NTREES.1 = 100, NTREES.2 = 1, SIM.PCT.DIFF = 1)
main_dir="Lycium5"
prep_mapping(main_dir,genera,NTREES.1 = 100, NTREES.2 = 1, SIM.PCT.DIFF = 5)
main_dir="Lycium15"
prep_mapping(main_dir,genera,NTREES.1 = 100, NTREES.2 = 1, SIM.PCT.DIFF = 15)
main_dir="Lycium30"
prep_mapping(main_dir,genera,NTREES.1 = 100, NTREES.2 = 1, SIM.PCT.DIFF = 30)
main_dir="Lycium_unconst"
prep_mapping(main_dir,genera,NTREES.1 = 100, NTREES.2 = 1, SIM.PCT.DIFF = 100)
# examine the results
get_res("res.lycium", NTREES.1 = 100, NTREES.2 = 1)

##################################################################
# main run
main_dir="runs.1"
genera = dir("traits")
prep_mapping(main_dir,genera,NTREES.1 = 100, NTREES.2 = 10, SIM.PCT.DIFF = 5)

##################################################################
# test when herm is constrained at the root
main_dir="runs.2"
genera = dir("traits")
prep_mapping(main_dir,genera,NTREES.1 = 100, NTREES.2 = 10, SIM.PCT.DIFF = 5,run_file = "map_genus_constH.R")

##################################################################
# try with SIM.PCT.DIFF = 1
main_dir="runs.3"
genera = dir("traits")
prep_mapping(main_dir,genera,NTREES.1 = 100, NTREES.2 = 10, SIM.PCT.DIFF = 1)

##################################################################
# test when herm is constrained at the root and SIM.PCT.DIFF = 1
main_dir="runs.4"
genera = dir("traits")
prep_mapping(main_dir,genera,NTREES.1 = 100, NTREES.2 = 10, SIM.PCT.DIFF = 1,run_file = "map_genus_constH.R")

##################################################################
# add threshBayes
main_dir="runs.new"
genera = dir("traits")
prep_mapping(main_dir,genera,NTREES.1 = 100, NTREES.2 = 10, SIM.PCT.DIFF = 5)




# examine the results

get_res("res.1", NTREES.1 = 100, NTREES.2 = 10)
get_res("res.2", NTREES.1 = 100, NTREES.2 = 10)
get_res("res.3", NTREES.1 = 100, NTREES.2 = 10)
get_res("res.4", NTREES.1 = 100, NTREES.2 = 10)

get_res("res.new", NTREES.1 = 100, NTREES.2 = 10)




