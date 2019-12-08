# Software Engineering for Economists
# The Effect of Asset Purchases on Asset Prices and Inequality
# Thomas Schiller 


# Prepare working space ################## 
rm(list = ls())
gc()
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load packages
toload <- c("ggplot2", "ggfortify","forecast","tidyverse","stargazer", "lodown")

lapply(toload,require, character.only = T)


## Construct Household Balance Sheet for First and Fifth Quintiles (following Domanski et al 2016)

# Read in data
data <- lapply(paste0("Data/",list.files("Data")), function(x) read.csv(x)[,2])
data <- lapply(data, as.ts)

myfunction3<-function(name){
  variable<-deparse(substitute(name))
  assign(variable, read.csv2(file = paste0(substitute(name), ".csv"), sep = ","), env =.GlobalEnv)
}




