##################  prepare working space ################## 
rm(list = ls())
gc()
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#load packages
toload <- c("ggplot2", "ggfortify","forecast","magrittr","stargazer")

lapply(toload,require, character.only = T)

#read in data
data <- lapply(paste0("Data/",list.files("Data")), function(x) read.csv(x)[,2])
data <- lapply(data, as.ts)