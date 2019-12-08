# Software Engineering for Economists
# The Effect of Asset Purchases on Asset Prices and Inequality
# Thomas Schiller 


## Prepare working space
rm(list = ls())
gc()
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load packages
toload <- c("ggplot2", "ggfortify","forecast","tidyverse","stargazer", "lodown", "readxl")

lapply(toload,require, character.only = T)


## Construct Household Balance Sheet for First and Fifth Quintiles (following Domanski et al 2016)

financialAsset<-read_excel("scf2016_tables_internal_nominal_historical.xlsx", sheet="Table 6 16", skip=88)
financialAssets<-financialAsset[-c(1:6,13:84),]
colnames(financialAssets)[1]<-"Percentile of income"

stockHolding<-read_excel("scf2016_tables_internal_nominal_historical.xlsx", sheet="Table 7")
stockHoldings<-stockHolding[-c(1:7,14:26),-c(1:20,22:31)]
colnames(stockHoldings)[1]<-"Percentile of income"

nonfinancialAsset<-read_excel("scf2016_tables_internal_nominal_historical.xlsx", sheet="Table 9 16", skip=2)
nonfinancialAssets<-nonfinancialAsset[-c(1:92, 99:170),]
colnames(nonfinancialAssets)[1]<-"Percentile of income"

headers <- read_excel("scf2016_tables_internal_nominal_historical.xlsx", sheet="Table 13 16 Alt", range="A90:K91", col_names = FALSE)
headersDebt <- sapply(headers,paste,collapse="_")
debt<-read_excel("scf2016_tables_internal_nominal_historical.xlsx", sheet="Table 13 16 Alt", range="A98:K103", col_names = FALSE)
names(debt)<-headersDebt


