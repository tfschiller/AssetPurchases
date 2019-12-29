# Inequality 
# Software Engineering for Economists
# The Effect of Asset Purchases on Asset Prices and Inequality
# Thomas Schiller 

## Prepare working space
rm(list = ls())
gc()

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load packages
toload <- c("ggplot2", "ggfortify","forecast","tidyverse","stargazer", "lodown", "readxl", "dplyr", "astsa", "vars", "urca", "lubridate", "rlist", "zoo", "xts")

lapply(toload,require, character.only = T)


## Construct Household Balance Sheet for First and Fifth Quintiles 

# Financial Assets
financialAsset<-read_excel("scf2016_tables_internal_nominal_historical.xlsx", sheet="Table 6 16 means")
financialAssets<-financialAsset[10:15,]
colnames(financialAssets)[1]<-"Percentile of income"
colnames(financialAssets)[2:12]<-read_excel("scf2016_tables_internal_nominal_historical.xlsx", sheet="Table 6 16 means", range = "B3:L3", col_names = FALSE)

# Non Financial Assets 
nonfinancialAsset<-read_excel("scf2016_tables_internal_nominal_historical.xlsx", sheet="Table 9 16 means")
nonfinancialAssets<-nonfinancialAsset[10:15,]
colnames(nonfinancialAssets)[1]<-"Percentile of income"
colnames(nonfinancialAssets)[2:9]<-read_excel("scf2016_tables_internal_nominal_historical.xlsx", sheet="Table 9 16 means", range = "B3:I3", col_names = FALSE)


# Total Assets
householdPortfolio<-Reduce(function(x, y) left_join(x, y, by="Percentile of income"), list(financialAssets,nonfinancialAssets))
colnames(householdPortfolio)[c(11,18)]<-c("Other financial assets", "Other nonfinancial assets")
householdPortfolio[,2:20]<-as.numeric(unlist(householdPortfolio[,2:20]))








row.names(householdPortfolio)<-householdPortfolio[,1]
householdPortfolio[is.na(householdPortfolio)]<-0


# Calculate percent of household portfolio for each asset type by income distrbution
householdassets <- as.data.frame(lapply(householdPortfolio[,-1], function(x) {
  x / apply(householdPortfolio[,-1],1,sum)}))*100

rownames(householdassets)<-unlist(unname(householdPortfolio[,1]))
colnames(householdassets)<-colnames(householdPortfolio[2:20])  
  

# Debt 
headers <- read_excel("scf2016_tables_internal_nominal_historical.xlsx", sheet="Table 13 16 Alt means", range="B3:K4", col_names = FALSE)
headersDebt <- sapply(headers,paste,collapse="_")
debt<-read_excel("scf2016_tables_internal_nominal_historical.xlsx", sheet="Table 13 16 Alt means", range="A12:K17", col_names = FALSE)
names(debt)<-headersDebt
colnames(debt)[1]<-"Percentile of income"








colnames(householdPortfolio)[c(11,19,23,24,25,26,28,29,30,31)]<-c("Other financial assets", "Other nonfinancial assets", 
                                                                  "HELOC - Secured by primary residence", "Other residential debt", 
                                                                  "Credit card balances", "Lines of credit not secured by residential property",
                                                                  "Vehicle loans", "Other installment loans", "Other debt", "Any debt")
householdPortfolio[1:3,5]<-0
householdPortfolio[,2:31]<-as.numeric(unlist(householdPortfolio[,2:31]))


# Calculate relative weights of each asset class for the different income quintiles (as percentage of total asset holdings)

pcts <- as.data.frame(lapply(householdPortfolio[,-1], function(x) {
  x / apply(householdPortfolio[,-1],1,sum)
}))*100


# Combine the fifth and sixth rows as these represent the eighth and ninth deciles 
pcts1<-pcts[-6,]
pcts1[5,]<-pcts[5,]+pcts[6,]


# Keep only the asset class a la Domanski et al 2016






