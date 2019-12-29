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

# Combine the fifth and sixth rows as these represent the eighth and ninth deciles 
householdPortfolio[5,2:20]<-householdPortfolio[5,2:20]+householdPortfolio[6,2:20]
householdPortfolio<-householdPortfolio[-6,]
householdPortfolio[c(1,5),1]<-c("0-19.9", "80-100")


row.names(householdPortfolio)<-unlist((householdPortfolio[,1]))
householdPortfolio[is.na(householdPortfolio)]<-0


# Calculate percent of household portfolio for each asset type by income distrbution
householdassets <- as.data.frame(lapply(householdPortfolio[,-1], function(x) {
  x / apply(householdPortfolio[,-1],1,sum)}))*100

rownames(householdassets)<-unlist(unname(householdPortfolio[,1]))
colnames(householdassets)<-colnames(householdPortfolio[2:20])  
  

# Debt 
debt<-read_excel("scf2016_tables_internal_nominal_historical.xlsx", sheet="Table 13 16 Alt means", range="A12:K17", col_names = FALSE)
colnames(debt)[2:length(debt)]<-c("Mortgages - Secured by primary residence", "HELOC - Secured by primary residence", "Other residential debt", 
                                                                  "Credit card balances", "Lines of credit not secured by residential property", "Education loans",
                                                                  "Vehicle loans", "Other installment loans", "Other debt", "Any debt")
rownames(debt)<-unlist(debt[,1])


# Combine the fifth and sixth rows as these represent the eighth and ninth deciles 
debt[5,2:11]<-debt[5,2:11]+debt[6,2:11]
debt<-debt[-6,]
debt[c(1,5),1]<-c("0-19.9", "80-100")

# Calculate percent of household portfolio for each asset type by income distrbution
householddebt <- as.data.frame(lapply(debt[,-1], function(x) {
  x / apply(debt[,-1],1,sum)}))*100

rownames(householddebt)<-unlist(debt[,1])
colnames(householddebt)<-colnames(debt[2:11])


# Keep only the asset class a la Domanski et al 2016






